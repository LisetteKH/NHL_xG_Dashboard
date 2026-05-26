# 01_data_pipeline.R
# Pulls shot-level play-by-play data from the NHL API
# and engineers all features needed for the xG model and dashboard
#
# USAGE:
#   source("R/00_api_client.R")
#   source("R/01_data_pipeline.R")
#
#   seasons <- c("20192020","20202021","20212022","20222023","20232024","20242025","20252026")
#   pull_multiple_seasons(seasons)
#
#   all_shots <- purrr::map_dfr(seasons, function(s) readRDS(glue("data/shots_{s}.rds")))
#   save_shots(all_shots, "data/shots_all.rds")
#
#   players <- pull_player_metadata(unique(c(all_shots$shooter_id, all_shots$goalie_id)))
#   saveRDS(players, "data/player_metadata.rds")

library(dplyr)
library(purrr)
library(glue)

source("00_api_client.R")

# ── Null coalescing operator ──────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[[1]])) a else b

# =============================================================================
# 1. FETCH PLAY-BY-PLAY FOR A SINGLE GAME
# =============================================================================

get_game_pbp <- function(game_id) {
  url    <- glue("{NHL_API}/gamecenter/{game_id}/play-by-play")
  result <- nhl_get(url)
  return(result)
}

# =============================================================================
# 2. PARSE SHOTS FROM RAW PLAY-BY-PLAY
# =============================================================================
# Extracts all shot events and captures every feature we need for:
#   - xG model (distance, angle, shot type, rebound, strength state)
#   - Power play analysis (situation_code, strength_state)
#   - Goalie dashboard (goalie_id, empty_net)
#   - Player analysis (shooter_id, game_date)
#   - Team analysis (shooting_team, defending_team)

parse_shots <- function(game) {
  shot_types <- c("shot-on-goal", "goal", "missed-shot")
  
  # Game-level metadata
  home_id   <- game$homeTeam$id
  away_id   <- game$awayTeam$id
  home_team <- game$homeTeam$abbrev
  away_team <- game$awayTeam$abbrev
  game_date <- game$gameDate %||% NA
  
  purrr::keep(game$plays, function(p) {
    p$typeDescKey %in% shot_types
  }) |>
    purrr::map_dfr(function(p) {
      
      owner <- p$details$eventOwnerTeamId %||% NA
      
      # ── Situation code parsing ──────────────────────────────────────────────
      # 4-digit code: away_goalie | away_skater | home_skaters | home_goalie
      # e.g. "1551" = 5v5 even strength
      #      "1541" = home team on powerplay (home has 5, away has 4)
      #      "1460" = away team on powerplay, home goalie pulled
      sit_code <- p$situationCode %||% "1551"
      away_g   <- as.integer(substr(as.character(sit_code), 1, 1))
      away_sk  <- as.integer(substr(as.character(sit_code), 2, 2))
      home_sk  <- as.integer(substr(as.character(sit_code), 3, 3))
      home_g   <- as.integer(substr(as.character(sit_code), 4, 4))
      
      is_home_shooter <- !is.na(owner) && owner == home_id
      
      # ── Strength state ──────────────────────────────────────────────────────
      strength_state <- if (is.na(away_sk) || is.na(home_sk)) {
        "5v5"
      } else if (away_sk == home_sk) {
        "5v5"
      } else if (is_home_shooter) {
        if (home_sk > away_sk) "Powerplay" else "Shorthanded"
      } else {
        if (away_sk > home_sk) "Powerplay" else "Shorthanded"
      }
      
      # ── Empty net ───────────────────────────────────────────────────────────
      # Shooting at empty net = opposing goalie has been pulled
      empty_net <- if (is_home_shooter) {
        as.integer(!is.na(away_g) && away_g == 0)
      } else {
        as.integer(!is.na(home_g) && home_g == 0)
      }
      
      # ── Skater counts ───────────────────────────────────────────────────────
      # Useful for identifying 4v4, 3v3 OT situations
      shooting_skaters  <- if (is_home_shooter) home_sk else away_sk
      defending_skaters <- if (is_home_shooter) away_sk else home_sk
      
      data.frame(
        # Identifiers
        event_id       = p$eventId,
        period         = p$periodDescriptor$number %||% NA,
        period_type    = p$periodDescriptor$periodType %||% "REG",
        time           = p$timeInPeriod %||% NA,
        game_date      = game_date,
        
        # Event type
        event_type     = p$typeDescKey,
        
        # Shot location
        x              = p$details$xCoord    %||% NA,
        y              = p$details$yCoord    %||% NA,
        zone_code      = p$details$zoneCode  %||% NA,  # O=offensive, D=defensive, N=neutral
        
        # Shot characteristics
        shot_type      = p$details$shotType  %||% NA,
        
        # Players
        shooter_id     = p$details$shootingPlayerId %||%
          p$details$scoringPlayerId   %||% NA,
        goalie_id      = p$details$goalieInNetId     %||% NA,
        
        # Strength state
        situation_code    = sit_code,
        strength_state    = strength_state,
        shooting_skaters  = shooting_skaters,
        defending_skaters = defending_skaters,
        empty_net         = empty_net,
        
        # Teams
        shooting_team  = ifelse(!is.na(owner) & owner == home_id,
                                home_team, away_team),
        defending_team = ifelse(!is.na(owner) & owner == home_id,
                                away_team, home_team),
        home_team      = home_team,
        away_team      = away_team
      )
    })
}

# =============================================================================
# 3. ENGINEER FEATURES FOR xG MODEL
# =============================================================================

engineer_features <- function(shots) {
  shots |>
    dplyr::mutate(
      
      # Standardize coordinates — all shots attack in +x direction
      # Net is always at x=89 after standardization
      x_standardized = abs(x),
      
      # Distance from net center (net at x=89, y=0)
      # Total rink = 200ft | half = 100ft | net = 100-11 = 89ft from center
      distance = sqrt((x_standardized - 89)^2 + y^2),
      
      # Angle from net (0=straight on, 90=from the side)
      angle = abs(atan2(abs(y), abs(89 - x_standardized)) * (180 / pi)),
      
      # Outcome variable
      goal = as.integer(event_type == "goal"),
      
      # Convert time to seconds for rebound calculation
      time_seconds = as.integer(substr(time, 1, 2)) * 60 +
        as.integer(substr(time, 4, 5)),
      
      # Overtime indicator
      overtime = as.integer(period > 3)
      
    ) |>
    # Rebound: shot within 3 seconds of previous shot in same period
    arrange(period, time_seconds) |>
    group_by(period) |>
    mutate(
      prev_time  = lag(time_seconds),
      is_rebound = as.integer(
        !is.na(prev_time) & (time_seconds - prev_time) <= 3
      )
    ) |>
    ungroup() |>
    # Rush: shot within 10 seconds of previous, from close range
    mutate(
      is_rush = as.integer(
        !is.na(prev_time) &
          (time_seconds - prev_time) > 3 &
          (time_seconds - prev_time) <= 10 &
          distance < 50
      )
    )
}

# =============================================================================
# 4. BUILD SHOT DATASET FOR ONE GAME
# =============================================================================

build_shot_dataset <- function(game_id) {
  message("Pulling game: ", game_id)
  
  raw <- tryCatch(
    get_game_pbp(game_id),
    error = function(e) {
      message("  Error fetching game ", game_id, ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(raw) || length(raw$plays) == 0) return(NULL)
  
  shots <- tryCatch(
    parse_shots(raw),
    error = function(e) {
      message("  Error parsing game ", game_id, ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(shots) || nrow(shots) == 0) return(NULL)
  
  data <- engineer_features(shots)
  data$game_id <- game_id
  
  return(data)
}

# =============================================================================
# 5. SAVE SHOTS TO DISK
# =============================================================================

save_shots <- function(data, filename = "data/shots.rds") {
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  saveRDS(data, filename)
  message("Saved ", nrow(data), " shots to ", filename)
}

# =============================================================================
# 6. PULL ALL GAME IDs FOR A SEASON
# =============================================================================

nhl_teams <- c("ANA","BOS","BUF","CAR","CBJ","CGY","CHI","COL",
               "DAL","DET","EDM","FLA","LAK","MIN","MTL","NJD",
               "NSH","NYI","NYR","OTT","PHI","PIT","SEA","SJS",
               "STL","TBL","TOR","UTA","VAN","VGK","WPG","WSH")

get_season_game_ids <- function(season, game_types = c(2, 3)) {
  all_ids <- c()
  
  for (team in nhl_teams) {
    url <- glue("{NHL_API}/club-schedule-season/{team}/{season}")
    raw <- nhl_get(url)
    
    if (is.null(raw)) next
    
    ids <- purrr::keep(raw$games, ~.x$gameType %in% game_types) |>
      purrr::map_int(~.x$id)
    
    all_ids <- unique(c(all_ids, ids))
    Sys.sleep(0.3)
  }
  
  message("Season ", season, ": ", length(all_ids), " unique games found")
  return(all_ids)
}

# =============================================================================
# 7. PULL MULTIPLE SEASONS (saves each season individually)
# =============================================================================

pull_multiple_seasons <- function(seasons) {
  purrr::map_dfr(seasons, function(season) {
    message("\n~~~~~PULLING SEASON ", season, "~~~~~")
    
    game_ids <- get_season_game_ids(season)
    
    season_data <- purrr::map_dfr(game_ids, function(gid) {
      Sys.sleep(0.5)
      tryCatch(
        build_shot_dataset(gid),
        error = function(e) {
          message("Failed on game ", gid, ": ", e$message)
          NULL
        }
      )
    })
    
    # Add game_type and season columns from game_id
    season_data <- season_data |>
      mutate(
        game_type = case_when(
          substr(as.character(game_id), 5, 6) == "02" ~ "Regular Season",
          substr(as.character(game_id), 5, 6) == "03" ~ "Playoffs",
          TRUE ~ "Other"
        ),
        season = season
      )
    
    # Save each season immediately — if it crashes you don't lose everything
    save_shots(season_data, glue("data/shots_{season}.rds"))
    message("Season ", season, " complete: ", nrow(season_data), " shots")
    
    season_data
  })
}

# =============================================================================
# 8. PULL PLAYER METADATA
# =============================================================================
# Maps player IDs to names, positions, headshots
# Run after pulling shot data to get names for the dashboard

pull_player_metadata <- function(player_ids) {
  player_ids <- unique(player_ids[!is.na(player_ids)])
  message("Pulling metadata for ", length(player_ids), " players...")
  
  purrr::map_dfr(player_ids, function(pid) {
    Sys.sleep(0.2)
    raw <- tryCatch(
      nhl_get(glue("{NHL_API}/player/{pid}/landing")),
      error = function(e) NULL
    )
    if (is.null(raw)) return(NULL)
    
    data.frame(
      player_id  = pid,
      first_name = raw$firstName$default  %||% NA,
      last_name  = raw$lastName$default   %||% NA,
      full_name  = paste(raw$firstName$default %||% "",
                         raw$lastName$default  %||% ""),
      position   = raw$position           %||% NA,
      team       = raw$currentTeamAbbrev  %||% NA,
      number     = raw$sweaterNumber      %||% NA,
      headshot   = raw$headshot           %||% NA,
      shoots     = raw$shootsCatches      %||% NA,
      birth_date = raw$birthDate          %||% NA,
      birth_country = raw$birthCountry    %||% NA
    )
  })
}

message("01_data_pipeline.R loaded successfully")
message("Key functions: build_shot_dataset, pull_multiple_seasons,")
message("               get_season_game_ids, pull_player_metadata, save_shots")

