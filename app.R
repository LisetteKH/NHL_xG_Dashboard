# app.R
# NHL xG Shiny Dashboard

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(ggimage)
library(gganimate)
library(gifski)
library(purrr)
library(httr)
library(jsonlite)
library(tidyr)
library(MASS)

#  Load data 
shots <- readRDS("data/shots_all.rds")
model <- readRDS("data/model_gam.rds")

# Pre-compute 2025-26 season profiles for game predictions
season_profiles <- {
  xgf <- shots |>
    filter(season=="20252026", game_type=="Regular Season", empty_net==0) |>
    group_by(shooting_team) |>
    summarise(games    = n_distinct(game_id),
              xgf_game = round(sum(xg,na.rm=TRUE)/n_distinct(game_id), 2),
              .groups  = "drop")
  xga <- shots |>
    filter(season=="20252026", game_type=="Regular Season", empty_net==0) |>
    group_by(defending_team) |>
    summarise(xga_game = round(sum(xg,na.rm=TRUE)/n_distinct(game_id), 2),
              .groups  = "drop")
  xgf |> left_join(xga, by=c("shooting_team"="defending_team"))
}

# API helpers
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
NHL_API <- "https://api-web.nhle.com/v1"
nhl_get <- function(url) {
  res <- httr::GET(url)
  if (httr::status_code(res) != 200) return(NULL)
  jsonlite::fromJSON(httr::content(res, "text", encoding="UTF-8"),
                     simplifyVector=FALSE)
}

nhl_teams <- c("ANA","BOS","BUF","CAR","CBJ","CGY","CHI","COL",
               "DAL","DET","EDM","FLA","LAK","MIN","MTL","NJD",
               "NSH","NYI","NYR","OTT","PHI","PIT","SEA","SJS",
               "STL","TBL","TOR","UTA","VAN","VGK","WPG","WSH")

# CUSTOM CSS
custom_css <- "
  body, .wrapper { background-color: #0d1117 !important; }
  .content-wrapper { background-color: #0d1117 !important; }
  .main-sidebar, .left-side { background-color: #161b22 !important; }
  .sidebar-menu > li > a { color: #8b949e !important; font-size: 13px; }
  .sidebar-menu > li.active > a,
  .sidebar-menu > li > a:hover { color: #ffffff !important; background-color: #21262d !important; }
  .sidebar-menu > li > a .fa { color: #58a6ff !important; }
  .main-header .navbar, .main-header .logo {
    background-color: #161b22 !important;
    border-bottom: 1px solid #30363d !important;
  }
  .main-header .logo { color: #58a6ff !important; font-weight: 700; font-size: 16px; }
  .box {
    background: #161b22 !important;
    border: 1px solid #30363d !important;
    border-top: 3px solid #58a6ff !important;
    border-radius: 6px !important;
    box-shadow: none !important;
  }
  .box-header { background: #161b22 !important; border-bottom: 1px solid #30363d !important; }
  .box-title { color: #e6edf3 !important; font-size: 13px; font-weight: 600; letter-spacing: 0.5px; text-transform: uppercase; }
  .small-box { border-radius: 6px !important; border: 1px solid #30363d !important; }
  .small-box h3 { font-size: 28px !important; font-weight: 700; }
  .small-box p  { font-size: 12px !important; text-transform: uppercase; letter-spacing: 0.5px; }
  .small-box.bg-blue  { background-color: #1f6feb !important; }
  .small-box.bg-red   { background-color: #da3633 !important; }
  .small-box.bg-green { background-color: #238636 !important; }
  table.dataTable { background: #161b22 !important; color: #e6edf3 !important; border: none !important; }
  table.dataTable thead th {
    background: #21262d !important; color: #e6edf3 !important;
    border-bottom: 1px solid #30363d !important;
    font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; font-weight: 600;
  }
  table.dataTable tbody tr { background: #161b22 !important; border-bottom: 1px solid #21262d !important; }
  table.dataTable tbody tr:hover { background: #21262d !important; }
  .dataTables_wrapper, .dataTables_info,
  .dataTables_length, .dataTables_filter { color: #8b949e !important; font-size: 12px; }
  .dataTables_scrollHead table.dataTable thead th { color: #e6edf3 !important; }
  .metric-box {
    background: #21262d;
    border: 1px solid #30363d;
    border-left: 4px solid #58a6ff;
    border-radius: 4px;
    padding: 10px 14px;
    margin-bottom: 10px;
    color: #e6edf3;
    font-size: 12px;
    line-height: 1.6;
    flex: 1;
    min-width: 200px;
  }
  .metric-box strong { color: #58a6ff; font-size: 13px; display: block; margin-bottom: 4px; }
  .metric-box .metric-example { color: #3fb950; font-weight: 600; font-size: 11px; margin-top: 4px; display: block; }
  .selectize-input { background: #21262d !important; color: #e6edf3 !important; border: 1px solid #30363d !important; }
  .selectize-dropdown { background: #21262d !important; color: #e6edf3 !important; border: 1px solid #30363d !important; }
  .selectize-dropdown-content .option:hover { background: #30363d !important; }
  .game-card-btn {
    background: #1f6feb; color: #fff; border: none;
    font-size: 11px; padding: 4px 10px; border-radius: 4px; cursor: pointer;
  }
  .game-card-btn:hover { background: #388bfd; }
"

# HELPER FUNCTIONS
cup_finals <- list(
  "20192020" = list(champion="TBL", runnerup="DAL"),
  "20202021" = list(champion="TBL", runnerup="MTL"),
  "20212022" = list(champion="COL", runnerup="TBL"),
  "20222023" = list(champion="VGK", runnerup="FLA"),
  "20232024" = list(champion="FLA", runnerup="EDM"),
  "20242025" = list(champion="FLA", runnerup="EDM")
)

metric_box <- function(title, explanation, example=NULL) {
  div(class="metric-box",
      tags$strong(title),
      p(style="margin:0;", explanation),
      if (!is.null(example)) tags$span(class="metric-example", example)
  )
}

scatter_theme <- function() {
  theme_minimal() +
    theme(
      plot.background  = element_rect(fill="#0d1117", color=NA),
      panel.background = element_rect(fill="#0d1117", color=NA),
      panel.grid.major = element_line(color="#21262d", linewidth=0.3),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(color="#8b949e", size=9),
      axis.title       = element_text(color="#8b949e", size=10),
      plot.title       = element_text(color="#e6edf3", size=14, face="bold", hjust=0.5),
      plot.subtitle    = element_text(color="#8b949e", size=10, hjust=0.5),
      plot.margin      = margin(20, 20, 20, 20)
    )
}

add_quadrants <- function(p, xga_mid, xgf_mid, all_xga, all_xgf) {
  p +
    annotate("rect", xmin=all_xga[1]*0.97, xmax=xga_mid, ymin=xgf_mid, ymax=all_xgf[2]*1.03, fill="#3fb950", alpha=0.06) +
    annotate("rect", xmin=xga_mid, xmax=all_xga[2]*1.03, ymin=xgf_mid, ymax=all_xgf[2]*1.03, fill="#f0a500", alpha=0.06) +
    annotate("rect", xmin=all_xga[1]*0.97, xmax=xga_mid, ymin=all_xgf[1]*0.97, ymax=xgf_mid, fill="#8b949e", alpha=0.06) +
    annotate("rect", xmin=xga_mid, xmax=all_xga[2]*1.03, ymin=all_xgf[1]*0.97, ymax=xgf_mid, fill="#f85149", alpha=0.06) +
    geom_vline(xintercept=xga_mid, color="#30363d", linewidth=0.6, linetype="dashed") +
    geom_hline(yintercept=xgf_mid, color="#30363d", linewidth=0.6, linetype="dashed") +
    annotate("text", x=all_xga[1]+(xga_mid-all_xga[1])*0.15, y=all_xgf[2]-(all_xgf[2]-xgf_mid)*0.08,
             label="High Offense\nLow Goals Against", color="#3fb950", size=3, fontface="bold") +
    annotate("text", x=all_xga[2]-(all_xga[2]-xga_mid)*0.15, y=all_xgf[2]-(all_xgf[2]-xgf_mid)*0.08,
             label="High Offense\nHigh Goals Against", color="#f0a500", size=3, fontface="bold") +
    annotate("text", x=all_xga[1]+(xga_mid-all_xga[1])*0.15, y=all_xgf[1]+(xgf_mid-all_xgf[1])*0.08,
             label="Low Offense\nLow Goals Against", color="#8b949e", size=3, fontface="bold") +
    annotate("text", x=all_xga[2]-(all_xga[2]-xga_mid)*0.15, y=all_xgf[1]+(xgf_mid-all_xgf[1])*0.08,
             label="Low Offense\nHigh Goals Against", color="#f85149", size=3, fontface="bold")
}

build_reg_vs_combined <- function(data) {
  playoff_teams <- data |> filter(game_type=="Playoffs") |> pull(shooting_team) |> unique()
  reg <- data |>
    filter(game_type=="Regular Season", shooting_team %in% playoff_teams) |>
    group_by(team=shooting_team) |>
    summarise(xgf=sum(xg,na.rm=TRUE), games=n_distinct(game_id), .groups="drop") |>
    left_join(
      data |> filter(game_type=="Regular Season", defending_team %in% playoff_teams) |>
        group_by(team=defending_team) |> summarise(xga=sum(xg,na.rm=TRUE), .groups="drop"),
      by="team"
    ) |>
    mutate(xgf=xgf/games, xga=xga/games, phase="1 Regular Season")
  combined <- data |>
    filter(shooting_team %in% playoff_teams) |>
    group_by(team=shooting_team) |>
    summarise(xgf=sum(xg,na.rm=TRUE), games=n_distinct(game_id), .groups="drop") |>
    left_join(
      data |> filter(defending_team %in% playoff_teams) |>
        group_by(team=defending_team) |> summarise(xga=sum(xg,na.rm=TRUE), .groups="drop"),
      by="team"
    ) |>
    mutate(xgf=xgf/games, xga=xga/games, phase="2 Regular Season + Playoffs")
  list(
    reg=reg, combined=combined,
    all=bind_rows(reg, combined) |>
      mutate(logo_url=paste0("https://assets.nhle.com/logos/nhl/svg/",team,"_light.svg"),
             phase=factor(phase, levels=c("1 Regular Season","2 Regular Season + Playoffs")))
  )
}

draw_offensive_zone <- function() {
  list(
    annotate("segment", x=-42.5, xend=42.5, y=49, yend=49, color="#e6edf3", linewidth=1.5),
    annotate("segment", x=-42.5, xend=-42.5, y=49, yend=88, color="#e6edf3", linewidth=1.5),
    annotate("segment", x=42.5,  xend=42.5,  y=49, yend=88, color="#e6edf3", linewidth=1.5),
    annotate("path",
             x=42.5*cos(seq(0, pi, length.out=100)),
             y=88 + 8*sin(seq(0, pi, length.out=100)),
             color="#e6edf3", linewidth=1.5),
    annotate("segment", x=-3, xend=3, y=89, yend=89, color="#ff0000", linewidth=1),
    annotate("rect", xmin=-3, xmax=3, ymin=89, ymax=92, fill=NA, color="#ff0000", linewidth=0.8),
    annotate("path",
             x=6*cos(seq(pi, 2*pi, length.out=100)),
             y=89 + 6*sin(seq(pi, 2*pi, length.out=100)),
             color="#4488ff", linewidth=0.8),
    annotate("segment", x=-6, xend=6, y=89, yend=89, color="#4488ff", linewidth=0.5),
    annotate("segment", x=-42.5, xend=42.5, y=75, yend=75, color="#4488ff", linewidth=1.5),
    annotate("path",
             x=-22 + 15*cos(seq(0, 2*pi, length.out=100)),
             y= 69 + 15*sin(seq(0, 2*pi, length.out=100)),
             color="#ff0000", linewidth=0.8),
    annotate("point", x=-22, y=69, color="#ff0000", size=2.5),
    annotate("path",
             x=22 + 15*cos(seq(0, 2*pi, length.out=100)),
             y=69 + 15*sin(seq(0, 2*pi, length.out=100)),
             color="#ff0000", linewidth=0.8),
    annotate("point", x=22, y=69, color="#ff0000", size=2.5),
    annotate("path",
             x=20*cos(seq(pi, 2*pi, length.out=100)),
             y=89 + 20*sin(seq(pi, 2*pi, length.out=100)),
             color="#ffffff", linewidth=0.4, linetype="dashed", alpha=0.5),
    annotate("path",
             x=40*cos(seq(pi, 2*pi, length.out=100)),
             y=89 + 40*sin(seq(pi, 2*pi, length.out=100)),
             color="#ffffff", linewidth=0.4, linetype="dashed", alpha=0.5),
    annotate("text", x=0,  y=91, label="NET",         color="#ff4444", size=2.5, fontface="bold"),
    annotate("text", x=18, y=72, label="High Danger", color="#ffffff", size=2.2, alpha=0.7),
    annotate("text", x=28, y=53, label="Mid Danger",  color="#ffffff", size=2.2, alpha=0.7)
  )
}

plot_heatmap <- function(data, team, title) {
  team_data <- data |>
    filter(shooting_team==team, empty_net==0,
           !is.na(x_standardized), !is.na(y), x_standardized>=50)
  if (nrow(team_data) < 10) return(ggplot() + scatter_theme())
  ggplot(team_data, aes(x=y, y=x_standardized)) +
    stat_density_2d(aes(fill=after_stat(level)), geom="polygon", alpha=0.65, bins=15) +
    scale_fill_gradientn(colors=c("#0d1117","#1f6feb","#f0a500","#f85149"), guide="none") +
    draw_offensive_zone() +
    coord_fixed(xlim=c(-42.5,42.5), ylim=c(49,100)) +
    labs(title=title) +
    scatter_theme() +
    theme(axis.text=element_blank(), axis.title=element_blank(),
          panel.grid=element_blank(),
          plot.background=element_rect(fill="#0d1117",color=NA),
          panel.background=element_rect(fill="#0d1117",color=NA))
}

describe_team <- function(team_name, z_volume, z_quality, z_distance, z_rebound, z_rush) {
  volume_text <- case_when(
    z_volume >  2 ~ paste0(team_name," generates an elite volume of shots,  one of the highest in the league."),
    z_volume >  1 ~ paste0(team_name," generates above average shot volume."),
    z_volume > -1 ~ paste0(team_name," generates league average shot volume."),
    z_volume > -2 ~ paste0(team_name," generates below average shot volume."),
    TRUE          ~ paste0(team_name," struggles to generate shots,  one of the lowest in the league.")
  )
  quality_text <- case_when(
    z_quality >  2 ~ "Their shot quality is elite,  they consistently find high danger areas.",
    z_quality >  1 ~ "Their shot quality is above average,  good shooting positions.",
    z_quality > -1 ~ "Their shot quality is league average,  shots come from typical positions.",
    z_quality > -2 ~ "Their shot quality is below average,  taking too many low danger shots.",
    TRUE           ~ "Their shot quality is poor,  consistently shooting from low danger areas."
  )
  distance_text <- case_when(
    z_distance < -2 ~ "They shoot from significantly closer than average,  elite shot positioning.",
    z_distance < -1 ~ "They shoot from closer than average,  good at getting to dangerous areas.",
    z_distance <  1 ~ "Their shot distance is league average.",
    z_distance <  2 ~ "They shoot from further out than average,  could improve shot positioning.",
    TRUE            ~ "They shoot from well outside average,  heavy reliance on perimeter shots."
  )
  rebound_text <- case_when(
    z_rebound >  2 ~ "They generate an elite number of rebound opportunities,  constant net front pressure.",
    z_rebound >  1 ~ "They generate above average rebounds,  good at creating second chances.",
    z_rebound > -1 ~ "Their rebound generation is league average.",
    TRUE           ~ "They generate below average rebounds,  limited net front presence."
  )
  rush_text <- case_when(
    z_rush >  2 ~ "They are elite in transition,  generating a high number of rush chances.",
    z_rush >  1 ~ "They generate above average rush chances,  effective in transition.",
    z_rush > -1 ~ "Their transition game is league average.",
    TRUE        ~ "They generate below average rush chances,  more of a set play team."
  )
  beat_text <- case_when(
    z_volume > 2 & z_quality < 1 ~
      "How to beat them: limit shot volume,  their individual shot quality is not elite enough to hurt you if you keep them to average shot counts. Defensive structure and quick clears are key.",
    z_quality > 2 & z_volume < 1 ~
      "How to beat them: take away their shooting lanes and force them to the perimeter. They win with elite shot positioning, so denying clean looks drops their xG dramatically.",
    z_volume > 1 & z_quality > 1 ~
      "How to beat them: you need elite goaltending,  they generate both high volume and high quality. There is no easy answer against this team.",
    z_rebound > 2 ~
      "How to beat them: control rebounds,  their offense runs on second chance opportunities. Goalies need to smother pucks and defenders need to box out aggressively.",
    TRUE ~
      "How to beat them: this is a balanced team,  you will need to win both the volume and quality battle to come out ahead."
  )
  paste(volume_text, quality_text, distance_text, rebound_text, rush_text, "\n\n", beat_text, sep=" ")
}

get_team_profile <- function(data, team) {
  team_shots <- data |> filter(shooting_team==team, empty_net==0,
                               !is.na(x_standardized), !is.na(y))
  if (nrow(team_shots)==0) return(NULL)
  team_shots |>
    group_by(game_id) |>
    summarise(shots=n(), xg=sum(xg,na.rm=TRUE),
              distance=mean(distance,na.rm=TRUE), angle=mean(angle,na.rm=TRUE),
              rebound=mean(is_rebound,na.rm=TRUE)*100, rush=mean(is_rush,na.rm=TRUE)*100,
              .groups="drop") |>
    summarise(shots_per_game=mean(shots), xg_per_shot=mean(xg/shots),
              avg_distance=mean(distance), avg_angle=mean(angle),
              rebound_pct=mean(rebound), rush_pct=mean(rush))
}

get_league_norms <- function(data) {
  data |>
    filter(empty_net==0, !is.na(x_standardized), !is.na(y)) |>
    group_by(shooting_team, game_id) |>
    summarise(shots=n(), xg=sum(xg,na.rm=TRUE),
              distance=mean(distance,na.rm=TRUE), angle=mean(angle,na.rm=TRUE),
              rebound=mean(is_rebound,na.rm=TRUE)*100, rush=mean(is_rush,na.rm=TRUE)*100,
              .groups="drop") |>
    group_by(shooting_team) |>
    summarise(shots_per_game=mean(shots), xg_per_shot=mean(xg/shots),
              avg_distance=mean(distance), avg_angle=mean(angle),
              rebound_pct=mean(rebound), rush_pct=mean(rush), .groups="drop") |>
    summarise(across(shots_per_game:rush_pct, list(mean=mean, sd=sd), .names="{.col}_{.fn}"))
}

parse_live_shots <- function(pbp) {
  shot_types <- c("shot-on-goal","goal","missed-shot")
  home_id   <- pbp$homeTeam$id
  away_id   <- pbp$awayTeam$id
  home_team <- pbp$homeTeam$abbrev
  away_team <- pbp$awayTeam$abbrev
  shots_raw <- purrr::keep(pbp$plays, ~.x$typeDescKey %in% shot_types)
  if (length(shots_raw)==0) return(NULL)
  purrr::map_dfr(shots_raw, function(p) {
    owner      <- p$details$eventOwnerTeamId %||% NA
    x_raw      <- p$details$xCoord %||% NA
    y_raw      <- p$details$yCoord %||% NA
    sit_code   <- p$situationCode %||% "1551"
    away_g     <- as.integer(substr(sit_code,1,1))
    away_sk    <- as.integer(substr(sit_code,2,2))
    home_sk    <- as.integer(substr(sit_code,3,3))
    home_g_val <- as.integer(substr(sit_code,4,4))
    is_home    <- !is.na(owner) && owner==home_id
    strength_state <- if (away_sk==home_sk) "5v5" else if (is_home) {
      if (home_sk>away_sk) "Powerplay" else "Shorthanded"
    } else { if (away_sk>home_sk) "Powerplay" else "Shorthanded" }
    empty_net  <- if (is_home) as.integer(away_g==0) else as.integer(home_g_val==0)
    time_s     <- as.integer(substr(p$timeInPeriod,1,2))*60 + as.integer(substr(p$timeInPeriod,4,5))
    data.frame(
      event_id=p$eventId, period=p$periodDescriptor$number %||% 1,
      time_seconds=time_s, event_type=p$typeDescKey,
      x=x_raw, y=y_raw, x_standardized=if(!is.na(x_raw)) abs(x_raw) else NA,
      shot_type=p$details$shotType %||% NA,
      shooting_team=ifelse(is_home,home_team,away_team),
      defending_team=ifelse(is_home,away_team,home_team),
      home_team=home_team, away_team=away_team,
      strength_state=strength_state, empty_net=empty_net,
      goal=as.integer(p$typeDescKey=="goal"),
      stringsAsFactors=FALSE
    )
  }) |>
    dplyr::mutate(
      distance=sqrt((x_standardized-89)^2+y^2),
      angle=abs(atan2(abs(y),abs(89-x_standardized))*(180/pi)),
      overtime=as.integer(period>3)
    ) |>
    dplyr::arrange(period, time_seconds) |>
    dplyr::mutate(
      prev_time =dplyr::lag(time_seconds),
      is_rebound=as.integer(!is.na(prev_time)&(time_seconds-prev_time)<=3),
      is_rush   =as.integer(!is.na(prev_time)&(time_seconds-prev_time)>3&
                              (time_seconds-prev_time)<=10&distance<50)
    )
}
#UI
ui <- dashboardPage(
  skin="black",
  dashboardHeader(title=span(icon("hockey-puck")," NHL xG Analytics")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("League Overview",     tabName="overview",  icon=icon("chart-bar")),
      menuItem("Regular vs Playoffs", tabName="regvsplay", icon=icon("arrows-left-right")),
      menuItem("Strength State",      tabName="strength",  icon=icon("hockey-puck")),
      menuItem("Game Explorer",       tabName="games",     icon=icon("magnifying-glass")),
      menuItem("Team Spotlight",      tabName="team",      icon=icon("crosshairs")),
      menuItem("Goalie Dashboard",    tabName="goalies",   icon=icon("shield-halved"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    tabItems(
      
      # LEAGUE OVERVIEW
      tabItem(tabName="overview",
              fluidRow(column(12,
                              div(style="display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px;",
                                  metric_box("xGF% — Expected Goals For %",
                                             "Share of total shot quality a team generates vs allows. Above 50% means dominating shot quality.",
                                             "Example: CAR at 53.1% = best in league over 7 seasons"),
                                  metric_box("xGF — Expected Goals For",
                                             "Total expected goals generated based on shot quality. Higher is better.",
                                             "Example: CAR = most offensive shot quality in the league"),
                                  metric_box("xGA — Expected Goals Against",
                                             "Total expected goals allowed based on shot quality faced. Lower is better.",
                                             "Example: MIN = best defensive shot suppression"),
                                  metric_box("xGD — Expected Goals Differential",
                                             "xGF minus xGA. Positive means generating more shot quality than allowed.",
                                             "Example: CAR = best net shot quality over 7 seasons"),
                                  metric_box("Rebound Conversion",
                                             "Goals scored per shot on rebounds vs non-rebounds. Rebounds convert at 2-3x the normal rate.",
                                             "Rebounds: ~16% | Non-rebounds: ~6%"),
                                  metric_box("Expansion Teams",
                                             "When looking at performance over 7 years, ARI, SEA, and UTA appear in the bottom right due to expansion.")
                              )
              )),
              fluidRow(
                valueBoxOutput("vbox_total_shots", width=4),
                valueBoxOutput("vbox_goal_rate",   width=4),
                valueBoxOutput("vbox_avg_xg",      width=4)
              ),
              fluidRow(box(width=12,
                           title="Expected Goals For vs Against — Regular Season Only",
                           div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                               "Each logo = one team. Top-left = elite (high offense, low goals against). Logos toward bottom-right are struggling on both ends."),
                           div(style="display:flex;align-items:center;gap:12px;padding:8px 10px 0;",
                               selectInput("selected_season", label=NULL,
                                           choices=c("All Seasons (7 Year View)"="all",
                                                     "2019-20"="20192020","2020-21"="20202021","2021-22"="20212022",
                                                     "2022-23"="20222023","2023-24"="20232024","2024-25"="20242025","2025-26"="20252026"),
                                           selected="all", width="220px"),
                               uiOutput("cup_final_label")
                           ),
                           plotOutput("plot_xgf_scatter", height="520px"),
                           div(style="display:flex;gap:10px;flex-wrap:wrap;margin-top:12px;padding:0 10px 10px;",
                               div(style="flex:1;min-width:180px;padding:10px;background:#0d1f15;border-left:4px solid #3fb950;border-radius:4px;",
                                   tags$strong(style="color:#3fb950;","High Offense / Low Goals Against"),
                                   p(style="color:#8b949e;font-size:12px;margin:4px 0 0;","Elite teams. Stanley Cup contenders live here.")),
                               div(style="flex:1;min-width:180px;padding:10px;background:#1f1a0d;border-left:4px solid #f0a500;border-radius:4px;",
                                   tags$strong(style="color:#f0a500;","High Offense / High Goals Against"),
                                   p(style="color:#8b949e;font-size:12px;margin:4px 0 0;","Boom or bust. Exciting but vulnerable in playoffs.")),
                               div(style="flex:1;min-width:180px;padding:10px;background:#0d0d1f;border-left:4px solid #8b949e;border-radius:4px;",
                                   tags$strong(style="color:#8b949e;","Low Offense / Low Goals Against"),
                                   p(style="color:#8b949e;font-size:12px;margin:4px 0 0;","Defensive. Limiting chances but can't generate enough offensively.")),
                               div(style="flex:1;min-width:180px;padding:10px;background:#1f0d0d;border-left:4px solid #f85149;border-radius:4px;",
                                   tags$strong(style="color:#f85149;","Low Offense / High Goals Against"),
                                   p(style="color:#8b949e;font-size:12px;margin:4px 0 0;","Rebuilding. Struggling on both ends."))
                           )
              )),
              fluidRow(
                box(width=5, title="Team xGF% Rankings",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "Sorted by xGF%. Teams above 50% are generating more shot quality than they allow."),
                    DTOutput("table_team_xgf")),
                box(width=7, title="League xGF% by Team",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "xGF% above 50% means a team generates more shot quality than it allows. Above 55% is elite over a full season."),
                    plotlyOutput("plot_team_xgf", height="480px"))
              ),
              fluidRow(
                box(width=6, title="Goal Rate by Season",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "Goals divided by total shots. League-wide rate hovers around 7%. Higher means more shots are finding the net."),
                    plotlyOutput("plot_goal_rate", height="280px")),
                box(width=6, title="Rebound vs Non-Rebound Conversion",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "Rebounds convert at 2-3x the rate of regular shots because the goalie is already out of position."),
                    plotlyOutput("plot_rebound", height="280px"))
              )
      ),
      
      # REGULAR vs PLAYOFFS
      tabItem(tabName="regvsplay",
              fluidRow(column(12, div(style="display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px;",
                                      div(class="metric-box",
                                          tags$strong("How to read this"),
                                          p(style="margin:0;","Left = Regular Season per game. Right = Regular Season + Playoffs combined.
               The animation shows logos moving from regular season to combined position.
               LEFT = better defense in playoffs. UP = better offense."),
                                          tags$span(class="metric-example","Teams that elevate shift toward the green quadrant")
                                      )
              ))),
              fluidRow(column(12, div(style="padding:0 15px 10px;",
                                      selectInput("regplay_season", label=NULL,
                                                  choices=c("All Seasons (7 Year View)"="all",
                                                            "2019-20"="20192020","2020-21"="20202021","2021-22"="20212022",
                                                            "2022-23"="20222023","2023-24"="20232024","2024-25"="20242025","2025-26"="20252026"),
                                                  selected="20232024", width="220px")
              ))),
              fluidRow(
                box(width=6, title="Regular Season (Per Game)",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "Each logo = one team. Up = more offense, left = better defense. Green quadrant = elite."),
                    plotOutput("plot_reg_static", height="430px")),
                box(width=6, title="Regular Season + Playoffs (Per Game)",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "Same chart with playoff games added. Teams that move up or left elevated their game when it mattered."),
                    plotOutput("plot_combined_static", height="430px"))
              ),
              fluidRow(box(width=12,
                           title="Animation — Logos Move from Regular Season to Combined Position",
                           div(style="text-align:center;color:#8b949e;font-size:12px;padding:4px 10px;",
                               uiOutput("regplay_cup_label")),
                           imageOutput("plot_reg_vs_play", height="420px")
              ))
      ),
      
      # STRENGTH STATE
      tabItem(tabName="strength",
              fluidRow(column(12, div(style="display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px;",
                                      div(class="metric-box",
                                          tags$strong("How to read this"),
                                          p(style="margin:0;","5v5 shows true team quality. Powerplay shows offensive unit effectiveness.
               Penalty Kill shows defensive unit effectiveness. xGF% above 50% = generating more shot quality than allowing."),
                                          tags$span(class="metric-example","A team can be elite 5v5 but have a bad powerplay, roster construction matters")
                                      )
              ))),
              fluidRow(
                column(3, selectInput("strength_season", label="Season",
                                      choices=c("All Seasons"="all","2019-20"="20192020","2020-21"="20202021",
                                                "2021-22"="20212022","2022-23"="20222023","2023-24"="20232024",
                                                "2024-25"="20242025","2025-26"="20252026"),
                                      selected="all", width="100%")),
                column(3, selectInput("strength_situation", label="Situation",
                                      choices=c("5v5"="5v5","Powerplay"="Powerplay","Penalty Kill"="Shorthanded"),
                                      selected="5v5", width="100%")),
                column(3, selectInput("strength_team", label="Team Spotlight",
                                      choices=c("All Teams"="all", sort(nhl_teams)),
                                      selected="all", width="100%"))
              ),
              fluidRow(
                box(width=8, title="xGF vs xGA by Situation",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "Compares shot quality generated vs allowed at the selected strength state. Per game to account for ice time differences."),
                    plotOutput("plot_strength_scatter", height="480px")),
                box(width=4, title="Rankings",                 DTOutput("table_strength_rank"))
              ),
              fluidRow(box(width=12, title="Team Breakdown — All Three Situations",
                           div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                               "xGF% at 5v5, powerplay, and penalty kill side by side. A team can dominate 5v5 but have a weak powerplay. Hover each bar for exact xGF%."),
                           plotlyOutput("plot_strength_breakdown", height="380px")))
      ),
      
      # GAME EXPLORER
      tabItem(tabName="games",
              fluidRow(column(12, div(style="display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px;",
                                      div(class="metric-box",
                                          tags$strong("Live & Historical Game Explorer"),
                                          p(style="margin:0;","Live games show real-time xG with predictions. Future games show predicted scores.
               Historical games show shot-by-shot xG timeline and whether the result matched shot quality."),
                                          tags$span(class="metric-example","xG winner ≠ actual winner about 30% of the time")
                                      )
              ))),
              fluidRow(box(width=12, title="Live & Upcoming Games",
                           div(style="display:flex;justify-content:flex-end;margin-bottom:8px;",
                               actionButton("refresh_live","Refresh", icon=icon("rotate"),
                                            style="background:#21262d;color:#e6edf3;border:1px solid #30363d;")),
                           uiOutput("live_games_ui")
              )),
              fluidRow(box(width=12, title="Historical Game Explorer",
                           div(style="display:flex;gap:12px;flex-wrap:wrap;padding:0 0 12px;",
                               selectInput("game_season", label="Season",
                                           choices=c("2019-20"="20192020","2020-21"="20202021","2021-22"="20212022",
                                                     "2022-23"="20222023","2023-24"="20232024","2024-25"="20242025","2025-26"="20252026"),
                                           selected="20252026", width="160px"),
                               selectInput("game_type_filter", label="Game Type",
                                           choices=c("Playoffs"="Playoffs","Regular Season"="Regular Season"),
                                           selected="Playoffs", width="160px"),
                               selectInput("game_team_filter", label="Team",
                                           choices=c("All Teams"="all", sort(nhl_teams)),
                                           selected="all", width="160px")
                           ),
                           DTOutput("table_game_select")
              )),
              fluidRow(box(width=12, title="Game Recap",
                           uiOutput("game_recap_header"),
                           fluidRow(
                             column(6,
                                    div(style="color:#8b949e;font-size:11px;padding:4px 0 6px;",
                                        "Bars show total expected goals. Gold diamond shows actual goals. When the bar is taller than the diamond, the team underperformed their chances."),
                                    plotOutput("plot_game_xg_bar", height="280px")),
                             column(6,
                                    div(style="color:#8b949e;font-size:11px;padding:4px 0 6px;",
                                        "Cumulative xG over time. A steeper slope means higher quality chances. Stars mark actual goals. When a team scores below their xG line, they got lucky,  above it, unlucky."),
                                    plotlyOutput("plot_game_timeline", height="280px"))
                           )
              ))
      ),
      
      # TEAM SPOTLIGHT
      tabItem(tabName="team",
              fluidRow(
                column(3, selectInput("team_season", label="Season",
                                      choices=c("All Seasons"="all","2019-20"="20192020","2020-21"="20202021",
                                                "2021-22"="20212022","2022-23"="20222023","2023-24"="20232024",
                                                "2024-25"="20242025","2025-26"="20252026"),
                                      selected="all", width="100%")),
                column(3, selectInput("team_situation", label="Situation",
                                      choices=c("5v5"="5v5","Powerplay"="Powerplay","Penalty Kill"="Shorthanded"),
                                      selected="5v5", width="100%")),
                column(3, selectInput("team_gametype", label="Game Type",
                                      choices=c("Regular Season"="Regular Season","Playoffs"="Playoffs","Both"="both"),
                                      selected="Regular Season", width="100%")),
                column(3, selectInput("team_selected", label="Team",
                                      choices=sort(nhl_teams), selected="CAR", width="100%"))
              ),
              fluidRow(box(width=12, title="League Baseline", uiOutput("team_league_baseline"))),
              fluidRow(box(width=12, title="Team Profile",    uiOutput("team_narrative"))),
              fluidRow(
                box(width=6, title="Shot Density Map",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "Where shots come from in the offensive zone. Red = highest concentration. Teams with heat near the net are generating more dangerous looks."),
                    plotOutput("plot_team_heatmap", height="380px")),
                box(width=6, title="Danger Zone Breakdown",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "High danger = within 20ft of net. Mid danger = 20-40ft. Low danger = 40ft+. High danger shots convert at roughly 3x the rate of low danger shots. Hover for goals and avg xG per zone."),
                    plotlyOutput("plot_team_danger", height="380px"))
              ),
              fluidRow(
                box(width=6, title="Team Profile vs League Average",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "Each bar shows how far this team is from the league average. +1 = one standard deviation above average (top ~16% of teams). +2 = top ~2%. Negative = below average. Hover for exact values."),
                    plotlyOutput("plot_team_zscore", height="330px")),
                box(width=6, title="Home vs Away Split",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "Large gaps between home and away suggest the team plays differently on the road. Elite teams perform consistently in both settings."),
                    plotlyOutput("plot_team_homeaway", height="330px"))
              ),
              fluidRow(box(width=12, title="Head to Head Comparison",
                           div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                               "Select two teams to compare their shot profiles side by side. The heatmaps show where each team shoots from,  overlapping red zones mean similar tendencies, different zones reveal contrasting styles. The panel in the middle breaks down who has the advantage on each metric with actual numbers."),
                           div(style="display:flex;gap:12px;padding:0 0 12px;",
                               selectInput("h2h_team1", label="Team 1", choices=sort(nhl_teams), selected="CAR", width="160px"),
                               selectInput("h2h_team2", label="Team 2", choices=sort(nhl_teams), selected="EDM", width="160px")
                           ),
                           fluidRow(
                             column(5, plotOutput("plot_h2h_heatmap1", height="350px")),
                             column(2, uiOutput("h2h_narrative")),
                             column(5, plotOutput("plot_h2h_heatmap2", height="350px"))
                           )
              ))
      ),
      
      tabItem(tabName="goalies",
              fluidRow(
                column(12,
                       div(style="display:flex;gap:12px;flex-wrap:wrap;align-items:flex-end;padding:0 0 10px;",
                           div(
                             tags$label("Season", style="color:#8b949e;font-size:12px;display:block;margin-bottom:4px;"),
                             selectInput("goalie_season", label=NULL,
                                         choices=c("All Seasons"="all","2019-20"="20192020","2020-21"="20202021",
                                                   "2021-22"="20212022","2022-23"="20222023","2023-24"="20232024",
                                                   "2024-25"="20242025","2025-26"="20252026"),
                                         selected="all", width="160px")),
                           div(
                             tags$label("Game Type", style="color:#8b949e;font-size:12px;display:block;margin-bottom:4px;"),
                             selectInput("goalie_gametype", label=NULL,
                                         choices=c("Regular Season"="Regular Season","Playoffs"="Playoffs","Both"="both"),
                                         selected="Regular Season", width="160px")),
                           div(
                             tags$label("Min Shots Faced", style="color:#8b949e;font-size:12px;display:block;margin-bottom:4px;"),
                             sliderInput("goalie_min_shots", label=NULL, min=100, max=3000,
                                         value=500, step=100, width="220px"))
                       )
                )
              ),
              fluidRow(
                box(width=12, title="Goalie Leaderboard",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;margin-bottom:8px;",
                        "GSAx (Goals Saved Above Expected) = actual goals allowed minus expected goals allowed. Negative is GOOD,  the goalie stopped more than a league-average goalie would have. Click any row to load that goalie\'s profile below."),
                    DTOutput("table_goalie_leaderboard")
                )
              ),
              fluidRow(
                box(width=6, title="Goalie Profile,  Season by Season",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "GSAx per season for the selected goalie. Negative bars = above average performance. A consistently negative line across seasons signals a true elite goalie vs a one-year fluke."),
                    uiOutput("goalie_profile_header"),
                    plotlyOutput("plot_goalie_season", height="300px")
                ),
                box(width=6, title="Shot Quality Allowed",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "Save percentage broken down by shot danger zone. High danger (within 20ft of net) is hardest to stop,  league average is around 82%. Low danger shots should be stopped at 95%+. A goalie elite in high danger is genuinely special."),
                    plotlyOutput("plot_goalie_zones", height="300px")
                )
              ),
              fluidRow(
                box(width=12, title="Shot Map -- Goals Allowed vs Shots Faced",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;",
                        "Each point is a shot. Red = goal allowed, grey = save. Clusters of red in high danger areas are expected. Clusters of red from distance suggest unlucky bounces or weak positioning. Compare high vs low danger save% in the zone breakdown above."),
                    plotOutput("plot_goalie_shotmap", height="400px")
                )
              ),
              fluidRow(
                box(width=12, title="Team vs Goalie Matchup",
                    div(style="color:#8b949e;font-size:11px;padding:4px 10px 0;margin-bottom:12px;",
                        "Select a team to see how their offensive profile matches up against the selected goalie,  projected scoring based on shot locations, plus the actual historical record from games they have faced each other."),
                    fluidRow(
                      column(4,
                             selectInput("matchup_team", label="Select Team",
                                         choices=sort(nhl_teams), selected="CAR", width="100%")),
                      column(8,
                             uiOutput("matchup_summary_cards"))
                    ),
                    fluidRow(
                      column(6,
                             div(style="color:#e6edf3;font-weight:600;font-size:13px;padding:12px 0 6px;",
                                 "Projected Matchup -- Shot Quality vs Goalie Zones"),
                             div(style="color:#8b949e;font-size:11px;margin-bottom:8px;",
                                 "Bars show the team\'s shot volume from each danger zone. The line shows the goalie\'s save% in that zone. Where the bar is tall and the line is low = dangerous matchup for the goalie."),
                             plotlyOutput("plot_matchup_projection", height="300px")
                      ),
                      column(6,
                             div(style="color:#e6edf3;font-weight:600;font-size:13px;padding:12px 0 6px;",
                                 "Historical Head to Head"),
                             div(style="color:#8b949e;font-size:11px;margin-bottom:8px;",
                                 "Every game this team faced this goalie. xG shows how many goals the attack deserved,  actual goals shows how it played out. Games above the diagonal line = team outscored their xG."),
                             plotlyOutput("plot_matchup_history", height="300px")
                      )
                    )
                )
              )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # VALUE BOXES
  output$vbox_total_shots <- renderValueBox({
    valueBox(format(nrow(shots),big.mark=","), "Total Shots (7 Seasons)", icon("hockey-puck"), color="blue")
  })
  output$vbox_goal_rate <- renderValueBox({
    valueBox(paste0(round(mean(shots$goal)*100,2),"%"), "Overall Goal Rate", icon("bullseye"), color="red")
  })
  output$vbox_avg_xg <- renderValueBox({
    valueBox(round(mean(shots$xg,na.rm=TRUE),4), "Average xG Per Shot", icon("chart-line"), color="green")
  })
  
  # CUP FINAL LABELS
  output$cup_final_label <- renderUI({
    if (input$selected_season=="all") return(NULL)
    finals <- cup_finals[[input$selected_season]]
    if (is.null(finals)) return(NULL)
    div(style="color:#8b949e;font-size:12px;",
        icon("trophy",style="color:#f0a500;"), " ",
        span(style="color:#f0a500;font-weight:600;", finals$champion),
        " defeated ", span(style="color:#e6edf3;", finals$runnerup),
        " in the Stanley Cup Final")
  })
  
  output$regplay_cup_label <- renderUI({
    if (input$regplay_season=="all")
      return(span(style="color:#8b949e;font-size:12px;",
                  "Select a specific season to see the Stanley Cup Final result"))
    finals <- cup_finals[[input$regplay_season]]
    if (is.null(finals)) return(NULL)
    div(style="color:#8b949e;font-size:12px;",
        icon("trophy",style="color:#f0a500;"), " ",
        span(style="color:#f0a500;font-weight:600;", finals$champion),
        " defeated ", span(style="color:#e6edf3;", finals$runnerup),
        " in the Stanley Cup Final")
  })
  
  # LEAGUE OVERVIEW
  team_xgf_data <- reactive({
    data <- if (input$selected_season=="all") shots|>filter(game_type=="Regular Season") else
      shots|>filter(season==input$selected_season, game_type=="Regular Season")
    data|>group_by(shooting_team)|>summarise(xgf=sum(xg,na.rm=TRUE),.groups="drop")|>
      left_join(data|>group_by(defending_team)|>summarise(xga=sum(xg,na.rm=TRUE),.groups="drop"),
                by=c("shooting_team"="defending_team"))|>
      mutate(xgf_pct=xgf/(xgf+xga), xgd=round(xgf-xga,1),
             logo_url=paste0("https://assets.nhle.com/logos/nhl/svg/",shooting_team,"_light.svg"))|>
      arrange(desc(xgf_pct))
  })
  
  output$plot_xgf_scatter <- renderPlot({
    df <- team_xgf_data()
    xgf_mid <- median(df$xgf); xga_mid <- median(df$xga)
    all_xga <- range(df$xga);  all_xgf <- range(df$xgf)
    p <- ggplot(df, aes(x=xga,y=xgf))
    p <- add_quadrants(p, xga_mid, xgf_mid, all_xga, all_xgf)
    p + geom_image(aes(image=logo_url), size=0.055, asp=1.6) +
      scale_x_continuous(trans="reverse", limits=c(all_xga[2]*1.03,all_xga[1]*0.97)) +
      scale_y_continuous(limits=c(all_xgf[1]*0.97,all_xgf[2]*1.03)) +
      labs(x="Expected Goals Against (lower = better defense)",
           y="Expected Goals For (higher = better offense)") + scatter_theme()
  }, bg="#0d1117")
  
  output$table_team_xgf <- renderDT({
    df <- team_xgf_data()|>
      mutate(rank=row_number(),
             logo=paste0('<img src="https://assets.nhle.com/logos/nhl/svg/',shooting_team,'_light.svg" height="28"/>'),
             xgf_pct=paste0(round(xgf_pct*100,1),"%"), xgf=round(xgf,1), xga=round(xga,1))|>
      dplyr::select(Rank=rank,Logo=logo,Team=shooting_team,`xGF%`=xgf_pct,xGF=xgf,xGA=xga,xGD=xgd)
    datatable(df, escape=FALSE, rownames=FALSE,
              options=list(pageLength=32,dom="t",scrollY="460px",ordering=FALSE))
  })
  
  output$plot_team_xgf <- renderPlotly({
    df <- team_xgf_data()|>
      mutate(team=reorder(shooting_team,xgf_pct),
             category=ifelse(xgf_pct>0.5,"Above Average","Below Average"))
    p <- ggplot(df,aes(x=team,y=xgf_pct,fill=category,
                       text=paste0(shooting_team,"\nxGF%: ",round(xgf_pct*100,1),"%")))+
      geom_col(width=0.7)+geom_hline(yintercept=0.5,linetype="dashed",color="#f0a500",linewidth=0.6)+
      scale_fill_manual(values=c("Above Average"="#3fb950","Below Average"="#f85149"),name=NULL)+
      scale_y_continuous(labels=scales::percent_format(accuracy=1))+
      coord_flip()+labs(x=NULL,y="xGF%")+theme_minimal(base_size=11)+
      theme(plot.background=element_rect(fill="#161b22",color=NA),
            panel.background=element_rect(fill="#161b22",color=NA),
            panel.grid.major=element_line(color="#21262d"),panel.grid.minor=element_blank(),
            axis.text=element_text(color="#e6edf3",size=9),axis.title=element_text(color="#e6edf3"),
            legend.background=element_rect(fill="#161b22",color=NA),legend.text=element_text(color="#e6edf3"))
    ggplotly(p,tooltip="text")|>layout(paper_bgcolor="#161b22",plot_bgcolor="#161b22",
                                       font=list(color="#e6edf3"),legend=list(font=list(color="#e6edf3"),bgcolor="#161b22"))
  })
  
  output$plot_goal_rate <- renderPlotly({
    df <- shots|>filter(game_type=="Regular Season")|>group_by(season)|>
      summarise(goal_rate=mean(goal)*100,.groups="drop")
    p <- ggplot(df,aes(x=season,y=goal_rate,text=paste0(season,"\n",round(goal_rate,2),"%")))+
      geom_col(fill="#58a6ff",width=0.6)+
      geom_text(aes(label=paste0(round(goal_rate,2),"%")),vjust=-0.5,color="#e6edf3",size=3.5)+
      labs(x="Season",y="Goal Rate (%)")+theme_minimal()+
      theme(plot.background=element_rect(fill="#161b22",color=NA),
            panel.background=element_rect(fill="#161b22",color=NA),
            panel.grid.major=element_line(color="#21262d"),panel.grid.minor=element_blank(),
            axis.text=element_text(color="#e6edf3"),axis.title=element_text(color="#e6edf3"))
    ggplotly(p,tooltip="text")|>layout(paper_bgcolor="#161b22",plot_bgcolor="#161b22",font=list(color="#e6edf3"))
  })
  
  output$plot_rebound <- renderPlotly({
    df <- shots|>filter(!is.na(is_rebound))|>group_by(is_rebound)|>
      summarise(conversion_rate=mean(goal)*100,.groups="drop")|>
      mutate(type=ifelse(is_rebound==1,"Rebound","Non-Rebound"))
    p <- ggplot(df,aes(x=type,y=conversion_rate,fill=type,
                       text=paste0(type,"\n",round(conversion_rate,2),"%")))+
      geom_col(show.legend=FALSE,width=0.5)+
      geom_text(aes(label=paste0(round(conversion_rate,2),"%")),vjust=-0.5,color="#e6edf3",size=4)+
      scale_fill_manual(values=c("Rebound"="#f85149","Non-Rebound"="#58a6ff"))+
      labs(x=NULL,y="Conversion Rate (%)")+theme_minimal()+
      theme(plot.background=element_rect(fill="#161b22",color=NA),
            panel.background=element_rect(fill="#161b22",color=NA),
            panel.grid.major=element_line(color="#21262d"),panel.grid.minor=element_blank(),
            axis.text=element_text(color="#e6edf3"),axis.title=element_text(color="#e6edf3"))
    ggplotly(p,tooltip="text")|>layout(paper_bgcolor="#161b22",plot_bgcolor="#161b22",font=list(color="#e6edf3"))
  })
  
  # REGULAR VS PLAYOFFS
  reg_vs_combined <- reactive({
    data <- if (input$regplay_season=="all") shots else shots|>filter(season==input$regplay_season)
    build_reg_vs_combined(data)
  })
  
  output$plot_reg_vs_play <- renderImage({
    d <- reg_vs_combined(); combined <- d$all; reg <- d$reg
    all_xga <- range(combined$xga); all_xgf <- range(combined$xgf)
    xgf_mid <- median(reg$xgf); xga_mid <- median(reg$xga)
    p <- ggplot(combined, aes(x=xga,y=xgf))
    p <- add_quadrants(p, xga_mid, xgf_mid, all_xga, all_xgf)
    p <- p + geom_image(aes(image=logo_url), size=0.07, asp=1.6) +
      scale_x_continuous(trans="reverse", limits=c(all_xga[2]*1.03,all_xga[1]*0.97)) +
      scale_y_continuous(limits=c(all_xgf[1]*0.97,all_xgf[2]*1.03)) +
      labs(title="{closest_state}", x="xGA per Game (lower = better defense)",
           y="xGF per Game (higher = better offense)") +
      scatter_theme() +
      transition_states(phase, transition_length=2, state_length=1) +
      ease_aes("cubic-in-out")
    outfile <- tempfile(fileext=".gif")
    animate(p, width=700, height=420, fps=10, duration=3,
            renderer=gifski_renderer(outfile), bg="#0d1117")
    list(src=outfile, contentType="image/gif", width="100%")
  }, deleteFile=TRUE)
  
  output$plot_reg_static <- renderPlot({
    d <- reg_vs_combined()
    reg <- d$reg|>mutate(logo_url=paste0("https://assets.nhle.com/logos/nhl/svg/",team,"_light.svg"))
    all_xga <- range(d$all$xga); all_xgf <- range(d$all$xgf)
    xgf_mid <- median(d$reg$xgf); xga_mid <- median(d$reg$xga)
    p <- ggplot(reg, aes(x=xga,y=xgf))
    p <- add_quadrants(p, xga_mid, xgf_mid, all_xga, all_xgf)
    p + geom_image(aes(image=logo_url), size=0.08, asp=1) +
      scale_x_continuous(trans="reverse", limits=c(all_xga[2]*1.03,all_xga[1]*0.97)) +
      scale_y_continuous(limits=c(all_xgf[1]*0.97,all_xgf[2]*1.03)) +
      labs(x="xGA per Game", y="xGF per Game") + scatter_theme()
  }, bg="#0d1117")
  
  output$plot_combined_static <- renderPlot({
    d <- reg_vs_combined()
    comb <- d$combined|>mutate(logo_url=paste0("https://assets.nhle.com/logos/nhl/svg/",team,"_light.svg"))
    all_xga <- range(d$all$xga); all_xgf <- range(d$all$xgf)
    xgf_mid <- median(d$reg$xgf); xga_mid <- median(d$reg$xga)
    p <- ggplot(comb, aes(x=xga,y=xgf))
    p <- add_quadrants(p, xga_mid, xgf_mid, all_xga, all_xgf)
    p + geom_image(aes(image=logo_url), size=0.08, asp=1) +
      scale_x_continuous(trans="reverse", limits=c(all_xga[2]*1.03,all_xga[1]*0.97)) +
      scale_y_continuous(limits=c(all_xgf[1]*0.97,all_xgf[2]*1.03)) +
      labs(x="xGA per Game", y="xGF per Game") + scatter_theme()
  }, bg="#0d1117")
  
  # STRENGTH STATE
  strength_data <- reactive({
    data <- if (input$strength_season=="all")
      shots|>filter(game_type=="Regular Season",empty_net==0) else
        shots|>filter(season==input$strength_season,game_type=="Regular Season",empty_net==0)
    sit <- input$strength_situation
    if (sit=="Shorthanded") {
      xgf_data <- data|>filter(strength_state=="Shorthanded")|>
        group_by(team=shooting_team)|>summarise(xgf=sum(xg,na.rm=TRUE),games=n_distinct(game_id),.groups="drop")
      xga_data <- data|>filter(strength_state=="Powerplay")|>
        group_by(team=defending_team)|>summarise(xga=sum(xg,na.rm=TRUE),.groups="drop")
    } else {
      xgf_data <- data|>filter(strength_state==sit)|>
        group_by(team=shooting_team)|>summarise(xgf=sum(xg,na.rm=TRUE),games=n_distinct(game_id),.groups="drop")
      xga_data <- data|>filter(strength_state==sit)|>
        group_by(team=defending_team)|>summarise(xga=sum(xg,na.rm=TRUE),.groups="drop")
    }
    xgf_data|>left_join(xga_data,by="team")|>
      mutate(xgf=xgf/games, xga=xga/games, xgf_pct=round(xgf/(xgf+xga)*100,1),
             logo_url=paste0("https://assets.nhle.com/logos/nhl/svg/",team,"_light.svg"))|>
      arrange(desc(xgf_pct))
  })
  
  output$plot_strength_scatter <- renderPlot({
    df <- strength_data(); if (nrow(df)==0) return(NULL)
    xgf_mid <- median(df$xgf); xga_mid <- median(df$xga)
    all_xga <- range(df$xga); all_xgf <- range(df$xgf)
    df$highlight <- if (input$strength_team=="all") FALSE else df$team==input$strength_team
    p <- ggplot(df, aes(x=xga,y=xgf))
    p <- add_quadrants(p, xga_mid, xgf_mid, all_xga, all_xgf)
    p + geom_image(aes(image=logo_url, size=ifelse(highlight,0.09,0.06)), asp=1.6) +
      scale_size_identity() +
      scale_x_continuous(trans="reverse", limits=c(all_xga[2]*1.03,all_xga[1]*0.97)) +
      scale_y_continuous(limits=c(all_xgf[1]*0.97,all_xgf[2]*1.03)) +
      labs(x="xGA per Game (lower = better)", y="xGF per Game (higher = better)") + scatter_theme()
  }, bg="#0d1117")
  
  output$table_strength_rank <- renderDT({
    df <- strength_data()|>
      mutate(rank=row_number(),
             logo=paste0('<img src="https://assets.nhle.com/logos/nhl/svg/',team,'_light.svg" height="24"/>'),
             xgf=round(xgf,2), xga=round(xga,2), `xGF%`=paste0(xgf_pct,"%"))|>
      dplyr::select(Rank=rank,Logo=logo,Team=team,`xGF%`,xGF=xgf,xGA=xga)
    datatable(df,escape=FALSE,rownames=FALSE,
              options=list(pageLength=32,dom="t",scrollY="460px",ordering=FALSE))
  })
  
  output$plot_strength_breakdown <- renderPlotly({
    data <- if (input$strength_season=="all")
      shots|>filter(game_type=="Regular Season",empty_net==0) else
        shots|>filter(season==input$strength_season,game_type=="Regular Season",empty_net==0)
    df <- data|>filter(strength_state %in% c("5v5","Powerplay","Shorthanded"))|>
      group_by(team=shooting_team,strength_state)|>
      summarise(xgf=sum(xg,na.rm=TRUE),games=n_distinct(game_id),.groups="drop")|>
      left_join(
        data|>filter(strength_state %in% c("5v5","Powerplay","Shorthanded"))|>
          group_by(team=defending_team,strength_state)|>summarise(xga=sum(xg,na.rm=TRUE),.groups="drop"),
        by=c("team","strength_state")
      )|>mutate(xgf_pct=xgf/(xgf+xga)*100)
    if (input$strength_team!="all") {
      df <- df|>filter(team==input$strength_team)
    } else {
      top_teams <- df|>group_by(team)|>summarise(avg=mean(xgf_pct))|>
        arrange(desc(avg))|>head(10)|>pull(team)
      df <- df|>filter(team %in% top_teams)
    }
    df <- df|>mutate(strength_state=factor(strength_state,
                                           levels=c("5v5","Powerplay","Shorthanded"),labels=c("5v5","Power Play","Penalty Kill")))
    p <- ggplot(df,aes(x=reorder(team,xgf_pct),y=xgf_pct,fill=strength_state,
                       text=paste0(team," — ",strength_state,"\nxGF%: ",round(xgf_pct,1),"%")))+
      geom_col(position="dodge",width=0.7)+
      geom_hline(yintercept=50,linetype="dashed",color="#f0a500",linewidth=0.6)+
      scale_fill_manual(values=c("5v5"="#58a6ff","Power Play"="#3fb950","Penalty Kill"="#f85149"),name=NULL)+
      scale_y_continuous(limits=c(0,70))+coord_flip()+labs(x=NULL,y="xGF%")+theme_minimal()+
      theme(plot.background=element_rect(fill="#161b22",color=NA),
            panel.background=element_rect(fill="#161b22",color=NA),
            panel.grid.major=element_line(color="#21262d"),panel.grid.minor=element_blank(),
            axis.text=element_text(color="#e6edf3",size=9),axis.title=element_text(color="#e6edf3"),
            legend.background=element_rect(fill="#161b22",color=NA),legend.text=element_text(color="#e6edf3"))
    ggplotly(p,tooltip="text")|>layout(paper_bgcolor="#161b22",plot_bgcolor="#161b22",
                                       font=list(color="#e6edf3"),legend=list(font=list(color="#e6edf3"),bgcolor="#161b22"))
  })
  
  # GAME EXPLORER — live timer polls every 30 seconds
  live_timer          <- reactiveTimer(30000)
  selected_live_game_id <- reactiveVal(NULL)
  
  live_games_data <- reactive({
    input$refresh_live
    tryCatch({
      live  <- nhl_get("https://api-web.nhle.com/v1/scoreboard/now")
      purrr::map_dfr(live$gamesByDate, function(day) {
        purrr::map_dfr(day$games, function(g) {
          data.frame(
            game_id    = g$id,    game_date  = g$gameDate,
            state      = g$gameState,
            away_team  = g$awayTeam$abbrev,
            away_score = g$awayTeam$score %||% NA,
            home_team  = g$homeTeam$abbrev,
            home_score = g$homeTeam$score %||% NA,
            period     = g$period %||% NA,
            stringsAsFactors=FALSE
          )
        })
      })
    }, error=function(e) NULL)
  })
  
  # Wire live game select buttons
  observe({
    games <- live_games_data()
    if (is.null(games)) return()
    lapply(1:nrow(games), function(i) {
      local({
        gid    <- games$game_id[i]
        btn_id <- paste0("live_select_", gid)
        observeEvent(input[[btn_id]], { selected_live_game_id(gid) }, ignoreNULL=TRUE)
      })
    })
  })
  
  # Live play-by-play — polls every 30s when a live game is selected
  live_pbp_data <- reactive({
    live_timer()
    gid <- selected_live_game_id()
    if (is.null(gid)) return(NULL)
    tryCatch(
      nhl_get(paste0("https://api-web.nhle.com/v1/gamecenter/", gid, "/play-by-play")),
      error=function(e) NULL
    )
  })
  
  # Parse and score live shots
  live_shots_data <- reactive({
    pbp <- live_pbp_data()
    if (is.null(pbp)) return(NULL)
    shots_raw <- parse_live_shots(pbp)
    if (is.null(shots_raw) || nrow(shots_raw)==0) return(NULL)
    non_empty <- shots_raw|>filter(empty_net==0,
                                   !is.na(distance),!is.na(angle),!is.na(shot_type),
                                   !is.na(is_rebound),!is.na(is_rush),!is.na(strength_state),!is.na(overtime))
    if (nrow(non_empty)>0)
      non_empty$xg <- tryCatch(predict(model,newdata=non_empty,type="response"),
                               error=function(e) rep(0.05,nrow(non_empty)))
    empty   <- shots_raw|>filter(empty_net==1)|>mutate(xg=0.85)
    skipped <- shots_raw|>filter(empty_net==0,is.na(distance)|is.na(angle)|is.na(shot_type))|>mutate(xg=0.05)
    bind_rows(non_empty,empty,skipped)|>arrange(period,time_seconds)|>
      group_by(shooting_team)|>mutate(cumxg=cumsum(xg))|>ungroup()
  })
  
  output$live_games_ui <- renderUI({
    games   <- live_games_data()
    sel_gid <- selected_live_game_id()
    if (is.null(games)||nrow(games)==0)
      return(div(style="color:#8b949e;padding:20px;text-align:center;","No games currently scheduled."))
    
    game_cards <- lapply(1:nrow(games), function(i) {
      g   <- games[i,]
      gid <- g$game_id
      state_color <- switch(g$state,"LIVE"="#3fb950","CRIT"="#f85149","OFF"="#8b949e","FUT"="#58a6ff","#58a6ff")
      state_label <- switch(g$state,
                            "LIVE"=paste0("LIVE — P",g$period),"CRIT"=paste0("LIVE — P",g$period),
                            "OFF"="Final","FUT"="Upcoming",g$state)
      
      # Prediction
      away_prof <- season_profiles|>filter(shooting_team==g$away_team)
      home_prof <- season_profiles|>filter(shooting_team==g$home_team)
      pred_section <- if (nrow(away_prof)>0 && nrow(home_prof)>0) {
        away_pred <- round((away_prof$xgf_game + home_prof$xga_game)/2, 2)
        home_pred <- round((home_prof$xgf_game + away_prof$xga_game)/2, 2)
        pred_winner <- ifelse(away_pred>home_pred, g$away_team, g$home_team)
        div(style="margin-top:8px;padding-top:8px;border-top:1px solid #30363d;",
            div(style="color:#8b949e;font-size:10px;text-transform:uppercase;letter-spacing:0.5px;margin-bottom:4px;","xG Prediction"),
            div(style="color:#e6edf3;font-size:13px;font-weight:600;",
                paste0(g$away_team," ",away_pred," — ",home_pred," ",g$home_team)),
            div(style="color:#f0a500;font-size:11px;margin-top:2px;",paste0("Favored: ",pred_winner))
        )
      } else NULL
      
      is_selected  <- !is.null(sel_gid) && sel_gid==gid
      card_border  <- if (is_selected) "2px solid #3fb950" else "1px solid #30363d"
      
      div(style=paste0("display:inline-block;background:#21262d;border:",card_border,";",
                       "border-radius:6px;padding:12px 16px;margin:6px;min-width:220px;vertical-align:top;"),
          div(style=paste0("color:",state_color,";font-size:11px;font-weight:600;",
                           "text-transform:uppercase;margin-bottom:8px;"), state_label),
          div(style="display:flex;align-items:center;gap:12px;",
              div(style="text-align:center;",
                  img(src=paste0("https://assets.nhle.com/logos/nhl/svg/",g$away_team,"_light.svg"),height="36px"),
                  div(style="color:#e6edf3;font-size:11px;margin-top:4px;",g$away_team)
              ),
              div(style="color:#e6edf3;font-size:22px;font-weight:700;",
                  if (g$state %in% c("LIVE","CRIT","OFF")) paste0(g$away_score," — ",g$home_score) else "vs"),
              div(style="text-align:center;",
                  img(src=paste0("https://assets.nhle.com/logos/nhl/svg/",g$home_team,"_light.svg"),height="36px"),
                  div(style="color:#e6edf3;font-size:11px;margin-top:4px;",g$home_team)
              )
          ),
          pred_section,
          div(style="margin-top:8px;text-align:center;",
              if (g$state %in% c("LIVE","CRIT")) {
                tags$button("Track Live xG", class="game-card-btn",
                            style="background:#238636;",
                            onclick=paste0("Shiny.setInputValue('live_select_",gid,"',true,{priority:'event'});",
                                           "Shiny.setInputValue('selected_game_id',",gid,",{priority:'event'})"))
              } else if (g$state=="OFF") {
                tags$button("View xG Recap", class="game-card-btn",
                            onclick=paste0("Shiny.setInputValue('selected_game_id',",gid,",{priority:'event'})"))
              } else {
                div(style="color:#8b949e;font-size:11px;margin-top:4px;","Prediction shown above")
              }
          )
      )
    })
    
    # Live xG tracker
    live_tracker <- if (!is.null(sel_gid)) {
      live_data <- isolate(live_shots_data())
      if (!is.null(live_data) && nrow(live_data)>0) {
        pbp  <- isolate(live_pbp_data())
        away <- live_data$away_team[1]; home <- live_data$home_team[1]
        away_xg  <- round(sum(live_data$xg[live_data$shooting_team==away],na.rm=TRUE),2)
        home_xg  <- round(sum(live_data$xg[live_data$shooting_team==home],na.rm=TRUE),2)
        away_g   <- sum(live_data$goal[live_data$shooting_team==away])
        home_g   <- sum(live_data$goal[live_data$shooting_team==home])
        away_sog <- sum(live_data$event_type[live_data$shooting_team==away]=="shot-on-goal") + away_g
        home_sog <- sum(live_data$event_type[live_data$shooting_team==home]=="shot-on-goal") + home_g
        xg_leader    <- ifelse(away_xg>home_xg, away, home)
        score_leader <- ifelse(away_g>home_g, away, ifelse(home_g>away_g, home, "Tied"))
        clock_text   <- if (!is.null(pbp$clock))
          paste0("P",pbp$displayPeriod," — ",pbp$clock$timeRemaining," remaining") else "Live"
        verdict <- if (xg_leader!=score_leader && score_leader!="Tied")
          paste0("⚡ ",score_leader," leading despite lower xG,",xg_leader," generating better chances")
        else if (score_leader=="Tied")
          paste0(xg_leader," winning the xG battle, expect them to pull ahead")
        else
          paste0(xg_leader," controlling the game,  score reflects shot quality")
        
        div(style="margin-top:16px;background:#0d1f15;border:1px solid #238636;border-radius:6px;padding:16px;",
            div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:12px;",
                div(style="color:#3fb950;font-size:13px;font-weight:600;",paste0("LIVE xG TRACKER,  ",clock_text)),
                div(style="color:#8b949e;font-size:11px;",paste0("Updates every 30s | ",nrow(live_data)," shots tracked"))
            ),
            div(style="display:flex;gap:20px;align-items:center;margin-bottom:12px;",
                div(style="text-align:center;flex:1;",
                    img(src=paste0("https://assets.nhle.com/logos/nhl/svg/",away,"_light.svg"),height="40px"),
                    div(style="color:#e6edf3;font-size:20px;font-weight:700;margin-top:4px;",away_g),
                    div(style="color:#58a6ff;font-size:13px;",paste0(away_xg," xG")),
                    div(style="color:#8b949e;font-size:11px;",paste0(away_sog," SOG")),
                    div(style="color:#8b949e;font-size:11px;",away)
                ),
                div(style="text-align:center;",div(style="color:#8b949e;font-size:16px;font-weight:700;","vs")),
                div(style="text-align:center;flex:1;",
                    img(src=paste0("https://assets.nhle.com/logos/nhl/svg/",home,"_light.svg"),height="40px"),
                    div(style="color:#e6edf3;font-size:20px;font-weight:700;margin-top:4px;",home_g),
                    div(style="color:#3fb950;font-size:13px;",paste0(home_xg," xG")),
                    div(style="color:#8b949e;font-size:11px;",paste0(home_sog," SOG")),
                    div(style="color:#8b949e;font-size:11px;",home)
                )
            ),
            div(style="background:#21262d;border-left:4px solid #f0a500;padding:8px 12px;border-radius:4px;margin-bottom:12px;",
                p(style="color:#f0a500;font-weight:600;font-size:11px;margin:0 0 2px;","GAME VERDICT"),
                p(style="color:#e6edf3;font-size:12px;margin:0;",verdict)
            ),
            plotlyOutput("plot_live_timeline", height="250px")
        )
      }
    } else NULL
    
    tagList(do.call(div, game_cards), live_tracker)
  })
  
  output$plot_live_timeline <- renderPlotly({
    live_data <- live_shots_data()
    if (is.null(live_data)||nrow(live_data)==0) return(NULL)
    away <- live_data$away_team[1]; home <- live_data$home_team[1]
    live_data <- live_data|>mutate(total_seconds=(period-1)*1200+time_seconds)
    away_df <- live_data|>filter(shooting_team==away)
    home_df <- live_data|>filter(shooting_team==home)
    plot_ly()|>
      add_lines(data=away_df,x=~total_seconds,y=~cumxg,name=away,line=list(color="#58a6ff",width=2))|>
      add_lines(data=home_df,x=~total_seconds,y=~cumxg,name=home,line=list(color="#3fb950",width=2))|>
      add_markers(data=live_data|>filter(goal==1,shooting_team==away),
                  x=~total_seconds,y=~cumxg,name=paste(away,"Goal"),
                  marker=list(color="#f0a500",size=12,symbol="star"))|>
      add_markers(data=live_data|>filter(goal==1,shooting_team==home),
                  x=~total_seconds,y=~cumxg,name=paste(home,"Goal"),
                  marker=list(color="#f0a500",size=12,symbol="star"))|>
      layout(paper_bgcolor="#0d1f15",plot_bgcolor="#0d1f15",font=list(color="#e6edf3"),
             xaxis=list(title="Game Time",gridcolor="#21262d",
                        tickvals=c(0,1200,2400,3600),ticktext=c("P1","P2","P3","OT")),
             yaxis=list(title="Cumulative xG",gridcolor="#21262d"),
             legend=list(font=list(color="#e6edf3"),bgcolor="#0d1f15"),margin=list(t=10))
  })
  
  # Historical game table
  historical_games <- reactive({
    data <- shots|>filter(game_type==input$game_type_filter, season==input$game_season)
    if (input$game_team_filter!="all")
      data <- data|>filter(shooting_team==input$game_team_filter|defending_team==input$game_team_filter)
    data|>group_by(game_id,game_date,home_team,away_team)|>
      summarise(
        away_xgf  =round(sum(xg[shooting_team==first(away_team)],na.rm=TRUE),2),
        home_xgf  =round(sum(xg[shooting_team==first(home_team)],na.rm=TRUE),2),
        away_goals=sum(goal[shooting_team==first(away_team)]),
        home_goals=sum(goal[shooting_team==first(home_team)]),
        .groups="drop"
      )|>mutate(
        xg_winner    =ifelse(away_xgf>home_xgf,away_team,home_team),
        actual_winner=ifelse(away_goals>home_goals,away_team,home_team),
        upset        =xg_winner!=actual_winner
      )|>arrange(desc(game_date))
  })
  
  output$table_game_select <- renderDT({
    df <- historical_games()|>
      mutate(Matchup=paste0(away_team," @ ",home_team),
             Score=paste0(away_goals,"-",home_goals),
             `Away xG`=away_xgf,`Home xG`=home_xgf,
             `xG Winner`=xg_winner,`Actual Winner`=actual_winner,
             Upset=ifelse(upset,"⚡ Yes","No"))|>
      dplyr::select(Date=game_date,Matchup,Score,`Away xG`,`Home xG`,`xG Winner`,`Actual Winner`,Upset,game_id)
    datatable(df,rownames=FALSE,selection="single",
              options=list(pageLength=15,scrollY="300px",
                           columnDefs=list(list(visible=FALSE,targets=8))),
              callback=JS("table.on('click.dt','tr',function(){
        var data=table.row(this).data();
        if(data) Shiny.setInputValue('selected_game_id',data[8],{priority:'event'});
      })"))
  })
  
  selected_game <- reactive({
    req(input$selected_game_id)
    shots|>filter(game_id==input$selected_game_id)
  })
  
  output$game_recap_header <- renderUI({
    req(input$selected_game_id)
    g <- selected_game(); if (nrow(g)==0) return(NULL)
    away <- g$away_team[1]; home <- g$home_team[1]
    away_xg <- round(sum(g$xg[g$shooting_team==away],na.rm=TRUE),2)
    home_xg <- round(sum(g$xg[g$shooting_team==home],na.rm=TRUE),2)
    away_g  <- sum(g$goal[g$shooting_team==away])
    home_g  <- sum(g$goal[g$shooting_team==home])
    xg_win  <- ifelse(away_xg>home_xg,away,home)
    act_win <- ifelse(away_g>home_g,away,home)
    div(style="padding:12px;",
        div(style="display:flex;align-items:center;gap:20px;margin-bottom:12px;",
            div(style="text-align:center;",
                img(src=paste0("https://assets.nhle.com/logos/nhl/svg/",away,"_light.svg"),height="60px"),
                div(style="color:#e6edf3;font-size:24px;font-weight:700;",paste0(away_g," (",away_xg," xG)")),
                div(style="color:#8b949e;font-size:12px;",away)
            ),
            div(style="color:#8b949e;font-size:20px;font-weight:700;","vs"),
            div(style="text-align:center;",
                img(src=paste0("https://assets.nhle.com/logos/nhl/svg/",home,"_light.svg"),height="60px"),
                div(style="color:#e6edf3;font-size:24px;font-weight:700;",paste0(home_g," (",home_xg," xG)")),
                div(style="color:#8b949e;font-size:12px;",home)
            ),
            if (xg_win!=act_win)
              div(style="background:#f85149;color:#fff;padding:8px 12px;border-radius:4px;font-size:12px;font-weight:600;",
                  paste0("⚡ UPSET,  ",act_win," won despite lower xG"))
            else
              div(style="background:#238636;color:#fff;padding:8px 12px;border-radius:4px;font-size:12px;font-weight:600;",
                  paste0("✓ ",act_win," won as expected by xG"))
        )
    )
  })
  
  output$plot_game_xg_bar <- renderPlot({
    req(input$selected_game_id)
    g <- selected_game(); if (nrow(g)==0) return(NULL)
    away <- g$away_team[1]; home <- g$home_team[1]
    df <- data.frame(
      team =c(away,home),
      xg   =c(sum(g$xg[g$shooting_team==away],na.rm=TRUE),sum(g$xg[g$shooting_team==home],na.rm=TRUE)),
      goals=c(sum(g$goal[g$shooting_team==away]),sum(g$goal[g$shooting_team==home]))
    )
    ggplot(df,aes(x=team,y=xg,fill=team))+
      geom_col(width=0.5,alpha=0.8)+
      geom_point(aes(y=goals),size=5,color="#f0a500",shape=18)+
      geom_text(aes(y=goals,label=paste0(goals," G")),vjust=-1,color="#f0a500",size=4,fontface="bold")+
      scale_fill_manual(values=c("#58a6ff","#3fb950"),guide="none")+
      labs(x=NULL,y="Expected Goals",caption="Bars = xG | Gold diamond = Actual Goals")+scatter_theme()
  }, bg="#0d1117")
  
  output$plot_game_timeline <- renderPlotly({
    req(input$selected_game_id)
    g <- selected_game(); if (nrow(g)==0) return(NULL)
    away <- g$away_team[1]; home <- g$home_team[1]
    g <- g|>arrange(period,time_seconds)|>
      mutate(total_seconds=(period-1)*1200+time_seconds)|>
      group_by(shooting_team)|>mutate(cumxg=cumsum(xg))|>ungroup()
    away_df <- g|>filter(shooting_team==away); home_df <- g|>filter(shooting_team==home)
    plot_ly()|>
      add_lines(data=away_df,x=~total_seconds,y=~cumxg,name=away,line=list(color="#58a6ff"))|>
      add_lines(data=home_df,x=~total_seconds,y=~cumxg,name=home,line=list(color="#3fb950"))|>
      add_markers(data=g|>filter(goal==1,shooting_team==away),x=~total_seconds,y=~cumxg,
                  name=paste(away,"Goal"),marker=list(color="#f0a500",size=10,symbol="star"))|>
      add_markers(data=g|>filter(goal==1,shooting_team==home),x=~total_seconds,y=~cumxg,
                  name=paste(home,"Goal"),marker=list(color="#f0a500",size=10,symbol="star"))|>
      layout(paper_bgcolor="#161b22",plot_bgcolor="#161b22",font=list(color="#e6edf3"),
             xaxis=list(title="Game Time",gridcolor="#21262d",
                        tickvals=c(0,1200,2400,3600),ticktext=c("P1","P2","P3","OT")),
             yaxis=list(title="Cumulative xG",gridcolor="#21262d"),
             legend=list(font=list(color="#e6edf3"),bgcolor="#161b22"))
  })
  
  # TEAM SPOTLIGHT
  team_filtered_data <- reactive({
    data <- shots
    if (input$team_season!="all") data <- data|>filter(season==input$team_season)
    if (input$team_gametype!="both") data <- data|>filter(game_type==input$team_gametype)
    data|>filter(strength_state==input$team_situation)
  })
  
  team_zscores <- reactive({
    data  <- team_filtered_data()
    norms <- get_league_norms(data)
    prof  <- get_team_profile(data, input$team_selected)
    if (is.null(prof)) return(NULL)
    data.frame(
      metric =c("Volume","Quality","Distance","Rebound","Rush"),
      z_score=c(
        (prof$shots_per_game - norms$shots_per_game_mean) / norms$shots_per_game_sd,
        (prof$xg_per_shot    - norms$xg_per_shot_mean)    / norms$xg_per_shot_sd,
        -(prof$avg_distance   - norms$avg_distance_mean)   / norms$avg_distance_sd,
        (prof$rebound_pct    - norms$rebound_pct_mean)    / norms$rebound_pct_sd,
        (prof$rush_pct       - norms$rush_pct_mean)       / norms$rush_pct_sd
      )
    )
  })
  
  output$team_league_baseline <- renderUI({
    data  <- team_filtered_data()
    norms <- get_league_norms(data)
    prof  <- get_team_profile(data, input$team_selected)
    team  <- input$team_selected
    
    stat_row <- function(label, league_val, team_val, team_name, higher_better=TRUE) {
      diff_color <- if ((team_val > league_val) == higher_better) "#3fb950" else "#f85149"
      div(class="metric-box",
          tags$strong(label),
          div(style="display:flex;justify-content:space-between;align-items:baseline;margin-top:6px;",
              div(
                div(style="color:#8b949e;font-size:10px;text-transform:uppercase;letter-spacing:0.5px;","League"),
                div(style="color:#e6edf3;font-size:16px;font-weight:600;",league_val)
              ),
              div(style="color:#30363d;font-size:18px;","→"),
              div(
                div(style=paste0("color:",diff_color,";font-size:10px;text-transform:uppercase;letter-spacing:0.5px;"),team_name),
                div(style=paste0("color:",diff_color,";font-size:16px;font-weight:700;"),team_val)
              )
          )
      )
    }
    
    if (is.null(prof)) {
      div(style="color:#8b949e;","No data for this selection.")
    } else {
      div(style="display:flex;gap:10px;flex-wrap:wrap;",
          stat_row("Shots / Game",
                   round(norms$shots_per_game_mean,1),
                   round(prof$shots_per_game,1),
                   team, higher_better=TRUE),
          stat_row("xG / Shot",
                   round(norms$xg_per_shot_mean,4),
                   round(prof$xg_per_shot,4),
                   team, higher_better=TRUE),
          stat_row("Avg Distance",
                   paste0(round(norms$avg_distance_mean,1),"ft"),
                   paste0(round(prof$avg_distance,1),"ft"),
                   team, higher_better=FALSE),
          stat_row("Rebound %",
                   paste0(round(norms$rebound_pct_mean,1),"%"),
                   paste0(round(prof$rebound_pct,1),"%"),
                   team, higher_better=TRUE),
          stat_row("Rush %",
                   paste0(round(norms$rush_pct_mean,1),"%"),
                   paste0(round(prof$rush_pct,1),"%"),
                   team, higher_better=TRUE)
      )
    }
  })
  
  output$team_narrative <- renderUI({
    z <- team_zscores()
    if (is.null(z)) return(div("No data available for this selection."))
    z_list    <- setNames(z$z_score, z$metric)
    narrative <- describe_team(input$team_selected,
                               z_volume=z_list["Volume"], z_quality=z_list["Quality"],
                               z_distance=z_list["Distance"], z_rebound=z_list["Rebound"], z_rush=z_list["Rush"])
    parts <- strsplit(narrative,"\n\n")[[1]]
    div(
      p(style="color:#e6edf3;font-size:13px;line-height:1.8;",parts[1])
    )
  })
  
  output$plot_team_heatmap <- renderPlot({
    plot_heatmap(team_filtered_data(), input$team_selected,
                 paste0(input$team_selected,",  Shot Density"))
  }, bg="#0d1117")
  
  output$plot_team_danger <- renderPlotly({
    data <- team_filtered_data()|>
      filter(shooting_team==input$team_selected,empty_net==0,!is.na(distance))|>
      mutate(danger_zone=case_when(distance<20~"High Danger",distance<40~"Mid Danger",TRUE~"Low Danger"))|>
      group_by(danger_zone)|>
      summarise(shots=n(),goals=sum(goal),avg_xg=round(mean(xg,na.rm=TRUE),4),.groups="drop")|>
      mutate(pct=round(shots/sum(shots)*100,1),
             color=case_when(danger_zone=="High Danger"~"#f85149",
                             danger_zone=="Mid Danger"~"#f0a500",TRUE~"#58a6ff"))
    plot_ly(data,labels=~danger_zone,values=~shots,type="pie",hole=0.5,
            marker=list(colors=data$color,line=list(color="#0d1117",width=2)),
            textinfo="label+percent",textfont=list(color="#e6edf3",size=12),
            hovertemplate=paste0("<b>%{label}</b><br>Shots: %{value}<br>Goals: ",data$goals,
                                 "<br>Avg xG: ",data$avg_xg,"<extra></extra>"))|>
      layout(paper_bgcolor="#161b22",font=list(color="#e6edf3"),
             legend=list(font=list(color="#e6edf3"),bgcolor="#161b22"),
             annotations=list(list(text=paste0(input$team_selected,"<br>Shot Mix"),
                                   x=0.5,y=0.5,showarrow=FALSE,
                                   font=list(color="#e6edf3",size=13))))
  })
  
  output$plot_team_zscore <- renderPlotly({
    z <- team_zscores(); if (is.null(z)) return(NULL)
    z <- z|>mutate(color=ifelse(z_score>=0,"#3fb950","#f85149"),
                   label=paste0(metric,": ",round(z_score,2)," SD"))
    p <- ggplot(z,aes(x=reorder(metric,z_score),y=z_score,fill=color,text=label))+
      geom_col(width=0.6)+
      geom_hline(yintercept=0,color="#e6edf3",linewidth=0.5)+
      scale_fill_identity()+coord_flip()+
      labs(x=NULL,y="Standard Deviations from League Average",
           title=paste0(input$team_selected," vs League Average"))+
      theme_minimal()+
      theme(plot.background=element_rect(fill="#161b22",color=NA),
            panel.background=element_rect(fill="#161b22",color=NA),
            panel.grid.major=element_line(color="#21262d"),panel.grid.minor=element_blank(),
            axis.text=element_text(color="#e6edf3",size=10),
            axis.title=element_text(color="#8b949e",size=9),
            plot.title=element_text(color="#e6edf3",size=12,face="bold"))
    ggplotly(p,tooltip="text")|>
      layout(paper_bgcolor="#161b22",plot_bgcolor="#161b22",font=list(color="#e6edf3"))
  })
  
  output$plot_team_homeaway <- renderPlotly({
    data <- team_filtered_data()|>
      filter(shooting_team==input$team_selected,empty_net==0)|>
      mutate(location=ifelse(shooting_team==home_team,"Home","Away"))|>
      group_by(location,game_id)|>
      summarise(shots=n(),xg=sum(xg,na.rm=TRUE),.groups="drop")|>
      group_by(location)|>
      summarise(shots_per_game=round(mean(shots),1),xg_per_game=round(mean(xg),2),
                xg_per_shot=round(mean(xg/shots),4),.groups="drop")|>
      pivot_longer(cols=c(shots_per_game,xg_per_game,xg_per_shot),
                   names_to="metric",values_to="value")|>
      mutate(metric=case_when(metric=="shots_per_game"~"Shots/Game",
                              metric=="xg_per_game"~"xG/Game",TRUE~"xG/Shot"))
    p <- ggplot(data,aes(x=metric,y=value,fill=location,
                         text=paste0(location,",  ",metric,": ",value)))+
      geom_col(position="dodge",width=0.6)+
      scale_fill_manual(values=c("Home"="#3fb950","Away"="#58a6ff"),name=NULL)+
      facet_wrap(~metric,scales="free_y",nrow=1)+labs(x=NULL,y=NULL)+theme_minimal()+
      theme(plot.background=element_rect(fill="#161b22",color=NA),
            panel.background=element_rect(fill="#161b22",color=NA),
            panel.grid.major=element_line(color="#21262d"),panel.grid.minor=element_blank(),
            axis.text=element_text(color="#e6edf3",size=9),
            strip.text=element_text(color="#e6edf3",size=10),
            legend.text=element_text(color="#e6edf3"),
            legend.background=element_rect(fill="#161b22",color=NA),
            axis.text.x=element_blank())
    ggplotly(p,tooltip="text")|>
      layout(paper_bgcolor="#161b22",plot_bgcolor="#161b22",
             font=list(color="#e6edf3"),
             legend=list(font=list(color="#e6edf3"),bgcolor="#161b22"))
  })
  
  output$plot_h2h_heatmap1 <- renderPlot({
    plot_heatmap(team_filtered_data(),input$h2h_team1,paste0(input$h2h_team1,",  Shot Density"))
  }, bg="#0d1117")
  
  output$plot_h2h_heatmap2 <- renderPlot({
    plot_heatmap(team_filtered_data(),input$h2h_team2,paste0(input$h2h_team2,",  Shot Density"))
  }, bg="#0d1117")
  
  output$h2h_narrative <- renderUI({
    data  <- team_filtered_data()
    norms <- get_league_norms(data)
    
    get_profile_and_z <- function(team) {
      prof <- get_team_profile(data, team)
      if (is.null(prof)) return(NULL)
      list(
        prof = prof,
        volume   = (prof$shots_per_game - norms$shots_per_game_mean) / norms$shots_per_game_sd,
        quality  = (prof$xg_per_shot    - norms$xg_per_shot_mean)    / norms$xg_per_shot_sd,
        distance = -(prof$avg_distance  - norms$avg_distance_mean)   / norms$avg_distance_sd,
        rebound  = (prof$rebound_pct    - norms$rebound_pct_mean)    / norms$rebound_pct_sd,
        rush     = (prof$rush_pct       - norms$rush_pct_mean)       / norms$rush_pct_sd
      )
    }
    
    p1 <- get_profile_and_z(input$h2h_team1)
    p2 <- get_profile_and_z(input$h2h_team2)
    if (is.null(p1) || is.null(p2)) return(NULL)
    
    t1 <- input$h2h_team1; t2 <- input$h2h_team2
    
    edge_label <- function(z_diff, winner, metric_label, unit1, val1, val2) {
      strength <- if (abs(z_diff) > 2) "large" else if (abs(z_diff) > 1) "clear" else "slight"
      color <- switch(strength, large="#f85149", clear="#f0a500", slight="#58a6ff")
      div(style="margin-bottom:12px;padding:8px 10px;background:#161b22;border-radius:4px;",
          div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:4px;",
              tags$strong(style="color:#e6edf3;font-size:12px;", metric_label),
              span(style=paste0("color:",color,";font-size:10px;font-weight:700;text-transform:uppercase;"),
                   paste0(strength, " edge: ", winner))
          ),
          div(style="display:flex;justify-content:space-between;",
              div(style="text-align:center;flex:1;",
                  div(style="color:#8b949e;font-size:10px;", t1),
                  div(style="color:#e6edf3;font-size:13px;font-weight:600;", val1)),
              div(style="text-align:center;flex:1;",
                  div(style="color:#8b949e;font-size:10px;", t2),
                  div(style="color:#e6edf3;font-size:13px;font-weight:600;", val2))
          )
      )
    }
    
    vol_winner  <- ifelse(p1$volume  > p2$volume,  t1, t2)
    qual_winner <- ifelse(p1$quality > p2$quality, t1, t2)
    reb_winner  <- ifelse(p1$rebound > p2$rebound, t1, t2)
    dist_winner <- ifelse(p1$distance> p2$distance,t1, t2)
    
    matchup <- if (vol_winner == qual_winner)
      paste0(vol_winner, " generates more shots AND higher quality chances. Clear xG advantage going in.")
    else
      paste0(vol_winner, " generates more shots per game. ",
             qual_winner, " generates higher quality looks per shot. ",
             "Volume vs efficiency -- which matters more often comes down to goaltending on the night.")
    
    div(style="padding:8px;",
        div(style="color:#8b949e;font-size:10px;margin-bottom:10px;line-height:1.5;",
            "Each edge shows which team has the advantage on that metric and by how much. ",
            tags$em("Large edge"), " means a significant real-world difference, not just noise."),
        
        edge_label(p1$volume - p2$volume, vol_winner,
                   "Shots Generated per Game",
                   "shots/gm",
                   paste0(round(p1$prof$shots_per_game,1), " shots"),
                   paste0(round(p2$prof$shots_per_game,1), " shots")),
        
        edge_label(p1$quality - p2$quality, qual_winner,
                   "Shot Quality (xG per Shot)",
                   "xG/shot",
                   round(p1$prof$xg_per_shot, 4),
                   round(p2$prof$xg_per_shot, 4)),
        
        edge_label(p1$rebound - p2$rebound, reb_winner,
                   "Rebound Generation",
                   "%",
                   paste0(round(p1$prof$rebound_pct,1), "%"),
                   paste0(round(p2$prof$rebound_pct,1), "%")),
        
        edge_label(p1$distance - p2$distance, dist_winner,
                   "Shot Positioning (closer = better)",
                   "ft",
                   paste0(round(p1$prof$avg_distance,1), "ft"),
                   paste0(round(p2$prof$avg_distance,1), "ft")),
        
        div(style="background:#21262d;border-left:4px solid #f0a500;padding:8px 12px;border-radius:4px;margin-top:4px;",
            p(style="color:#f0a500;font-weight:600;font-size:11px;margin:0 0 4px;","MATCHUP VERDICT"),
            p(style="color:#e6edf3;font-size:12px;margin:0;line-height:1.6;", matchup))
    )
  })
  
  # GOALIE TAB (work in progress)
  
  goalie_data_filtered <- reactive({
    d <- shots |>
      filter(!is.na(goalie_id), empty_net == 0)
    if (input$goalie_gametype != "both")
      d <- d |> filter(game_type == input$goalie_gametype)
    if (input$goalie_season != "all")
      d <- d |> filter(season == input$goalie_season)
    d
  })
  
  goalie_stats <- reactive({
    d <- goalie_data_filtered()
    d |>
      group_by(goalie_id) |>
      summarise(
        shots_faced   = n(),
        goals_allowed = sum(goal),
        xga           = round(sum(xg, na.rm=TRUE), 2),
        gsax          = round(sum(goal) - sum(xg, na.rm=TRUE), 2),
        sv_pct        = round(1 - mean(goal), 4),
        toi_est       = shots_faced / 30 * 60,  # rough TOI estimate in seconds
        .groups       = "drop"
      ) |>
      filter(shots_faced >= input$goalie_min_shots) |>
      mutate(gsax_60 = round(gsax / (toi_est / 3600), 2)) |>
      left_join(players |> dplyr::select(player_id, full_name, team, headshot),
                by = c("goalie_id" = "player_id")) |>
      mutate(
        full_name = ifelse(is.na(full_name), paste("Goalie", goalie_id), full_name),
        team      = ifelse(is.na(team), "---", team)
      ) |>
      arrange(gsax)
  })
  
  selected_goalie_id <- reactiveVal(NULL)
  
  output$table_goalie_leaderboard <- renderDT({
    df <- goalie_stats() |>
      mutate(
        Rank     = row_number(),
        Name     = full_name,
        Team     = team,
        Shots    = shots_faced,
        `Sv%`    = sprintf("%.3f", sv_pct),
        xGA      = sprintf("%.1f", xga),
        GSAx     = sprintf("%.1f", gsax),
        `GSAx/60`= sprintf("%.2f", gsax_60)
      ) |>
      dplyr::select(Rank, Name, Team, Shots, `Sv%`, xGA, GSAx, `GSAx/60`)
    
    datatable(df,
              selection  = "single",
              rownames   = FALSE,
              options    = list(
                pageLength = 15,
                order      = list(list(6, "asc")),
                dom        = "ftip",
                columnDefs = list(list(className="dt-center", targets=c(0,2,3,4,5,6,7)))
              )
    ) |>
      formatStyle("GSAx",
                  color = styleInterval(0, c("#3fb950", "#f85149")),
                  fontWeight = "bold"
      ) |>
      formatStyle(0:7,
                  backgroundColor = "#161b22",
                  color           = "#e6edf3"
      )
  })
  
  observeEvent(input$table_goalie_leaderboard_rows_selected, {
    idx <- input$table_goalie_leaderboard_rows_selected
    if (!is.null(idx)) {
      gid <- goalie_stats()$goalie_id[idx]
      selected_goalie_id(gid)
    }
  })
  
  # Auto-select top goalie on load
  observe({
    gs <- goalie_stats()
    if (is.null(selected_goalie_id()) && nrow(gs) > 0)
      selected_goalie_id(gs$goalie_id[1])
  })
  
  selected_goalie_row <- reactive({
    gid <- selected_goalie_id()
    if (is.null(gid)) return(NULL)
    goalie_stats() |> filter(goalie_id == gid)
  })
  
  output$goalie_profile_header <- renderUI({
    g <- selected_goalie_row()
    if (is.null(g) || nrow(g) == 0) return(NULL)
    div(style="display:flex;align-items:center;gap:12px;padding:8px 0 12px;",
        img(src=g$headshot[1], height="50px",
            style="border-radius:50%;border:2px solid #58a6ff;"),
        div(
          div(style="color:#e6edf3;font-size:16px;font-weight:700;", g$full_name[1]),
          div(style="color:#8b949e;font-size:12px;",
              paste0(g$team[1], " | ", g$shots_faced[1], " shots | GSAx: ", g$gsax[1]))
        )
    )
  })
  
  output$plot_goalie_season <- renderPlotly({
    gid <- selected_goalie_id()
    if (is.null(gid)) return(NULL)
    
    d <- shots |>
      filter(goalie_id == gid, empty_net == 0)
    if (input$goalie_gametype != "both")
      d <- d |> filter(game_type == input$goalie_gametype)
    
    season_stats <- d |>
      group_by(season) |>
      summarise(
        shots  = n(),
        gsax   = round(sum(goal) - sum(xg, na.rm=TRUE), 2),
        sv_pct = round(1 - mean(goal), 4),
        .groups= "drop"
      ) |>
      mutate(
        season_label = case_when(
          season == "20192020" ~ "2019-20",
          season == "20202021" ~ "2020-21",
          season == "20212022" ~ "2021-22",
          season == "20222023" ~ "2022-23",
          season == "20232024" ~ "2023-24",
          season == "20242025" ~ "2024-25",
          season == "20252026" ~ "2025-26",
          TRUE ~ season
        ),
        bar_color = ifelse(gsax < 0, "#3fb950", "#f85149"),
        tip = paste0(season_label, "<br>GSAx: ", gsax,
                     "<br>Sv%: ", sv_pct,
                     "<br>Shots: ", shots)
      )
    
    plot_ly(season_stats, x=~season_label, y=~gsax, type="bar",
            marker = list(color=~bar_color),
            text   = ~tip, hoverinfo="text"
    ) |>
      layout(
        paper_bgcolor = "#161b22", plot_bgcolor = "#161b22",
        font    = list(color="#e6edf3"),
        xaxis   = list(title="Season", gridcolor="#30363d"),
        yaxis   = list(title="GSAx (negative = better)", gridcolor="#30363d",
                       zeroline=TRUE, zerolinecolor="#58a6ff", zerolinewidth=2),
        showlegend = FALSE
      )
  })
  
  output$plot_goalie_zones <- renderPlotly({
    gid <- selected_goalie_id()
    if (is.null(gid)) return(NULL)
    
    d <- shots |>
      filter(goalie_id == gid, empty_net == 0)
    if (input$goalie_gametype != "both")
      d <- d |> filter(game_type == input$goalie_gametype)
    if (input$goalie_season != "all")
      d <- d |> filter(season == input$goalie_season)
    
    zone_stats <- d |>
      mutate(danger = case_when(
        distance <= 20 ~ "High Danger",
        distance <= 40 ~ "Mid Danger",
        TRUE           ~ "Low Danger"
      )) |>
      group_by(danger) |>
      summarise(
        shots  = n(),
        goals  = sum(goal),
        sv_pct = round(1 - mean(goal), 4),
        avg_xg = round(mean(xg, na.rm=TRUE), 4),
        .groups= "drop"
      ) |>
      mutate(
        danger = factor(danger, levels=c("High Danger","Mid Danger","Low Danger")),
        tip    = paste0(danger, "<br>Shots: ", shots,
                        "<br>Goals: ", goals,
                        "<br>Sv%: ", sv_pct,
                        "<br>Avg xG/shot: ", avg_xg),
        bar_color = case_when(
          danger == "High Danger" ~ "#f85149",
          danger == "Mid Danger"  ~ "#f0a500",
          TRUE                    ~ "#3fb950"
        )
      ) |>
      arrange(danger)
    
    # League average sv% by zone for reference
    league_zones <- shots |>
      filter(!is.na(goalie_id), empty_net == 0) |>
      mutate(danger = case_when(
        distance <= 20 ~ "High Danger",
        distance <= 40 ~ "Mid Danger",
        TRUE           ~ "Low Danger"
      )) |>
      group_by(danger) |>
      summarise(lg_sv_pct = round(1 - mean(goal), 4), .groups="drop") |>
      mutate(danger = factor(danger, levels=c("High Danger","Mid Danger","Low Danger"))) |>
      arrange(danger)
    
    plot_ly() |>
      add_bars(data=zone_stats, x=~danger, y=~sv_pct,
               marker=list(color=~bar_color, opacity=0.85),
               text=~tip, hoverinfo="text", name="This Goalie") |>
      add_lines(data=league_zones, x=~danger, y=~lg_sv_pct,
                line=list(color="#58a6ff", dash="dash", width=2),
                name="League Avg", hoverinfo="skip") |>
      layout(
        paper_bgcolor="#161b22", plot_bgcolor="#161b22",
        font  = list(color="#e6edf3"),
        xaxis = list(title="Danger Zone", gridcolor="#30363d"),
        yaxis = list(title="Save %", gridcolor="#30363d",
                     tickformat=".3f", range=c(0.75, 1.0)),
        legend= list(font=list(color="#e6edf3")),
        annotations = list(list(
          x=0.01, y=1.05, xref="paper", yref="paper",
          text="Blue dashed line = league average", showarrow=FALSE,
          font=list(color="#8b949e", size=10)
        ))
      )
  })
  
  output$plot_goalie_shotmap <- renderPlot({
    gid <- selected_goalie_id()
    if (is.null(gid)) return(NULL)
    
    d <- shots |>
      filter(goalie_id == gid, empty_net == 0)
    if (input$goalie_gametype != "both")
      d <- d |> filter(game_type == input$goalie_gametype)
    if (input$goalie_season != "all")
      d <- d |> filter(season == input$goalie_season)
    
    d <- d |> mutate(
      outcome = factor(ifelse(goal == 1, "Goal Allowed", "Save"),
                       levels = c("Goal Allowed", "Save")),
      x_plot  = x_standardized,
      y_plot  = y
    )
    
    # Save% by zone for subtitle
    hd_sv <- d |> filter(distance <= 20) |>
      summarise(sv=round(1-mean(goal),3)) |> pull(sv)
    ld_sv <- d |> filter(distance >  40) |>
      summarise(sv=round(1-mean(goal),3)) |> pull(sv)
    
    goalie_name <- players$full_name[players$player_id == gid]
    if (length(goalie_name) == 0) goalie_name <- paste("Goalie", gid)
    
    ggplot(d |> filter(!is.na(x_plot), !is.na(y_plot)),
           aes(x=x_plot, y=y_plot, color=outcome, alpha=outcome, size=outcome)) +
      geom_point(shape=16) +
      scale_color_manual(values=c("Goal Allowed"="#f85149","Save"="#30363d")) +
      scale_alpha_manual(values=c("Goal Allowed"=0.9, "Save"=0.35)) +
      scale_size_manual(values=c("Goal Allowed"=2.5, "Save"=1.2)) +
      annotate("rect", xmin=69, xmax=89, ymin=-3, ymax=3,
               fill=NA, color="#58a6ff", linewidth=0.8) +
      labs(
        title = paste0(goalie_name, " -- Shot Map"),
        subtitle = paste0("High danger Sv%: ", hd_sv,
                          "   |   Low danger Sv%: ", ld_sv,
                          "   |   Red = goal allowed, Grey = save"),
        x=NULL, y=NULL, color=NULL, alpha=NULL, size=NULL
      ) +
      theme_void() +
      theme(
        plot.background  = element_rect(fill="#161b22", color=NA),
        panel.background = element_rect(fill="#161b22", color=NA),
        plot.title       = element_text(color="#e6edf3", size=14, face="bold", hjust=0.5),
        plot.subtitle    = element_text(color="#8b949e", size=10, hjust=0.5, margin=margin(b=10)),
        legend.text      = element_text(color="#e6edf3"),
        legend.position  = "bottom"
      ) +
      coord_cartesian(xlim=c(25, 89))
  })
  
  # TEAM vs. GOALIE
  
  matchup_data <- reactive({
    gid  <- selected_goalie_id()
    team <- input$matchup_team
    if (is.null(gid) || is.null(team)) return(NULL)
    
    d <- shots |> filter(!is.na(goalie_id), empty_net == 0)
    if (input$goalie_gametype != "both")
      d <- d |> filter(game_type == input$goalie_gametype)
    if (input$goalie_season != "all")
      d <- d |> filter(season == input$goalie_season)
    
    list(
      team_shots  = d |> filter(shooting_team == team),
      goalie_shots= d |> filter(goalie_id == gid),
      h2h_shots   = d |> filter(shooting_team == team, goalie_id == gid),
      team        = team,
      gid         = gid
    )
  })
  
  output$matchup_summary_cards <- renderUI({
    md <- matchup_data()
    if (is.null(md)) return(NULL)
    
    h2h <- md$h2h_shots
    goalie_name <- players$full_name[players$player_id == md$gid]
    if (length(goalie_name) == 0) goalie_name <- paste("Goalie", md$gid)
    
    if (nrow(h2h) == 0) {
      return(div(style="color:#8b949e;padding:12px;",
                 paste0("No head to head data found for ", md$team, " vs ", goalie_name,
                        " under the current filters. Try selecting Both for game type or All Seasons.")))
    }
    
    games <- h2h |>
      group_by(game_id) |>
      summarise(
        xg_for    = round(sum(xg, na.rm=TRUE), 2),
        goals_for = sum(goal),
        .groups   = "drop"
      )
    
    n_games    <- nrow(games)
    total_xg   <- round(sum(games$xg_for), 2)
    total_goals<- sum(games$goals_for)
    xg_per_game<- round(total_xg / n_games, 2)
    g_per_game <- round(total_goals / n_games, 2)
    over_perf  <- sum(games$goals_for > games$xg_for)
    
    card <- function(label, val, sub=NULL, color="#e6edf3") {
      div(style="background:#21262d;border-radius:6px;padding:10px 14px;text-align:center;flex:1;",
          div(style="color:#8b949e;font-size:10px;text-transform:uppercase;letter-spacing:0.5px;margin-bottom:4px;", label),
          div(style=paste0("color:",color,";font-size:20px;font-weight:700;"), val),
          if (!is.null(sub)) div(style="color:#8b949e;font-size:10px;margin-top:2px;", sub)
      )
    }
    
    div(style="display:flex;gap:8px;flex-wrap:wrap;padding:4px 0;",
        card("Games Faced", n_games),
        card("xG / Game", xg_per_game, "team attack quality",
             ifelse(xg_per_game > 2.5, "#f85149", "#3fb950")),
        card("Goals / Game", g_per_game, "actual scoring rate",
             ifelse(g_per_game > xg_per_game, "#f85149", "#3fb950")),
        card("Games Over xG", paste0(over_perf, " / ", n_games),
             "team outscored their chances")
    )
  })
  
  output$plot_matchup_projection <- renderPlotly({
    md <- matchup_data()
    if (is.null(md)) return(NULL)
    
    team_zones <- md$team_shots |>
      mutate(danger = case_when(
        distance <= 20 ~ "High Danger",
        distance <= 40 ~ "Mid Danger",
        TRUE           ~ "Low Danger"
      )) |>
      group_by(danger) |>
      summarise(shots = n(), .groups="drop") |>
      mutate(danger = factor(danger, levels=c("High Danger","Mid Danger","Low Danger")),
             pct    = round(shots / sum(shots) * 100, 1))
    
    goalie_zones <- md$goalie_shots |>
      mutate(danger = case_when(
        distance <= 20 ~ "High Danger",
        distance <= 40 ~ "Mid Danger",
        TRUE           ~ "Low Danger"
      )) |>
      group_by(danger) |>
      summarise(sv_pct = round(1 - mean(goal), 4), .groups="drop") |>
      mutate(danger = factor(danger, levels=c("High Danger","Mid Danger","Low Danger")))
    
    combined <- left_join(team_zones, goalie_zones, by="danger") |>
      mutate(
        bar_color = case_when(
          danger == "High Danger" ~ "#f85149",
          danger == "Mid Danger"  ~ "#f0a500",
          TRUE                    ~ "#3fb950"
        ),
        tip_bar  = paste0(danger, "<br>", md$team, " shoots ", pct, "% from here"),
        tip_line = paste0(danger, "<br>Goalie Sv%: ", sv_pct)
      )
    
    goalie_name <- players$full_name[players$player_id == md$gid]
    if (length(goalie_name) == 0) goalie_name <- paste("Goalie", md$gid)
    
    plot_ly() |>
      add_bars(data=combined, x=~danger, y=~pct,
               marker=list(color=~bar_color, opacity=0.75),
               text=~tip_bar, hoverinfo="text",
               name=paste0(md$team, " Shot %"), yaxis="y") |>
      add_lines(data=combined, x=~danger, y=~sv_pct,
                line=list(color="#58a6ff", width=3),
                text=~tip_line, hoverinfo="text",
                name=paste0(goalie_name, " Sv%"), yaxis="y2",
                mode="lines+markers",
                marker=list(size=8, color="#58a6ff")) |>
      layout(
        paper_bgcolor="#161b22", plot_bgcolor="#161b22",
        font  = list(color="#e6edf3"),
        xaxis = list(title="Danger Zone", gridcolor="#30363d"),
        yaxis = list(title=paste0(md$team, " Shot % from Zone"),
                     gridcolor="#30363d", ticksuffix="%"),
        yaxis2= list(title="Goalie Save %", overlaying="y", side="right",
                     tickformat=".3f", range=c(0.75,1.0),
                     gridcolor="rgba(0,0,0,0)"),
        legend= list(font=list(color="#e6edf3"), orientation="h",
                     x=0, y=-0.2),
        bargap= 0.3
      )
  })
  
  output$plot_matchup_history <- renderPlotly({
    md <- matchup_data()
    if (is.null(md) || nrow(md$h2h_shots) == 0) return(NULL)
    
    goalie_name <- players$full_name[players$player_id == md$gid]
    if (length(goalie_name) == 0) goalie_name <- paste("Goalie", md$gid)
    
    games <- md$h2h_shots |>
      group_by(game_id, game_date, season) |>
      summarise(
        xg_for    = round(sum(xg, na.rm=TRUE), 2),
        goals_for = sum(goal),
        shots     = n(),
        .groups   = "drop"
      ) |>
      mutate(
        outperformed = goals_for > xg_for,
        tip = paste0(game_date, "<br>",
                     md$team, " xG: ", xg_for, "<br>",
                     md$team, " Goals: ", goals_for, "<br>",
                     "Shots: ", shots, "<br>",
                     ifelse(outperformed, "Outscored xG", "Underscored xG"))
      )
    
    max_val <- max(c(games$xg_for, games$goals_for), na.rm=TRUE) + 0.5
    
    plot_ly() |>
      add_segments(x=0, xend=max_val, y=0, yend=max_val,
                   line=list(color="#30363d", dash="dash", width=1),
                   hoverinfo="skip", showlegend=FALSE) |>
      add_markers(data=games, x=~xg_for, y=~goals_for,
                  color=~outperformed,
                  colors=c("TRUE"="#f85149", "FALSE"="#3fb950"),
                  size=~shots, sizes=c(8, 30),
                  text=~tip, hoverinfo="text",
                  marker=list(opacity=0.8, sizemode="area"),
                  showlegend=TRUE,
                  name=~ifelse(outperformed, "Team outscored xG", "Team underscored xG")
      ) |>
      layout(
        paper_bgcolor="#161b22", plot_bgcolor="#161b22",
        font  = list(color="#e6edf3"),
        xaxis = list(title=paste0(md$team, " xG (chances deserved)"),
                     gridcolor="#30363d", range=c(0, max_val)),
        yaxis = list(title=paste0(md$team, " Actual Goals"),
                     gridcolor="#30363d", range=c(0, max_val)),
        legend= list(font=list(color="#e6edf3"), orientation="h",
                     x=0, y=-0.25),
        annotations= list(list(
          x=0.5, y=1.05, xref="paper", yref="paper",
          text=paste0(nrow(games), " games vs ", goalie_name),
          showarrow=FALSE, font=list(color="#8b949e", size=10)
        ))
      )
  })
  
}

shinyApp(ui, server)
