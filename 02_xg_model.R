# 02_xg_model.R
# Building out the Expected Goals (xG) model using GAM
# Training over 7 seasons of NHL shot data

library(dplyr)
library(mgcv)
library(glue)
library(purrr)

# 1. LOAD ALL 7 SEASONS

seasons <- c("20192020","20202021","20212022",
             "20222023","20232024","20242025","20252026")

all_shots <- purrr::map_dfr(seasons, function(s) {
  readRDS(glue("data/shots_{s}.rds"))
})

cat("Total shots loaded:", nrow(all_shots), "\n")
cat("Goal rate:", round(mean(all_shots$goal) * 100, 2), "%\n")
cat("Seasons:", paste(unique(all_shots$season), collapse=", "), "\n")

# 2. TRAIN / VALIDATION SPLIT
# exclude empty net shots — not a measure of shot quality
# exclude 20252026 as holdout validation season

train <- all_shots |>
  filter(
    season        != "20252026",
    empty_net     == 0,
    !is.na(distance),
    !is.na(angle),
    !is.na(shot_type),
    !is.na(is_rebound),
    !is.na(is_rush),
    !is.na(strength_state),
    !is.na(overtime)
  )

validation <- all_shots |>
  filter(
    season        == "20252026",
    empty_net     == 0,
    !is.na(distance),
    !is.na(angle),
    !is.na(shot_type),
    !is.na(is_rebound),
    !is.na(is_rush),
    !is.na(strength_state),
    !is.na(overtime)
  )

cat("Train shots:     ", nrow(train),      "\n")
cat("Validation shots:", nrow(validation), "\n")

cat("\nStrength state breakdown in training data:\n")
print(train |> group_by(strength_state) |> summarise(shots=n(), conv_rate=round(mean(goal)*100,2)))

# 3. FIT THE GAM MODEL
# s() fits smooth curves instead of straight lines
# captures non-linear relationships e.g. distance vs goal probability
#
# new vs old model:
#   OLD: distance + angle + shot_type + is_rebound
#   NEW: + strength_state (PP shots more dangerous at same location)
#        + is_rush        (transition shots before defense sets)
#        + overtime       (3v3 OT creates wide open chances)
#        - empty_net      (excluded from training entirely)

model_gam <- gam(
  goal ~
    s(distance,    k = 10) +
    s(angle,       k = 10) +
    shot_type              +
    is_rebound             +
    is_rush                +
    strength_state         +
    overtime,
  data   = train,
  family = binomial(link = "logit")
)

saveRDS(model_gam, "data/model_gam.rds")
cat("Model saved\n")
summary(model_gam)

# 4. VALIDATE

validation$xg <- predict(model_gam, newdata = validation, type = "response")

cat("\nSample xG values:\n")
print(head(validation[, c("distance","angle","shot_type","strength_state",
                          "is_rebound","is_rush","overtime","goal","xg")]))

cat("\nValidation: actual conversion vs avg xG by strength state:\n")
print(
  validation |>
    group_by(strength_state) |>
    summarise(
      shots       = n(),
      actual_rate = round(mean(goal)        * 100, 2),
      avg_xg      = round(mean(xg, na.rm=TRUE) * 100, 2)
    )
)

# 5. ADD xG PREDICTIONS TO FULL DATASET
# empty net shots get xg = 0.85
# high but not 1.0 — goalies still make saves on some

all_shots$xg <- predict(model_gam, newdata = all_shots, type="response")
all_shots$xg[all_shots$empty_net == 1] <- 0.85

# 6. ADD game_type COLUMN

all_shots <- all_shots |>
  mutate(game_type = case_when(
    substr(as.character(game_id), 5, 6) == "02" ~ "Regular Season",
    substr(as.character(game_id), 5, 6) == "03" ~ "Playoffs",
    TRUE ~ "Other"
  ))

saveRDS(all_shots, "data/shots_all.rds")
message("shots_all.rds saved with xG predictions and game_type")
message("Game type breakdown:")
print(table(all_shots$game_type, all_shots$season))