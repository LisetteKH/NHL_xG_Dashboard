---

## License

Copyright (c) 2026 Lisette Kamper-Hinson. All rights reserved.

This repository is intended for portfolio and demonstration purposes only. Reproduction or use of any code, methodology, or content without express written permission from the author is prohibited.

---

# NHL Expected Goals Dashboard

Lisette Kamper-Hinson | M.S. Computer Science | Data Science Certificate 

[![R](https://img.shields.io/badge/R-4.x-276DC3)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Dashboard-blue)](https://shiny.posit.co/)
[![ggplot2](https://img.shields.io/badge/ggplot2-Visualization-276DC3)](https://ggplot2.tidyverse.org/)
[![Plotly](https://img.shields.io/badge/Plotly-Interactive%20Charts-3F4F75)](https://plotly.com/r/)
[![mgcv](https://img.shields.io/badge/mgcv-GAM%20Modeling-FF6B35)](https://cran.r-project.org/package=mgcv)
[![NHL API](https://img.shields.io/badge/NHL%20API-REST-009688)](https://api-web.nhle.com/v1)
[![Sports Analytics](https://img.shields.io/badge/Sports-Analytics-black)](https://github.com/)


An interactive R Shiny dashboard for analyzing shot quality and expected goals (xG) across 7 NHL seasons (2019-20 through 2025-26). Built as an analytics portfolio project demonstrating end-to-end sports data science: data engineering, statistical modeling, and interactive visualization.

---

## What This Project Does

Most NHL analytics tools show raw shot counts. This dashboard goes further, it asks not just *how many* shots a team took, but *how dangerous* those shots were, and whether goalies stopped more or fewer goals than they should have given the difficulty of the chances they faced.

The core metric is **expected goals (xG)**: a probability assigned to each shot based on where it came from, what type of shot it was, and the game situation. A shot from the slot on a rebound is worth more than a wrist shot from the blue line. Aggregating xG across games reveals which teams and goalies are genuinely good versus lucky.

---

## The Model

The xG model is a **Generalized Additive Model (GAM)** trained on 668,000 shots from six seasons, with 118,000 shots from 2025-26 held out for validation.

**Formula:**
```
goal ~ s(distance, k=10) + s(angle, k=10) + shot_type +
       is_rebound + is_rush + strength_state + overtime
```

Smooth terms on distance and angle allow the model to capture the non-linear relationship between shot location and goal probability, a shot 15 feet out is disproportionately more dangerous than one 25 feet out, not just slightly more dangerous. GAM handles this naturally where logistic regression with raw distance would not.

**Key modeling decisions:**
- Empty net shots excluded from training and assigned xG = 0.85 post-prediction, since they are effectively guaranteed goals that reflect game state rather than shot quality
- Strength state included (5v5, powerplay, shorthanded) because shot quality distributions differ meaningfully by situation
- Rebound and rush flags included as binary features, both significantly increase goal probability

**Validation results (2025-26 holdout):**

| Situation | Actual Goal Rate | Model Predicted |
|-----------|-----------------|-----------------|
| 5v5       | 6.19%           | 6.49%           |
| Powerplay | 10.4%           | 11.8%           |
| Shorthanded | 7.09%         | 7.64%           |

The model slightly overestimates across all situations, which is expected, it sees shot quality but not goalie quality, so the residual is partly explained by goalie performance above/below average.

---

## Data Pipeline

Data is pulled from the **NHL Stats API** (`api-web.nhle.com`) using a custom R client. The pipeline:

1. Pulls play-by-play data for all regular season and playoff games across 7 seasons
2. Extracts shot events with coordinates, shot type, shooter, goalie, game situation
3. Standardizes coordinates so all shots are mapped to the same end of the ice
4. Engineers features: distance, angle, rebound flag (shot within 3 seconds of previous shot from close range), rush flag, strength state from situation code
5. Runs GAM predictions on all 792,000 shots and stores xG alongside raw event data

**Data summary:**
- 792,115 total shots across 7 seasons
- 34 features per shot including xG and game context
- Covers all 32 NHL teams, regular season and playoffs

---

## Dashboard Tabs

### League Overview
League-wide xG landscape. xGF vs xGA scatter shows which teams generate more shot quality than they allow. Rankings table and bar chart for xGF%. Goal rate by season and rebound conversion rate show how league-wide scoring patterns have shifted over 7 years.

### Regular Season vs Playoffs
Animated visualization showing how team xG profiles shift from regular season to combined regular + playoff performance. Teams that move toward the elite quadrant in playoffs performed better when the stakes increased.

### Strength State
Breaks team xG performance into 5v5, powerplay, and penalty kill. A team can dominate at even strength but carry a weak powerplay -- this tab separates those contributions. Useful for evaluating roster construction tradeoffs.

### Game Explorer
Two modes:

**Live tracker** -- polls the NHL API every 30 seconds during live games. Tracks cumulative xG in real time, shows score, shots on goal, and a game verdict explaining whether the leading team is winning on merit or against the run of play.

**Historical explorer** -- filter any of the 7 seasons of games by team or game type. Click any game to load a full recap: xG vs actual goals per team and a cumulative xG timeline with goal markers showing exactly when the momentum shifted.

### Team Spotlight
Deep dive on any team across any season, game type, and strength state. Includes:
- League baseline comparison with team stats side by side and color coded by direction
- Shot density heatmap showing where shots originate on the ice
- Danger zone breakdown (high / mid / low danger) with conversion rates
- Z-score profile chart comparing the team to league average across five dimensions: volume, quality, distance, rebound rate, rush rate
- Home vs away splits
- Head to head comparison against any other team with plain English edge analysis and actual numbers

### Goalie Dashboard
Goalie performance evaluated against expected goals rather than raw save percentage. The key metric is **GSAx (Goals Saved Above Expected)** -- the difference between actual goals allowed and xG allowed. Negative GSAx means the goalie stopped more goals than a league-average goalie would have given the same shots.

Features:
- Leaderboard sortable by GSAx, filterable by season, game type, and minimum shots faced
- Season-by-season GSAx bar chart for any selected goalie
- Save percentage by danger zone vs league average benchmark
- Shot map showing every shot faced with goals allowed highlighted
- **Team vs Goalie matchup** -- select any team to see how their shot profile matches up against the selected goalie (projected scoring from danger zone overlap) plus the full historical head-to-head record from games they have actually faced each other

---

## Technical Stack

| Component | Tool |
|-----------|------|
| Language | R |
| Dashboard framework | Shiny + shinydashboard |
| Statistical model | mgcv (GAM) |
| Interactive charts | plotly |
| Static charts | ggplot2 |
| Animation | gganimate + gifski |
| Data tables | DT |
| API calls | httr + jsonlite |
| Data manipulation | dplyr + tidyr + purrr |

---

## Project Structure

```
nhl_xg_project/
├── 00_api_client.R       # NHL API wrapper functions
├── 01_data_pipeline.R    # Season data pull and feature engineering
├── 02_xg_model.R         # GAM training, validation, prediction
├── app.R                 # Shiny dashboard (UI + server)
├── data/
│   ├── shots_all.rds     # 792k shots with xG, all 7 seasons
│   ├── model_gam.rds     # Trained GAM object
│   └── player_metadata.rds  # Player names, teams, headshots
└── README.md
```

---

## Design Decisions Worth Noting

**Why GAM over logistic regression?** Shot danger is not linear with distance. A shot from 10 feet is not just "twice as dangerous" as one from 20 feet -- the relationship is steeper and curves sharply near the net. GAM's smooth terms capture this without requiring manual polynomial specification.

**Why exclude empty net shots from training?** Empty net shots have xG near 1.0 by definition -- they are not useful for learning what makes a normal shot dangerous. Including them would distort the model toward factors that correlate with game state (score, time remaining) rather than shot quality.

**Why GSAx over adjusted save percentage?** GSAx is cumulative and in units of goals, which makes it directly interpretable. Vasilevskiy at -150 GSAx over 7 seasons means he saved 150 more goals than expected -- that is roughly 20 extra wins over that span. Adjusted save percentage normalizes this, which is useful for comparison but loses the magnitude.

**Strength state in the model vs as a filter:** Strength state is both a model feature (because shot quality differs by situation) and a dashboard filter (because users want to isolate 5v5 performance from special teams). This is intentional -- the xG values are situation-aware, and the filters let analysts ask situation-specific questions without retraining.

---

## Limitations

- **Rush detection is approximate.** Rush shots are flagged based on timing between consecutive events, not tracking data. Real rush detection requires knowing the puck crossed the blue line at speed. The feature is directionally useful but not precise.
- **Two-season goalie comparisons are noisy.** GSAx stabilizes around 2,000+ shots. Goalies with fewer than 1,000 shots faced should be interpreted carefully.
- **The model does not account for traffic or screen.** Shot quality in front of the net is higher than coordinates alone suggest. This is a known limitation of coordinate-based xG models without tracking data.
- **Live xG uses the same model as historical.** Live shots are run through the GAM in real time, which works well but does not account for game state (score, time remaining) that affects how teams play.

---

## Author

**Lisette Kamper-Hinson**
M.S. Computer Science | Data Science Certificate
[LinkedIn](https://www.linkedin.com/in/lisette-kamper-hinson) | [GitHub](https://github.com/LisetteKH)






