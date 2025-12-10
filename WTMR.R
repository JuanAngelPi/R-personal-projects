#-------------------------------------------------------------------------
#  COMBINED MULTIPLE REGRESSION OF WINS BASED ON TEAM STATS
#-------------------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(nflverse)
library(GGally)
library(stats)
library(ggrepel)
library(broom)
library(kableExtra)
library(pscl)
library(car)
library(boot)
library(statmod)





# Load play-by-play data
pbp = load_pbp(2018:2022)

# Compute offensive and defensive points
pbp = pbp %>%
  mutate(
    points_scored = posteam_score_post - posteam_score,
    points_allowed = defteam_score_post - defteam_score
  )

#---------------------------------------
# Offensive stats by team
#---------------------------------------
offensive_team = pbp %>%
  filter(!is.na(posteam)) %>%
  group_by(season, team = posteam) %>%
  summarize(
    off_plays = n(),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    pass_td = sum(pass_touchdown, na.rm = TRUE),
    rush_td = sum(rush_touchdown, na.rm = TRUE),
    total_td = pass_td + rush_td,
    points_scored = sum(points_scored, na.rm = TRUE),
    total_epa_off = sum(epa, na.rm = TRUE),
    epa_per_play_off = mean(epa, na.rm = TRUE),
    success_rate_off = mean(success, na.rm = TRUE),
    passes = sum(pass, na.rm = TRUE),
    rushes = sum(rush, na.rm = TRUE),
    first_downs = sum(first_down, na.rm = TRUE),
    .groups = "drop"
  )

#---------------------------------------
# Defensive stats by team
#---------------------------------------
defensive_team = pbp %>%
  filter(!is.na(defteam)) %>%
  group_by(season, team = defteam) %>%
  summarize(
    def_plays = n(),
    epa_per_play_def = mean(epa, na.rm = TRUE),
    success_rate_def = mean(epa < 0, na.rm = TRUE),
    sacks = sum(sack == 1, na.rm = TRUE),
    interceptions = sum(interception == 1, na.rm = TRUE),
    qb_hits = sum(qb_hit == 1, na.rm = TRUE),
    fumbles_forced = sum(fumble_forced == 1, na.rm = TRUE),
    points_allowed = sum(points_allowed, na.rm = TRUE),
    .groups = "drop"
  )

#---------------------------------------
# Game results / wins
#---------------------------------------
game_results = pbp %>%
  group_by(season, game_id) %>%
  summarize(
    home_team = first(home_team),
    away_team = first(away_team),
    home_final = last(home_score),
    away_final = last(away_score),
    .groups = "drop"
  ) %>%
  mutate(
    winner = case_when(
      home_final > away_final ~ home_team,
      away_final > home_final ~ away_team,
      TRUE ~ NA_character_
    )
  ) %>%
  count(season, winner, name = "wins") %>%
  rename(team = winner)

#---------------------------------------
# Combine offensive, defensive stats, and wins
#---------------------------------------
final_data = offensive_team %>%
  left_join(defensive_team, by = c("season", "team")) %>%
  left_join(game_results, by = c("season", "team"))

# Create binary win variable
final_data$win_binary = ifelse(final_data$wins > 10, 1, 0)

#---------------------------------------
# Combined multiple regression model
#---------------------------------------
model_combined = glm(
  win_binary ~ points_scored + sacks + interceptions + fumbles_forced + qb_hits + points_allowed ,
  data = final_data,
  family = binomial
)

# Model summary
summary(model_combined)

# Tidy output with odds ratios
model_combined %>%
  tidy(conf.int = TRUE, conf.level = 0.95, exponentiate = FALSE) %>%
  kbl(format = "pipe", digits = 2) %>%
  kable_styling()

# VIF for multicollinearity
vif(model_combined)

# Pseudo R-squared
pR2(model_combined)

# ANOVA TEST
anova(model_combined, test = "Chisq")


# Q-Q PLOTS
glm.diag.plots(model_combined)
qqnorm(qres.binom(model_combined))


cor.test(final_data$interceptions, final_data$points_scored)
cor.test(final_data$sacks, final_data$points_scored)
