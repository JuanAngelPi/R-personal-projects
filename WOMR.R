#-------------------------------------------------------------------------
#  MULTIPLE REGRESSION OF WINS BASED ON OFFENSIVE STATS
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
    offense_points = posteam_score_post - posteam_score,
    defensive_points = defteam_score_post - defteam_score,
  )

#---------------------------------------
# Offensive stats by team
#---------------------------------------
offensive_team = pbp %>% 
  filter(!is.na(posteam)) %>% 
  mutate(
    points_scored = posteam_score_post - posteam_score
  ) %>% 
  group_by(season, posteam) %>% 
  summarize(
    plays = n(),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    pass_td = sum(pass_touchdown, na.rm = TRUE),
    rush_td = sum(rush_touchdown, na.rm = TRUE),
    total_td = pass_td + rush_td,
    points_scored = sum(points_scored, na.rm = TRUE),
    total_epa = sum(epa, na.rm = TRUE),
    epa_per_play = mean(epa, na.rm = TRUE),
    success_rate = mean(success, na.rm = TRUE),
    passes = sum(pass, na.rm = TRUE),
    rushes = sum(rush, na.rm = TRUE),
    first_downs = sum(first_down, na.rm = TRUE)
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
  rename(team = "winner")

final_data = offensive_team %>% 
  left_join(
    game_results %>% select(season, team, wins),
    by = (c("season", "posteam"="team"))
  )

final_data$win_binary = ifelse(final_data$wins > 10, 1, 0)

model_1 = glm(
  win_binary ~ points_scored, 
    data = final_data, 
    family = binomial )

summary(model_1)

model_1 %>% 
  tidy(model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  kbl(format = "pipe", digits = 3) %>% 
  kable_styling()

pR2(model_1)

model_2 = lm(
  points_scored ~  total_epa + total_td + plays, 
    data = final_data)

summary(model_2)

  model_2 %>% 
    tidy(model_2, conf.int = TRUE, conf.level = 0.95, exponentiate = FALSE) %>% 
    kbl(format = "pipe", digits = 3) %>% 
    kable_styling()

anova(model_1, test = "Chisq")


# Plots model 1
glm.diag.plots(model_1)
qqnorm(qres.binom(model_1))

