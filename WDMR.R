#-------------------------------------------------------------------------
#  MULTIPLE REGRESSION OF WINS BASED ON DEFENSIVE STATS
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


pbp = load_pbp(2018:2022)




pbp = pbp %>%
  mutate(
    points_by_offense = posteam_score_post - posteam_score,
    points_by_defense = defteam_score_post - defteam_score
  )




team_def = pbp %>%
  filter(!is.na(defteam)) %>%
  mutate(
    points_allowed = defteam_score_post - defteam_score
  ) %>%
  group_by(season, defteam) %>%
  summarize(
    plays = n(),
    epa_per_play = mean(epa, na.rm = TRUE),
    success_rate = mean(epa < 0, na.rm = TRUE),
    sacks = sum(sack == 1, na.rm = TRUE),
    interceptions = sum(interception == 1, na.rm = TRUE),
    qb_hits = sum(qb_hit == 1, na.rm = TRUE),
    fumbles_forced = sum(fumble_forced == 1, na.rm = TRUE),
    points_allowed = sum(points_allowed, na.rm = TRUE)
  )



game_results = pbp %>%
  group_by(game_id, season) %>%
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



final_data = team_def %>%
  left_join(
    game_results %>% select(season, team, wins),
    by = c("season", "defteam" = "team")
  )

nrow(final_data)


final_data$win_binary <- ifelse(final_data$wins > 10, 1, 0)

model = glm(
  win_binary ~ sacks + interceptions + fumbles_forced + qb_hits + points_allowed + epa_per_play + success_rate,
  data = final_data,
  family = binomial
)


summary(model)

model %>% 
  tidy(model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  kbl(format = "pipe", digits = 2) %>% 
  kable_styling()




pR2(model)
