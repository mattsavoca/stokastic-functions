current_week = 2
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
library("easypackages")
packages("tidyverse", "ffsimulator", "ffscrapr", prompt =  F)

conn = sleeper_connect(league_id = 843584157354401792)

scoring_history = ff_scoringhistory(conn = conn, season = 2016:2021)

adp_outcomes = ffs_adp_outcomes(scoring_history = scoring_history, 
                                gp_model = "simple", 
                                pos_filter = c("QB", "RB", "WR", "TE"))


latest_rankings = ffs_latest_rankings("week")

fp_projections = ffs_generate_projections_week(
  adp_outcomes,
  latest_rankings,
  n = 500
)







sample_sim = fp_projections %>% filter(week == 1) %>% filter(
  (pos != "QB" | ecr < 33) &
  (pos != "TE" | ecr <= 40) &
  ecr <=75
)

qbrb_df = sample_sim %>%
  filter(pos %in% c("QB", "RB")) %>%
  group_by(team, pos) %>%
  mutate(
    team_pct = projection/sum(projection),
    qb_proj = sum(projection)
  ) %>%
  ungroup() %>% 
  mutate(
    new_proj = projection
  )

wr_df = sample_sim %>%
  filter(pos != "QB") %>%
  left_join(
    qbrb_df %>% filter(pos == "QB") %>% group_by(team) %>% summarize(qb_proj = sum(projected_score))
  ) %>%
  group_by(team) %>%
  mutate(team_pct = projection/sum(projection)) %>% 
  rowwise() %>%
  filter(pos == "WR", team != "FA") %>%
  mutate(
    new_proj = qb_proj * team_pct * .64 + projected_score * (1-.64)
  ) %>% 
  arrange(-new_proj)

te_df = sample_sim %>%
  filter(pos != "QB") %>%
  left_join(
    qbrb_df %>% filter(pos == "QB") %>% group_by(team) %>% summarize(qb_proj = sum(projected_score))
  ) %>%
  group_by(team) %>%
  mutate(team_pct = projection/sum(projection)) %>% 
  rowwise() %>%
  filter(pos == "TE", team != "FA") %>%
  mutate(
    new_proj = qb_proj * team_pct * .35 + projected_score * (1-.35)
  ) %>% 
  arrange(-new_proj)

proj_df = qbrb_df %>%
  rbind(wr_df, te_df) %>%
  arrange(-new_proj)

