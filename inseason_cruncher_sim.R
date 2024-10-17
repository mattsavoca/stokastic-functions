current_week = 2
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
library("easypackages")
packages("tidyverse", "ffsimulator", "ffscrapr", prompt =  F)
projects_folder = paste0(dropbox, "Matt Savoca/Projects/")
week_folder = paste0(projects_folder, "NFL 2022/Week ", 
                     if_else(current_week < 10, "", ""), 
                     as.character(current_week))


crunch_grid_raw = read_csv(paste0(week_folder,"/crunch1_grid.csv")) %>% clean_names()

crunch_grid_dkmain_raw = read_csv(paste0(week_folder,"/crunch_grid_ids_dkmain.csv")) %>% clean_names()

crunch_grid_dkmain_raw %>% glimpse()

crunch_proj_raw = read_csv(paste0(week_folder,"/crunch1_proj.csv")) %>% clean_names()

cruncher_players_dkmain_raw = read_csv(paste0(week_folder,"/cruncher_players_dkmain.csv")) %>% clean_names()
  

grid_longer = crunch_grid_raw %>%
  pivot_longer(cols = contains(c("qb", "rb_", "wr_", "te", "flex", "dst")), names_to = "roster_pos", values_to = "player_label") %>%
  mutate(
    player = str_extract(player_label, "[A-z .'-]+ [0-9]") %>% str_remove(" [0-9]") %>% trim(),
    player = if_else(player == "ers", "49ers", player)
  ) %>%
  select(number, player) %>%
  left_join(
    cruncher_players_dkmain_raw %>% select(player, pos, team)
  ) %>%
  select(
    number, player, pos, team
  )

grid_nested = 
  grid_longer %>%
  group_by(player, pos, team) %>%
  nest()

proj_nested = crunch_proj_raw %>%
  pivot_longer(cols = contains(c("number")), names_to =  "sim_num", values_to = "sim_proj") %>%
  mutate(sim_num = str_extract(sim_num, "[0-9]+") %>% as.numeric()) %>%
  rowwise() %>%
  mutate(sim_proj = max(sim_proj, -2)) %>%
  ungroup() %>%
  select(player, pos, sim_num, sim_proj) %>%
  group_by(sim_num) %>%
  nest()


test_proj_df = proj_nested$data[[1]]

sim_joiner = function(df, grid_longer){
  suppressMessages({
  df %>%
    right_join(grid_longer) %>%
    group_by(number) %>%
    mutate(lineup_score = sum(sim_proj)) %>%
    ungroup() %>%
    select(-sim_proj) %>%
    arrange(number)
  })
}
  
test_nest = proj_nested[1:2,]


lineup_scores = proj_nested %>%
  mutate(scores = map(data, ~sim_joiner(., grid_longer = grid_longer))) %>%
  unnest(scores) %>%
  group_by(number) %>%
  select(sim_num, number, lineup_score) %>%
  summarize(lineup_score = mean(lineup_score)) %>%
  arrange(-lineup_score)



top_lineups = lineup_scores %>%
  top_n(150, lineup_score) %>%
  left_join(
    grid_longer
  ) 

top_exposure = top_lineups %>%
  group_by(player, pos, team) %>%
  summarize(exp = n()/150) %>%
  ungroup() %>%
  group_by(pos) %>%
  arrange(-exp) %>%
  View()
