current_week = 21
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
#remotes::install_github("ffverse/ffopportunity")
#remotes::install_github("dfs-with-r/coach")
library("easypackages")
packages("tidyverse", "ffsimulator", "ffscrapr", "progressr","tictoc","coach", "ffopportunity", "remotes", prompt =  F)
projects_folder = paste0(dropbox, "Matt Savoca/Projects/")
week_folder = paste0(projects_folder, "NFL 2022/Week ", 
                     if_else(current_week < 10, "", ""), 
                     as.character(current_week))


boom_bust_raw = read_csv(paste0(week_folder,"/NFL DK Boom Bust.csv")) %>% clean_names()

proj_raw = read_csv(paste0(week_folder,"/NFL DFS Main Projections.csv")) %>% clean_names()

matchups = proj_raw %>% select(team, opp = opponent) %>% distinct()

boom_bust = boom_bust_raw %>%
  left_join(matchups) %>%
  select(name, team, opp, everything())

mainslate_boombust = boom_bust %>% filter(salary > 0, slate == "Main")

# Functions ------

create_custom_boombust_proj = function(df, randomness_pct = 0.0){
  suppressMessages({
  
    df = df %>%
      select(name, team, opp, salary, pos = position, projection, std_dev) %>%
      rowwise() %>%
      mutate(
        projection = rnorm(1, projection, (std_dev + (randomness_pct*std_dev))),
        projection = max(-2, projection)
      ) %>%
      ungroup() %>%
      select(-std_dev)
    
    
    qbrb_df = df %>%
      filter(pos %in% c("QB", "RB")) %>%
      group_by(team, opp, pos) %>%
      mutate(
        team_pct = projection/sum(projection[projection > 0]),
        qb_proj = sum(projection)
      ) %>%
      ungroup() %>% 
      mutate(
        new_proj = projection
      )
    
    wr_df = df %>%
      filter(pos != "QB") %>%
      left_join(
        qbrb_df %>% filter(pos == "QB") %>% group_by(team) %>% summarize(qb_proj = sum(projection[projection > 0]))
      ) %>%
      group_by(team) %>%
      mutate(team_pct = projection/sum(projection[projection > 0])) %>% 
      rowwise() %>%
      filter(pos == "WR", team != "FA") %>%
      mutate(
        new_proj = qb_proj * team_pct * .64 + projection * (1-.64)
      ) %>% 
      arrange(-new_proj)
    
    te_df = df %>%
      filter(pos != "QB") %>%
      left_join(
        qbrb_df %>% filter(pos == "QB") %>% group_by(team) %>% summarize(qb_proj = sum(projection[projection > 0]))
      ) %>%
      group_by(team) %>%
      mutate(team_pct = projection/sum(projection)) %>% 
      rowwise() %>%
      filter(pos == "TE", team != "FA") %>%
      mutate(
        new_proj = qb_proj * team_pct * .35 + projection * (1-.35)
      ) %>% 
      arrange(-new_proj)
    
    dst_df = df %>%
      filter(pos != "QB") %>%
      mutate(team_pct = 1) %>%
      left_join(
        qbrb_df  %>% rename(team = opp, team_orig = team) %>% filter(pos == "QB") %>% group_by(team) %>% summarize(qb_proj = sum(projection[projection > 0]))
      ) %>%
      rowwise() %>%
      filter(pos == "DST", team != "FA") %>%
      mutate(
        new_proj = (qb_proj * -.47 + projection*1.47)
      ) %>% 
      arrange(-new_proj)
    
    
    
    proj_df = qbrb_df %>%
      rbind(wr_df, te_df, dst_df) %>%
      arrange(-new_proj) %>%
      mutate(row_id = row_number(),
             player_id = row_number() %>% as.character()) %>%
      select(
        row_id, player_id, player = name,team, position = pos, salary, fpts_proj = new_proj,
        ) %>%
      mutate(
        salary = as.integer(salary)
    )
  })
  
  return(proj_df)
}


# oep_boombust %>%
#   create_oep_boombust_proj() %>%
#   select(
#     player, team, position, salary
#   ) %>%
#   left_join(
#     mainslate_boombust %>% create_custom_boombust_proj()
#   ) %>% View()
  

create_boombust_lineup = function(df, n_lineups = 1, ...){
  model <- model_generic(df, 
                         #total_salary = rnorm(185, 5, n = 1), 
                         total_salary = 50000, 
                         roster_size = 9, 
                         max_from_team = 9)
  
  constraints <- list(
    "QB" = 1,
    "RB" = 2,
    "WR" = 3,
    "TE" = 1,
    "RB/WR/TE" = 1,
    "DST" = 1
  )
  
  model <- add_generic_positions_constraint(model, df, constraints)
  
  results = optimize_generic(df, model, L = n_lineups, max_exposure = .4, stack_sizes = 1:3)
  
  
  data.table::rbindlist(results, idcol = "lineup") 
  
  # %>%
  #   mutate(lineups = max(lineup)) %>%
  #   group_by(player) %>%
  #   mutate(exposure = n()/max(lineups)) %>%
  #   group_by(lineup) %>%
  #   mutate(player_sal_rk  = rank(-salary, ties.method= "random"),
  #          lineup_score = sum(fpts_proj),
  #          top_2_spend = sum(salary[player_sal_rk <= 2]),
  #          top_2_score = sum(fpts_proj[player_sal_rk <= 2]),
  #          top_3_spend = sum(salary[player_sal_rk <= 3]),
  #          top_3_score = sum(fpts_proj[player_sal_rk <= 3]),
  #          mid_3_spend = sum(salary[player_sal_rk <= 6 & player_sal_rk >= 4]),
  #          mid_3_sscore = sum(fpts_proj[player_sal_rk <= 6 & player_sal_rk >= 4]),
  #          top_4_spend = sum(salary[player_sal_rk <= 4]),
  #          top_4_score = sum(fpts_proj[player_sal_rk <= 4]),
  #          top_5_spend = sum(salary[player_sal_rk <= 5]),
  #          top_5_score = sum(fpts_proj[player_sal_rk <= 5]),
  #          players = trimws(paste(unique(player),collapse = ", "))) %>%
  #   ungroup()
    
}

stokastic_simjoiner = function(df, lineup_grid){
  suppressMessages({
    df %>%
      select(-row_id, -player_id) %>%
      right_join(lineup_grid %>% select(slate, lineup, player_id, player, team, position, salary)) %>%
      group_by(slate, lineup) %>%
      mutate(lineup_score = sum(fpts_proj, na.rm = T) %>% round(2)) %>%
      ungroup() %>%
      select(-fpts_proj) %>%
      arrange(lineup)
  })
}

quickgenerate_lineup = function(df,...){
  df %>%
    create_custom_boombust_proj() %>%
    create_boombust_lineup(...)
  
}

# TEST- create a lineup -------
optimal_lineup = mainslate_boombust %>%
  mutate(row_id = row_number(),
         salary = as.integer(salary)) %>%
  select(row_id, player_id = name, player = name, salary, team, position, fpts_proj = projection) %>%
  #create_custom_boombust_proj() %>%
  create_boombust_lineup(1, max_exposure = 1) %>%
  group_by(lineup) %>%
  mutate(
    score = sum(fpts_proj),
    total_sal = sum(salary)
  ) %>%
  arrange(-lineup)

optimal_lineup

mainslate_boombust %>%
  create_custom_boombust_proj() %>%
  create_boombust_lineup(3, max_exposure = 1) %>%
  group_by(lineup) %>%
  mutate(
    score = sum(fpts_proj),
    total_sal = sum(salary)
  ) %>%
  arrange(-lineup)

  


# OEP TEST- create a lineup -------
oep_boombust %>%
  create_custom_boombust_proj(randomness_pct = 1) %>%
  create_boombust_lineup(max_) %>%
  mutate(
    score = sum(fpts_proj),
    total_sal = sum(salary)
  )




# SIM INPUTS -------
N_SIMMED_LINEUPS = 1
N_SIMMED_SLATES = 15
N_SAVED_LINEUPS = N_SIMMED_LINEUPS * N_SIMMED_SLATES



# TESTING Lineup Gen -----
plan("multisession")
tic()
quick_lineups = 
  crossing(slate = 1:N_SIMMED_SLATES) %>%
  mutate(
    slate_lineups = map(slate, ~quickgenerate_lineup(mainslate_boombust, n_lineups = 1) %>% nest(data = everything()))
  ) %>%
  unnest(cols = c(slate_lineups)) %>% unnest(c(data))

toc()

# TESTING BoomBust Long Sim ------

tic()
simmed_slates = crossing(
  slate = 1:N_SIMMED_SLATES
) %>%
  mutate(
    sim_slate =  map(slate, ~(create_custom_boombust_proj(mainslate_boombust, randomness_pct = 1)))
  )

sim_lineups = 
  crossing(slate = 1:N_SIMMED_SLATES) %>%
  mutate(
    slate_lineups = map(slate, ~quickgenerate_lineup(mainslate_boombust, n_lineups = 1) %>% nest(data = everything()))
  ) %>%
  unnest(cols = c(slate_lineups)) %>% unnest(c(data))

lineup_scores = simmed_slates %>%
  rename(x = slate) %>%
  mutate(scores = map(sim_slate, ~stokastic_simjoiner(., lineup_grid = sim_lineups))) %>%
  unnest(scores) %>%
  group_by(slate, lineup) %>%
  select(slate, lineup, lineup_score) %>%
  summarize(lineup_score = mean(lineup_score)) %>%
  arrange(-lineup_score)

top_lineups = lineup_scores %>%
  ungroup() %>%
  mutate(lineup_id = paste0(slate, "_", lineup)) %>%
  top_n(N_SAVED_LINEUPS, lineup_score) %>%
  left_join(
    quick_lineups # %>% select(-lineup_score)
  )

top_exposure = top_lineups %>%
  group_by(player, position, salary, team) %>%
  summarize(exp = n()/N_SAVED_LINEUPS) %>%
  ungroup() %>%
  group_by(position) %>%
  arrange(-exp)
toc()
plan("sequential")



# Charting Long Sim ------

top_exposure %>%
  gt() %>%
  gt::fmt_percent(columns = c(exp))


top_exposure %>%
  group_by(position) %>%
  top_n(4, exp) %>%
  gt() %>%
  gt::fmt_percent(columns = c(exp))






# simmed_lineups = crossing(
#       lineup = 1:N_SIMMED_LINEUPS
#   ) %>%
#     mutate(
#      sim_lineup =  map(lineup, ~(create_custom_boombust_proj(mainslate_boombust, matchups) %>% create_boombust_lineup()))
#     ) %>%
#     unnest() %>%
#     select(-lineups, -exposure)





tic()
test_slate = simmed_slates$sim_slate[[1]]
test_slate %>% create_boombust_lineup(n_lineups = 150)
toc()

# FINAL SIM ------
plan("multisession")
tic()
simmed_slates = crossing(
  slate = 1:N_SIMMED_SLATES
) %>%
  mutate(
    sim_slate =  map(slate, ~(create_custom_boombust_proj(mainslate_boombust, randomness_pct = 1)))
  )
toc()
tic()
simmed_lineups = simmed_slates %>%
  mutate(
    sim_lineup = map(sim_slate, ~create_boombust_lineup(.))
  ) %>%
  unnest(sim_lineup) %>%
  select(-sim_slate) %>%
  rename(slate_lineup = lineup) %>%
  select(
    lineup = slate, everything()
  )
toc()
# tic()
# sim_lineups = 
#   crossing(slate = 1:N_SIMMED_SLATES) %>%
#   mutate(
#     slate_lineups = map(slate, ~quickgenerate_lineup(mainslate_boombust, n_lineups = 1) %>% nest(data = everything()))
#   ) %>%
#   unnest(cols = c(slate_lineups)) %>% unnest(c(data))
# toc()
tic()
lineup_scores = simmed_slates %>%
  rename(x = slate) %>%
  mutate(scores = map(sim_slate, ~stokastic_simjoiner(., lineup_grid = simmed_lineups %>% rename(slate = lineup)))) %>%
  unnest(scores) %>%
  group_by(slate, lineup) %>%
  select(slate, lineup, lineup_score) %>%
  summarize(lineup_score = mean(lineup_score)) %>%
  arrange(-lineup_score)
toc()
tic()
top_exposure = top_lineups %>%
  group_by(player, position, salary, team) %>%
  summarize(exp = n()/N_SAVED_LINEUPS) %>%
  ungroup() %>%
  group_by(position) %>%
  arrange(-exp)
toc()
plan("sequential")




toc()
