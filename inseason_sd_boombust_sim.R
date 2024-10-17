current_week = 3
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

cruncher_sd_proj_raw = read_csv(paste0(week_folder,"/draftkings_showdown_NFL_2022-week-3_players.csv")) %>% clean_names()

# cruncher import WIP -------

##@import cruncher players grid (no header) - to get names, salaries
##prepre cruncher players to join with boom/bust tool (name changes likely)
##generate a valid showdownn lineup



convert_sd_crunch_to_bb = function(df, bb_df){
  cruncher_sd_proj = df %>%
    mutate(
      fc_name = player,
      name = str_remove(player, "- [A-z]+") %>% trimws(),
      opp = str_extract(opp, "[A-Z]+")
    ) 
  cruncher_sd_flex =  cruncher_sd_proj %>%
    filter(pos != "CPTN") %>%
    select(name, fc_name, pos, team, opp, salary)
  
  cruncher_sd_cpt = cruncher_sd_proj %>%
    filter(pos == "CPTN") %>%
    select(name, fc_name, team, opp) %>%
    left_join(cruncher_sd_flex, by = c("name", "team", "opp")) %>%
    select(name, fc_name = fc_name.x, pos, team, opp, salary)
  
  cruncher_sd_df = cruncher_sd_cpt %>%
    rbind(cruncher_sd_flex) %>%
    left_join(
      bb_df %>% select(name, team, opp, pos = position, fpts = projection, std_dev)
    ) %>%
    mutate(
      is_cpt = if_else(str_detect(string = fc_name, pattern = "- CPTN"),1,0),
      std_dev = if_else(is.na(std_dev), 5.5, std_dev),
      std_dev = if_else(is_cpt == 1, 1.5 * std_dev, std_dev),
      salary = if_else(is_cpt == 1, 1.5 * salary, salary),
      fpts = fpts + (.5*is_cpt*fpts),
      value = fpts/(salary/1000)
    )
  
  cruncher_sd_boombust = cruncher_sd_df %>%
    select(
      player_id = name, fc_name, team, opp, salary, pos, projection = fpts, std_dev, is_cpt
    ) %>%
    arrange(-projection) %>%
    mutate(row_id = row_number(),
           salary = as.integer(salary),
           position = if_else(is_cpt > 0, "CPTN", "FLEX")) %>%
    select(row_id, player_id, player = fc_name, salary, team, opp, position, pos, fpts_proj = projection, std_dev) %>%
    left_join(
      cruncher_sd_proj %>%
        select(player_id = player, team, my_proj)
    ) %>%
    mutate(
      fpts_proj = if_else(is.na(fpts_proj), my_proj, fpts_proj)
    ) %>%
    select(-my_proj)
  
  return(cruncher_sd_boombust)
}

create_custom_sd_proj = function(df, randomness_pct = 0.0, matchup){
  suppressMessages({
    
    test_df_raw = df
    
    test_df = test_df_raw %>%
      rename(projection = fpts_proj) %>%
      filter(team %in% matchup) %>%
      mutate(
        std_dev = case_when(
          pos == "K" ~ 5.5,
          projection > 0 & is.na(std_dev) ~ 5.0,
          projection <= 0 ~ 0.0,
          T~std_dev))
    
    
    cpt_df = test_df %>%
      filter(position == "CPTN") %>%
      select(-std_dev, -projection)
    
    flex_df = test_df %>%
      filter(position != "CPTN")
    
    
    
    sim_df = flex_df %>%
      rowwise() %>%
      mutate(
        projection = rnorm(1, projection, (std_dev + (randomness_pct*std_dev))),
        projection = max(-1, projection)
      ) %>%
      ungroup()
    
    
    qbrb_df = sim_df %>%
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
    
    wr_df = sim_df %>%
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
    
    te_df = sim_df %>%
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
    
    dst_df = sim_df %>%
      mutate(team_pct = 1) %>%
      left_join(
        qbrb_df  %>% rename(team = opp, team_orig = team) %>% filter(pos == "QB") %>% group_by(team) %>% summarize(qb_proj = sum(projection[projection > 0]))
      ) %>%
      rowwise() %>%
      filter(pos == "DST", team != "FA") %>%
      mutate(
        new_proj = (qb_proj * -.17 + (if_else(projection<0,0,projection)*1.17))
      ) %>% 
      arrange(-new_proj)
    
    k_df = sim_df %>%
      filter(pos != "QB") %>%
      left_join(
        qbrb_df %>% filter(pos == "QB") %>% group_by(team) %>% summarize(qb_proj = sum(projection[projection > 0]))
      ) %>%
      mutate(team_pct = 1) %>%
      rowwise() %>%
      filter(pos == "K", team != "FA") %>%
      mutate(
        new_proj = (qb_proj *.1 + projection*.9)
      ) %>% 
      arrange(-new_proj)
    
    
    proj_df = qbrb_df %>%
      rbind(wr_df, te_df, dst_df, k_df) %>%
      arrange(-new_proj) %>%
      select(
        row_id, player_id, player,salary, team, opp, position, pos, fpts_proj = new_proj,
      ) 
    
    full_proj_df = proj_df %>%
      full_join(cpt_df) %>%
      group_by(player_id) %>%
      mutate(
        fpts_proj = mean(fpts_proj, na.rm = T)
      ) %>%
      rowwise() %>%
      mutate(
        fpts_proj = if_else(position == "CPTN", 1.5 * fpts_proj, fpts_proj),
      ) %>%
      ungroup() %>%
      mutate(
        row_id = row_number(),
        salary = as.integer(salary),
        value = fpts_proj/(salary/1000),
        new_pos = if_else(position == "CPTN", "CPTN", pos)
      ) %>%
      select(row_id, player_id, player, team, position, pos,
             salary, fpts_proj) 
    
    
  })
  
  return(full_proj_df)
}

generate_sd_lineup = function(full_proj_df, n_lineups = 1, ...){
  sd_model <- model_generic(full_proj_df, 
                            total_salary = 50000, 
                            roster_size = 6, max_from_team = 5)
  
  sd_constraints <- list(
    "CPTN" = 1,
    "FLEX" = 5
  )
  
  sd_model <- add_generic_positions_constraint(sd_model, full_proj_df, sd_constraints)
  
  sd_results = optimize_generic(full_proj_df, sd_model, L = n_lineups)
  
  data.table::rbindlist(sd_results, idcol = "lineup")
}

cruncher_sd_boombust = cruncher_sd_proj_raw %>%
  convert_sd_crunch_to_bb(boom_bust)  

cruncher_sd_NAs = cruncher_sd_boombust %>%
  filter(is.na(std_dev))

cruncher_sd_boombust %>% 
  create_custom_sd_proj(matchup  = c("DAL", "NYG")) %>% 
  filter(team %in% c("DAL", "NYG")) %>% mutate(row_id = row_number()) %>%
  generate_sd_lineup()

SLATES = 25
LINEUPS = 5
MATCHUP = c("DAL", "NYG")

tic()
slates = crossing(
  slate_num = 1:SLATES,
) %>%
  mutate(proj = map(slate_num, ~create_custom_sd_proj(df = cruncher_sd_boombust, randomness_pct = 10, matchup = MATCHUP))) %>%
  mutate(lineup = map(proj, ~generate_sd_lineup(full_proj_df = ., n_lineups = LINEUPS)))
toc()

top_players = slates %>% 
  unnest(lineup) %>% 
  select(
    -proj,
  ) %>%
  mutate(total = n()) %>%
  group_by(player_id, player, pos, position) %>%
  summarize(
    total = SLATES*LINEUPS,
    n = n(),
    pct=  n/total) %>%
  group_by(player_id) %>%
  mutate(
    player_n = sum(n),
    player_pct = player_n/total[1]
  ) %>%
  arrange(-player_pct, -pct)  %>%
  filter(position != "CPTN") %>%
  select(
    player, pos, player_n, player_pct
  )


top_plays = slates %>% 
  unnest(lineup) %>% 
  select(
    -proj,
  ) %>%
  mutate(total = n()) %>%
  group_by(player_id, player, pos, position) %>%
  summarize(
    total = SLATES*LINEUPS,
    n = n(),
    pct=  n/total) %>%
  select(
    player_id, position, pos, pct
  ) %>%
  arrange(-pct)










### ------ 


sd_result_df = cruncher_sd_boombust %>%
  filter(team %in% matchup) %>%
  generate_sd_lineup(n_lineups = 1)

full_proj_df %>%
  generate_sd_lineup(n_lineups = 1)

sd_result_df %>%
  group_by(player_id, pos) %>%
  summarize(
    n = n(),
    pct = n/max(lineup)
  ) %>%
  arrange(-pct) %>%
  group_by(pos) %>%
  gt() %>%
  gt::fmt_percent(pct)







