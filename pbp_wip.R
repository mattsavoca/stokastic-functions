#install.packages("easypackages")
#devtools::install_github("liamgilbey/ggwaffle")
library("easypackages")
packages("nflreadr", "tidyverse", "tidymodels", "ggthemes", "nflplotR", "ggwaffle", "ggcorrplot", "zoo")

#theme adjustments
theme_savoca = function (base_size = 12, base_family = "sans"){
  (theme_foundation(base_size = base_size, base_family = base_family) + 
     theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"), 
           axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"), 
           axis.text = element_text(size = ceiling(base_size * 0.7), colour = "black"), 
           axis.title = element_text(size = ceiling(base_size * .95), face = "bold"), 
           panel.grid.minor = element_blank(), 
           panel.grid.major.y = element_line(colour = "white",linetype = "dotted"), 
           panel.grid.major.x = element_blank(), 
           panel.background = element_blank(), 
           panel.border = element_blank(), 
           strip.background = element_rect(linetype = 0), 
           strip.text = element_text(), 
           strip.text.x = element_text(vjust = 0.5), 
           strip.text.y = element_text(angle = -90), 
           legend.text = element_text(size = ceiling(base_size * 0.9), family = "sans"), 
           legend.title = element_text(size = base_size, face = "bold", family = "sans"), 
           legend.position = "right", 
           legend.key = element_rect(fill = "white", colour = NA), 
           legend.background = element_rect(colour = "black"), 
           plot.background = element_rect(colour = "black"), 
           plot.title = element_text(size = ceiling(base_size * 1.6), face = "bold"), 
           plot.subtitle = element_text(size = ceiling(base_size * 1.05))))
}
theme_set(theme_savoca())
SEASONS = 1999:2022

# Load Plays ------

nfl_pbp_raw = load_pbp(seasons = SEASONS)


# -----

gbg_qbs = nfl_pbp_raw %>% 
  filter(!is.na(score_differential),
         season_type == "REG",
         !is.na(posteam) & posteam != "",
         #play_type %in% c("run", "pass")
         qb_dropback == 1
         ) %>%
  group_by(game_id, posteam, season, passer_player_name, passer_player_id) %>%
  summarize(
    dropbacks = n()
  ) %>%
  group_by(game_id, posteam, season) %>%
  top_n(1, dropbacks) %>%
  rename(primary_passer_name = passer_player_name, primary_passer_id = passer_player_id) 


primary_passers = nfl_pbp_raw %>%
  filter(!is.na(score_differential),
         season_type == "REG",
         !is.na(posteam) & posteam != "",
         #play_type %in% c("run", "pass")
         qb_dropback == 1
  ) %>%
  left_join(gbg_qbs) %>%
  select(game_id, posteam, season, week, primary_passer_name, primary_passer_id) %>%
  distinct() %>%
  group_by(primary_passer_name, primary_passer_id) %>%
  arrange(-season, -week) %>%
  mutate(primary_passer_game_recency = row_number()) %>%
  ungroup()

# spreads/totals correlations to team plays ----
primary_passer_epa = nfl_pbp_raw %>%
  filter(!is.na(score_differential),
         season_type == "REG",
         !is.na(posteam) & posteam != "",
         qb_dropback == 1, 
         !is.na(epa)) %>%
  left_join(primary_passers) %>%
  filter(!is.na(primary_passer_name)) %>%
  group_by(primary_passer_name, primary_passer_id) %>% 
  arrange(primary_passer_name, primary_passer_id,-season, -week, -play_id) %>%
  select(game_id, contains("primary"), epa, play_id) %>% 
  mutate(
    roll_5_epa = rollapply(epa, 5, mean, na.rm = T, by = 1, partial = T, fill = NA),
    roll_10_epa = rollapply(epa, 10, mean, na.rm = T, by = 1, partial = T, fill = NA),
    roll_25_epa = rollapply(epa, 25, mean, na.rm = T, by = 1, partial = T, fill = NA),
    roll_50_epa = rollapply(epa, 50, mean, na.rm = T, by = 1, partial = T, fill = NA),
  ) %>%
  group_by(primary_passer_name, primary_passer_id, game_id) %>%
  top_n(1, play_id) %>%
  ungroup()
  

nfl_gbg = nfl_pbp_raw %>% 
  filter(!is.na(score_differential),
         season_type == "REG",
         !is.na(posteam) & posteam != "",
         play_type %in% c("run", "pass")) %>%
  group_by(game_id, posteam, season) %>%
  summarize(
    home_team = home_team[1],
    away_team = away_team[1],
    spread_line = spread_line[1],
    total_line = total_line[1],
    off_plays = n(),
    passes = n_distinct(play_id[play_type == "pass"]),
    rushes = off_plays - passes
  ) %>%
  ungroup() %>%
  mutate(
    off_spread = if_else(posteam == home_team, spread_line, spread_line * -1),
    off_implied = total_line/2 + off_spread/2
  ) %>%
  left_join(primary_passer_epa) %>%
  select(-spread_line) %>%
  rename(off_team = posteam) %>%
  glimpse()

  
  


nfl_gbg %>%
  ggplot() +
  aes(x = total_line, y = passes) +
  geom_jitter()+
  geom_smooth()

gbg_corr = nfl_gbg %>% filter(season > 2007) %>% select(off_plays, contains("roll")) %>% cor() %>% round(2)

gbg_corr %>% ggcorrplot()

# Load Rosters ------

nfl_rosters_raw = load_rosters(seasons = SEASONS)

# QB play by season ----
season_qb_eff = nfl_pbp_raw %>%
  filter(season_type == "REG", qb_dropback == 1) %>%
  left_join(nfl_rosters_raw %>% rename(passer_id = gsis_id) %>% select(-jersey_number)) %>%
  group_by(passer_id, season) %>%
  mutate(passer_season_dropbacks = n()) %>%
  filter(passer_season_dropbacks >= 25) %>%
  group_by(season) %>%
  summarize(
    passers = n_distinct(passer_id),
    adj_passers = passers/n_distinct(week)*18 %>% round(1),
    dropbacks = n(),
    aya = mean(yards_gained,na.rm = T),
    ep = mean(ep, na.rm = T),
    epa = mean(epa, na.rm = T) %>% round(3)
  ) %>%
  select(-passers) %>%
  ungroup() %>%
  arrange(-aya) 

season_qb_eff  %>% View(.)
  

nfl_pbp_raw %>%
  filter(season_type == "REG", qb_dropback == 1) %>%
  left_join(nfl_rosters_raw %>% rename(passer_id = gsis_id) %>% select(-jersey_number)) %>%
  group_by(passer_id, season) %>%
  mutate(passer_season_dropbacks = n()) %>%
  filter(passer_season_dropbacks >= 25, !is.na(full_name)) %>%
  left_join(season_qb_eff %>% select(season, season_epa = epa)) %>%
  group_by(passer_id, full_name) %>%
  summarize(
    dropbacks = n(),
    season_sr = n()[epa>max(season_epa)]/dropbacks,
    aya = mean(yards_gained,na.rm = T),
    ep = mean(ep, na.rm = T),
    epa = mean(epa, na.rm = T) %>% round(3)
  ) %>%
  ungroup() %>%
  arrange(-aya) %>%
  View(.)
  

# Red Zone Ep ------

nfl_pbp_raw %>%
  filter(
    yardline_100 <= 20, 
    play_type %in% c("pass", "run", "field_goal")
  ) %>%
  group_by(
    posteam, season, game_id, drive
  ) %>%
  summarize(
    plays = n(),
    drives = 1,
    ep = max(ep, na.rm = T),
    epa = sum(epa, na.rm = T)
  ) %>%
  group_by(
    posteam, season
  ) %>%
  summarize(
    drives = sum(drives),
    plays = sum(plays),
    epa = sum(epa),
    epa_play = epa/plays,
    ep = sum(ep),
    ep_rzdrive = ep/drives
  ) %>%
  filter(season == 2022) %>%
  arrange(-ep) %>%
  View()
  
  
# Most Common Score Differentials ------

nfl_pbp_raw %>%
  filter(!is.na(score_differential),
         season_type == "REG") %>%
  select(game_id, play_id, score_differential) %>%
  group_by(game_id) %>%
  mutate(last_play = max(play_id)) %>%
  filter(play_id == last_play) %>%
  group_by(score_differential) %>%
  summarize(
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    pct = n/sum(n)
  ) %>%
  arrange(-pct) %>%
  mutate(
    pct = round(pct*100, 1),
    sum = cumsum(pct)
  ) %>%
  gt()


ggplotly(
  nfl_pbp_raw %>%
    filter(!is.na(total_line),
           season_type == "REG") %>%
    select(game_id, play_id, total, total_line) %>%
    group_by(game_id) %>%
    mutate(last_play = max(play_id)) %>%
    filter(play_id == last_play) %>%
    arrange(-total_line) %>%
    group_by(total_line) %>%
    summarise(
      n = n(),
      lower = quantile(total, prob = .2),
      median = quantile(total, prob = .5),
      upper = quantile(total, prob = .8),
    ) %>%
    ungroup() %>%
    filter(n > 10) %>%
    mutate(total_line = reorder(total_line, median)) %>%
    ggplot()+
    aes(
      xmin = lower, x = median, xmax = upper, y = total_line
    ) +
    geom_errorbarh()+
    geom_point(size = 1.5)+
    theme_FE+
    theme(
      legend.position = 'none'
    )
)    

# Game Range of Outcomes by Total ------

ggplotly(
  nfl_pbp_raw %>%
    filter(!is.na(total_line),
           season_type == "REG") %>%
    select(game_id, play_id, total, total_line) %>%
    group_by(game_id) %>%
    mutate(last_play = max(play_id)) %>%
    filter(play_id == last_play) %>%
    arrange(-total_line) %>%
    group_by(total_line) %>%
    mutate(n = n(),
           med = quantile(total, .5),
           upper = quantile(total, .975),
           lower = quantile(total, .025)) %>%
    ungroup() %>%
    mutate(total_line = reorder(total_line, -total_line)) %>%
    filter(n > 10) %>%
    ggplot()+
    aes(
      x = total, 
    ) +
    geom_density(fill = "navy", alpha = .5)+
    geom_vline(aes(xintercept = med, group = total_line))+
    geom_vline(aes(xintercept = upper, group = total_line), lty = 2)+
    geom_vline(aes(xintercept = lower, group = total_line), lty = 2)+
    scale_x_continuous(breaks = seq(0, 99, 6))+
    coord_cartesian(xlim = c(0, 99))+
    facet_wrap(total_line ~ .)+
    theme_FE
)
  

## open source EP -----
#remotes::install_github("ffverse/ffopportunity")
packages("ffopportunity")


ep_df_raw = ep_load(season = 2022, type = "weekly")

ggplotly(
  ep_df_raw  %>% 
    filter(full_name %in% c("Devin Singletary", "AJ Dillon", "A.J. Dillon", 
                            "Kareem Hunt", "Raheem Mostert",
                            "Travis Etienne", 
                            "Saquon Barkley", "Nick Chubb", "Christian McCaffrey")) %>%
    mutate(
      line_color = case_when(
        full_name %in% c("Nick Chubb", "Saquon Barkley", "Christian McCaffrey") ~ "gold",
        full_name %in% c("Devin Singletary") ~ "black",
        T ~ "green"
      )
    ) %>%
    mutate(oep = total_fantasy_points_exp) %>%
    ggplot()+
    aes(
      y = oep, x = week, color = line_color, 
    )+
    geom_line(aes(group = full_name))+
    geom_point(aes(label = full_name), size = 4)+
    geom_smooth(aes(y = oep, x = week), color = "grey", lty = 2, se =F)+
    scale_color_identity()

)

ep_df_raw2 = ep_load(season = 2021:2022, type = "weekly")

oep_df = ep_df_raw2 %>%
  group_by(player_id) %>%
  mutate(season = as.numeric(season)) %>%
  arrange(
    -season, -week
  ) %>%
  mutate(
    game_recency = row_number()
  ) %>%
  filter(
    game_recency <= 6
  ) %>%
  ungroup() %>% 
  rowwise() %>%
  mutate(
    total_dk_bonus_fantasy_points_exp = 
      ((pass_yards_gained_exp/300 * 3) + (rec_yards_gained_exp/100 * 3) +
         (rush_yards_gained_exp/100 * 3)),
  ) %>%
  ungroup() %>%
  mutate(
    total_fantasy_points_exp = total_fantasy_points_exp + total_dk_bonus_fantasy_points_exp
  ) %>%
  select(season, week, player_id, full_name, position, posteam, total_fantasy_points_diff, contains(c(
    #"touchdown_exp",
    "fantasy_points_exp")),
    -contains(c(
      "_exp_team"
    ))) %>%
  group_by(
    player_id, full_name, position
  ) %>%
  summarize(
    posteam = posteam[1],
    oep = mean(total_fantasy_points_exp, na.rm = T)
  ) %>%
  mutate_if(
    is.numeric,round
  ) %>%
  select(player_id, name = full_name, team = posteam, position, oep) %>%
  arrange(-oep) %>%
  mutate(name = nflfastr_to_stokastic_namechange(name),
         team = nflfastr_to_stokastic_teamchange(team),
         position = if_else(
           name == "Taysom Hill", "TE", position
         ))




 nflfastr_to_stokastic_teamchange = function(x){
  name = if_else(x == "LA", "LAR", x)
  return(name)
}


nflfastr_to_stokastic_namechange = function(x){
  name = gsub("Allen Robinson", "Allen Robinson II", x)
  name = gsub("Allen Robinson II II", "Allen Robinson II", name)
  name = gsub("Darrell Henderson", "Darrell Henderson Jr.", name)
  name = gsub("D.J. Moore", "DJ Moore", name)
  name = gsub("Robby Anderson", "Robbie Anderson", name)
  name = gsub("Marvin Jones", "Marvin Jones Jr.", name)
  name = gsub("D.J. Chark", "DJ Chark Jr.", name)
  name = gsub("Travis Etienne", "Travis Etienne Jr.", name)
  name = gsub("Melvin Gordon", "Melvin Gordon III", name)
  name = gsub("Cedrick Wilson", "Cedrick Wilson Jr.", name)
  name = gsub("Jeffery Wilson", "Jeff Wilson Jr.", name)
  name = gsub("Jeffrey Wilson", "Jeff Wilson Jr.", name)
  name = gsub("Mark Ingram", "Mark Ingram II", name)
  name = gsub("Mark Ingram II II", "Mark Ingram II", name)
  name = gsub("Richie James", "Richie James Jr.", name)
  name = gsub("Kenneth Walker", "Kenneth Walker III", name)
  name = gsub("Kenneth Walker III III", "Kenneth Walker III", name)
  return(name)
}

oep_df = ep_df_raw %>% 
  rowwise() %>%
  mutate(
    total_dk_bonus_fantasy_points_exp = 
      ((pass_yards_gained_exp/300 * 3) + (rec_yards_gained_exp/100 * 3) +
      (rush_yards_gained_exp/100 * 3)),
  ) %>%
  ungroup() %>%
  mutate(
    total_fantasy_points_exp = total_fantasy_points_exp + total_dk_bonus_fantasy_points_exp
  ) %>%
  select(season, week, player_id, full_name, position, posteam, total_fantasy_points_diff, contains(c(
    #"touchdown_exp",
    "fantasy_points_exp")),
    -contains(c(
      "_exp_team"
    ))) %>% 
  mutate_if(
    is.numeric,round
  ) %>%
  select(player_id, name = full_name, position, team = posteam, oep = total_fantasy_points_exp) %>%
  arrange(-oep) %>%
  mutate(name = nflfastr_to_stokastic_namechange(name),
         team = nflfastr_to_stokastic_teamchange(team),
         position = if_else(
           name == "Taysom Hill", "TE", position
         ))

nfl_rosters = load_rosters(seasons = 2022) %>%
  select(player_id = gsis_id,name = full_name, position, team
         ) %>%
  mutate(name = nflfastr_to_stokastic_namechange(name),
         team = nflfastr_to_stokastic_teamchange(team),
         position = if_else(
           name == "Taysom Hill", "TE", position
         ))



new_opp_crew = c("Jeff Wilson Jr.")


oep_boombust = mainslate_boombust %>%
  left_join(oep_df) %>%
  left_join(nfl_rosters, by = c("name", "team", "position")) %>%
  mutate(player_id = if_else(is.na(player_id.x), player_id.y, player_id.x))%>%
  select(-contains(c(".x", ".y"))) %>%
  mutate(
    row_id = row_number(),
    oep = if_else(is.na(oep), round(projection), oep),
    player_id = if_else(position == "DST", paste0("00-",(as.factor(name) %>% as.numeric() %>% as.character())), player_id)
  ) %>% 
  rowwise() %>%
  mutate(new_proj = projection * .85 +  oep * .15) %>%
  select(name, team, opp, salary, position, projection = new_proj, std_dev)

# Load Game Stats -------
game_stats_raw = load_player_stats(seasons = SEASONS)

# 2022 Playoffs
game_stats_raw %>%
  arrange(-season, -week) %>%
  filter(!is.na(player_name), season_type == "POST", season == 2022, recent_team %in% c("PHI", "KC")) %>%
  mutate(across(c(passing_epa, rushing_epa, receiving_epa), replace_na,0)) %>%
  group_by(player_id, position, season) %>%
  summarize(
    recent_team = recent_team[1],
    player_name = player_display_name[1],
    games = n(),
    targets = sum(targets)/games,
    rushes = sum(carries)/games,
    passes = sum(attempts)/games,
    epa  = (sum(passing_epa) + sum(rushing_epa) + sum(receiving_epa))/games,
    opps = targets + rushes + passes,
    touchdowns = sum(receiving_tds) + sum(rushing_tds) + sum(passing_tds),
    tds_opp = touchdowns/(opps*games),
    touchdowns = touchdowns/games,
    epa_play = epa/opps
  ) %>% 
  ungroup() %>%
  arrange(-epa_play) %>%
  filter(opps > 3)  %>% View(.)
  group_by(recent_team) %>%
  summarize(
    opps = sum(opps),
    epa = sum(epa),
    epa_play = mean(epa_play)
  ) %>%
  arrange(-epa)

  
  
# Tds - Opp ----


tds_opp_df = game_stats_raw %>%
  arrange(-season, -week) %>%
  filter(!is.na(player_name), season_type == "REG") %>%
  group_by(player_id, position, season) %>%
  summarize(
    recent_team = recent_team[1],
    player_name = player_display_name[1],
    games = n(),
    targets = sum(targets),
    rushes = sum(carries),
    passes = sum(attempts),
    opps = targets + rushes + passes,
    touchdowns = sum(receiving_tds) + sum(rushing_tds) + sum(passing_tds),
    tds_opp = touchdowns/opps
  ) %>%
  ungroup() %>%
  arrange(-tds_opp)

alt_tds_opp_df = game_stats_raw %>%
  arrange(-season, -week) %>%
  filter(!is.na(player_name), season_type == "REG") %>%
  mutate(across(c(passing_epa, rushing_epa, receiving_epa), replace_na,0)) %>%
  group_by(player_id, position, season) %>%
  summarize(
    recent_team = recent_team[1],
    player_name = player_display_name[1],
    games = n(),
    targets = sum(targets)/games,
    rushes = sum(carries)/games,
    passes = sum(attempts)/games,
    epa  = (sum(passing_epa) + sum(rushing_epa) + sum(receiving_epa))/games,
    opps = targets + rushes + passes,
    touchdowns = sum(receiving_tds) + sum(rushing_tds) + sum(passing_tds),
    tds_opp = touchdowns/(opps*games),
    touchdowns = touchdowns/games,
  ) %>%
  ungroup() %>%
  arrange(-tds_opp)

# Top EPA Performers ------
retiree_list = c(
  "Andrew Luck",
  "Drew Brees",
  "Philip Rivers",
  "Antonio Brown",
  "Rob Gronkowski",
  "Emmanuel Sanders"
)

top_epa_df = alt_tds_opp_df %>%
  select(player_id, team = recent_team, player_name, position, season, games, opps, epa, touchdowns) %>%
  arrange(-season) %>%
  filter(season >= 2021, season <= 2022) %>%
  group_by(player_id, player_name, position) %>%
  summarize(
    team = team[1],
    games = sum(games),
    td_opp = sum(touchdowns)/sum(opps),
    opps = mean(opps, na.rm = T),
    epa = mean(epa, na.rm = T),
    epa_17 = epa * 17
  ) %>%
  ungroup() %>%
  arrange(-epa_17) 

#EPA Charts--------

top_epa_df %>%
  filter(
    (games >= 20 & opps >= 4) | player_name == "Brock Purdy",
    !player_name %in% retiree_list
    ) %>%
  mutate(
    player_label = paste0(player_name, " - ", position),
    player_label = reorder(player_label, epa_17),
    new_alpha = if_else(player_name %in% retiree_list, max(epa_17), epa_17)) %>%
  filter(position %in% c("TE", "RB","WR", "QB"), epa > 0) %>%
  top_n(25, epa_17) %>%
  ggplot()+
  aes(
    x = epa_17, y = player_label, fill = team
  ) + 
  geom_col(show.legend = F, aes(alpha = new_alpha)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.025) +
  scale_fill_nfl()+
  labs(
    y = "",
    title = "Expected Points Added Per Season since 2020",
    subtitle = "EPA Per Game Extrapolated to a 17 Game Pace (20 Game Minimum)",
    caption = "Data: @nflfastR",
    x = "Expected Points Added per 17 Games"
  )+
  theme_savoca(base_size = 16)+
  scale_x_continuous(breaks = seq(0, 225, 25))


top_epa_df %>%
  filter(
    (games >= 16 & opps >= 5)
    #!player_name %in% retiree_list
  ) %>%
  mutate(player_label = paste0(player_name, " - ", position)) %>% 
  filter(position == "QB") %>%
  top_n(25, td_opp) %>%
  mutate(player_label = reorder(player_label, td_opp),
         new_alpha = if_else(player_name %in% retiree_list, max(td_opp), td_opp)) %>%
  ggplot()+
  aes(
    x = td_opp, y = player_label, fill = team
  ) + 
  geom_col(show.legend = F, aes(alpha = new_alpha)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.025) +
  scale_x_continuous(breaks = seq(0, 0.08, 0.005), labels = percent_format())+
  scale_fill_nfl()+
  labs(
    y = "",
    title = "Touchdown Rate",
    subtitle = "",
    caption = "Data: @nflfastR",
    x = "Touchdowns Per Opportunity (Rush + Dropbox)"
  )+
  theme_savoca(base_size = 16)



# Touchdown Over-Acheivers -------


alt_tds_opp_df %>%
  filter(opps > 5, position == "WR", games > 2) %>%
  arrange(-tds_opp) %>%
  top_n(25) %>%
  select(player_name, position, games, season, tds_opp) %>%
  gt() %>%
  fmt_percent(columns = tds_opp, decimals = 1)

alt_tds_opp_df %>%
  mutate(
    season_n_1 = season + 1
  ) %>%
  select(player_id, position, player_name, games, season_n_1) %>%
  left_join(
    alt_tds_opp_df %>% select(player_id, position, player_name, season_n_1 = season, season_n_1_opps = opps,season_n_1_tds_opp = tds_opp)
  ) %>%
  mutate(season = season_n_1 -1) %>%
  select(player_id, position, player_name, season, season_n_1_opps, season_n_1_tds_opp) %>%
  left_join(alt_tds_opp_df) %>%
  arrange(-tds_opp) %>%
  select(player_id, position, player_name, season, targets, rushes, passes, opps, games, touchdowns, tds_opp, season_n_1_opps, season_n_1_tds_opp) %>%
  rowwise() %>%
  mutate(season_n_1_diff = season_n_1_tds_opp - tds_opp,
         season_n_1_opp_diff = season_n_1_opps - opps) %>%
  ungroup() %>%
  #arrange(-season_n_1_opp_diff) %>%
  mutate(range = if_else(season == "2022", "2022-",paste0(season, "-", season+1))) %>%
  select(player_name, position, range, season, games, opps, tds_opp, season_n_1_opp_diff, season_n_1_diff) %>%
  filter(tds_opp >= .10, position %in% c("WR", "TE", "RB"), player_name != "Tim Tebow", 
         season >= 2015, season <= 2022,
         #season == 2022,
         opps >= 5, games > 1) %>%
  select(-season) %>%
  group_by(position) %>%
  #top_n(60, tds_opp) %>%
  gt(rowname_col = "player_name") %>%
  fmt_number(columns = contains(c("opps", "n_1_opp_diff")), decimals = 1) %>%
  fmt_percent(columns = contains(c("season_n_1_diff", "tds_opp")), decimals = 1) %>%
  sub_missing(columns = contains(c("season_n_1_diff", "season_n_1_opp_diff")), missing_text = "") %>%
  summary_rows(groups = T, columns = contains(c("season_n_1_diff", "tds_opp")), 
               fns = list(Averages = ~mean(., na.rm = T)), formatter = fmt_percent, decimals = 1) %>%
  summary_rows(groups = T, columns = contains(c("season_n_1_opp_diff")), 
               fns = list(Averages = ~mean(., na.rm = T)), formatter = fmt_number, decimals = 1) %>%
  cols_label(player_name = "Name", position = "Pos", range = "Seasons", opps = "Opportunities/Game", tds_opp = "TD%", 
             season_n_1_opp_diff = "YOY Change in Opps.",season_n_1_diff = "YOY Change in TD%") %>%
  gt::cols_align(columns = range, align = "left") %>%
  tab_header(
    title = "Touchdown Over-Achievers",
    subtitle = "Opportunities = Targets + Rushes + Passes"
  )



alt_tds_opp_df %>%
  mutate(
    season_n_1 = season + 1
  ) %>%
  select(player_id, position, player_name, games, season_n_1) %>%
  left_join(
    alt_tds_opp_df %>% select(player_id, position, player_name, season_n_1 = season, season_n_1_opps = opps,season_n_1_tds_opp = tds_opp)
  ) %>%
  mutate(season = season_n_1 -1) %>%
  select(player_id, position, player_name, season, season_n_1_opps, season_n_1_tds_opp) %>%
  left_join(alt_tds_opp_df) %>%
  arrange(-tds_opp) %>%
  select(player_id, position, player_name, season, targets, rushes, passes, opps, games, touchdowns, tds_opp, season_n_1_opps, season_n_1_tds_opp) %>%
  rowwise() %>%
  mutate(season_n_1_diff = season_n_1_tds_opp - tds_opp,
         season_n_1_opp_diff = season_n_1_opps - opps) %>%
  ungroup() %>%
  #arrange(-season_n_1_opp_diff) %>%
  mutate(range = if_else(season == "2022", "2022-",paste0(season, "-", season+1))) %>%
  filter(games > 1, opps >= 3, tds_opp > .1) %>%
  ggplot()+
  aes(
    x = season_n_1_diff, fill = position
  )+
  geom_density(alpha = .5)+
  geom_vline(xintercept = 0, lty = 2)+
  theme_light()
  
  

  

# 