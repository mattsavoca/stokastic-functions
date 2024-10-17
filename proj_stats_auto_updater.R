# Get Week and Weekly Sheet -----------
current_week = 22
#weekly_props_ss = "https://docs.google.com/spreadsheets/d/1zL5QMZJu3qzZnspHFRQOhBPQ9KS9IAJmCYuj3PkKFsQ/edit?usp=sharing"
filtered_teams = c("")
matchup = c("KC", "PHI")
theme_set(theme_light())
#matchup = c("")
# Install Packages and Link Filepaths ----------
library(easypackages)
packages("plotly", "jsonlite","googlesheets4", "odds.converter","htmlwidgets", "crosstalk", "nflplotR")
dropbox = ifelse(Sys.info()['sysname'] == "Windows", "C:/Users/matts/dropbox/", "~/Dropbox/")
source(paste0(dropbox, "Matt Savoca/Projects/theme_FE.R"))
projects_folder = paste0(dropbox, "Matt Savoca/Projects/")
week_folder = paste0(projects_folder, "NFL 2022/Week ", 
                     if_else(current_week < 10, "", ""), 
                     as.character(current_week))
downloads_folder = paste0(dropbox, "Matt Savoca/Droploads/")
theme_set(theme_light())

# Get stat projections ----
rec_raw = read_csv(glue("{week_folder}/NFL Receiving Stats.csv")) %>% clean_names()
ru_raw = read_csv(glue("{week_folder}/NFL Rushing Stats.csv")) %>% clean_names()
pa_raw = read_csv(glue("{week_folder}/NFL Passing Stats.csv")) %>% clean_names()

proj_stats = rec_raw %>%
  full_join(ru_raw) %>%
  full_join(pa_raw %>% rename(qb_fum_l = fum_l)) %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  mutate(
    opps = targets + rush,
    yards = rec_yd + rush_yd + pass_yd,
    tds = rec_td + rush_td + touchdowns,
    fum_l = fum_l + qb_fum_l,
  ) %>%
  select(
    name,
    team,
    opps,
    yards,
    tds,
    everything(),
    -qb_fum_l
  )
# Opps Chart ------
opps_chart = proj_stats %>%
  filter(team %in% matchup) %>%  
  mutate(name = reorder(name, opps)) %>%
  ggplot()+
  aes(y= name, x = opps, fill = team)+
  geom_col()+
  scale_fill_nfl(type = "primary")+
  labs(
    title = "Opportunity Chart",
    y= "")

ggplotly(opps_chart)



# Rush Yds Chart ------
ruyds_chart = proj_stats %>%
  filter(team %in% matchup) %>%  
  mutate(name = reorder(name, rush_yd)) %>%
  filter(rush_yd > 0) %>% 
  ggplot()+
  aes(y= name, x = rush_yd, fill = team)+
  geom_col()+
  scale_fill_nfl(type = "primary")+
  labs(
    title = "Rush Yards Chart",
    y= "")

ggplotly(ruyds_chart)

# Shitty Rushing Chart ------
shittyrushing_chart = proj_stats %>%
  filter(team %in% matchup) %>%  
  mutate(
    bad_yards = rush * 3,
    name = reorder(name, bad_yards)) %>%
  filter(bad_yards > 0) %>% 
  ggplot()+
  aes(y= name, x = bad_yards, fill = team)+
  geom_col()+
  scale_fill_nfl(type = "primary")+
  labs(
    title = "Shitty Rushing Chart",
    y= "")

ggplotly(shittyrushing_chart)


# Rec Yds Chart ------
recyds_chart = proj_stats %>%
  filter(team %in% matchup) %>%  
  mutate(name = reorder(name, rec_yd)) %>%
  filter(rec_yd > 0) %>% 
  ggplot()+
  aes(y= name, x = rec_yd, fill = team)+
  geom_col()+
  scale_fill_nfl(type = "primary")+
  labs(
    title = "Rec Yards Chart",
    y= "")

ggplotly(recyds_chart)

# Rec Chart ------
rec_chart = proj_stats %>%
  filter(team %in% matchup) %>%  
  mutate(name = reorder(name, catches)) %>%
  filter(catches > 0) %>% 
  ggplot()+
  aes(y= name, x = catches, fill = team)+
  geom_col()+
  scale_fill_nfl(type = "primary")+
  labs(
    title = "Rec Chart",
    y= "")

ggplotly(rec_chart)


# YFS Chart ------
yds_chart = proj_stats %>%
  filter(team %in% matchup) %>%  
  mutate(name = reorder(name, yards)) %>%
  filter(rec_yd > 0) %>% 
  ggplot()+
  aes(y= name, x = yards, fill = team)+
  geom_col()+
  nflplotR::scale_fill_nfl(type = "primary")+
  labs(
    title = "Yards Chart",
    y= "")

ggplotly(yds_chart)


# proj_stats %>%
#   filter(team %in% matchup) %>%
#   View()





#Blend with DFS Proj ------
proj_stats %>%
  select(-opponent) %>%
  mutate(usg = opps + attempts) %>%
  filter(team %in% matchup) %>% 
  left_join(dk_projections_raw %>% clean_names() %>% select(name, position, team, opponent, salary)) %>% 
  select(name, position, team, opponent, salary, everything()) %>%
  mutate(
    opps_1k = opps/(salary/1000),
    usg_1k = usg/(salary/1000),
  ) %>%
  arrange(-usg_1k)


usg_chart = proj_stats %>%
  select(-opponent) %>%
  mutate(usg = opps + attempts) %>%
  filter(team %in% matchup) %>% 
  left_join(dk_projections_raw %>% clean_names() %>% select(name, position, team, opponent, salary)) %>% 
  select(name, position, team, opponent, salary, everything()) %>%
  mutate(
    opps_1k = opps/(salary/1000),
    usg_1k = usg/(salary/1000),
  ) %>%
  arrange(-usg_1k) %>%
  filter(team %in% matchup) %>%  
  mutate(name = reorder(name, usg_1k)) %>%
  ggplot()+
  aes(y= name, x = usg_1k, fill = team)+
  geom_col()+
  nflplotR::scale_fill_nfl(type = "primary")+
  labs(
    title = "Usage/1k Chart",
    y= "")

ggplotly(usg_chart)
  