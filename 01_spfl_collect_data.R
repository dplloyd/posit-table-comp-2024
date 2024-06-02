# Script for calculating the statistics to feed into the table

library(tidyverse)

# Read data downloaded from https://www.football-data.co.uk/mmz4281/2324/SC0.csv
df <- read.csv('data/SC0.csv')

# Rename the variables to something more human readable
df <- df |>
  select(
    div = Div,
    date = Date,
    time = Time,
    home_team = HomeTeam,
    away_team = AwayTeam,
    home_ft_goals =  FTHG,
    away_ft_goals =  FTAG,
    fulltime_result = FTR,
    home_ht_goals =  HTHG,
    away_ht_goals =   HTAG,
    halftime_result =    HTR,
    ref =  Referee,
    home_shots =  HS,
    away_shots =  AS,
    home_shots_on_target =  HST,
    away_shots_on_target =  AST,
    home_corners = HC,
    away_corners =  AC,
    home_fouls = HF,
    away_fouls =  AF,
    home_yellows =  HY,
    away_yellows =  AY,
    home_reds =  HR,
    away_reds = AR
  )


# Build a slightly longer data set, where we have a new variable tracking the location
# I'm sure there's a way in pivot_longer to do this. EDIT - there is; to add Martin's suggestion

away_df <- select(df, div, date, time, contains("away")) |>
  rename_with( ~ str_remove(., 'away_')) |>
  mutate(location = "away")

home_df <- select(df, div, date, time, contains("home")) |>
  rename_with( ~ str_remove(., 'home_')) |>
  mutate(location = "home")

# The final dataset to work with
spfl <- rbind(away_df, home_df) |>
  select(div, date, time, team, location, everything()) |>
  arrange(team)


## CALCULATIONS of interest ----

### SWING WHEN YER MINGIN: Games won from half-time (and vice versa) ----
df <- df |>
  mutate(
    game_won_FT_from_non_win_HT =
      case_when(
        halftime_result != "A" & fulltime_result == "A" ~ away_team,
        halftime_result != "H" &
          fulltime_result == "H" ~ home_team,
        TRUE ~ NA
      ),
    game_lost_FT_from_non_losing_HT =
      case_when(
        halftime_result != "H" & fulltime_result == "H" ~ away_team,
        halftime_result != "A" &
          fulltime_result == "A" ~ home_team,
        TRUE ~ NA
      )
  )

# Final swing dataset
swing_counts <-
  df |> pivot_longer(
    cols = c(
      game_won_FT_from_non_win_HT,
      game_lost_FT_from_non_losing_HT
    ),
    names_to = "swing",
    values_to = "team"
  ) |>
  group_by(swing) |>
  count(team) |> 
  pivot_wider(names_from = swing, values_from = n)

### WIN LOSS DRAW COUNTS ----
# Win loss draw points won, by location (home/away) and total
wld_points <- df |>
  select(home_team, away_team, fulltime_result) |>
  pivot_longer(
    cols = away_team:home_team,
    # Select all columns to pivot
    names_to = c("location", "var"),
    # New variable names for parts of the original names
    names_sep = "_",
    values_to = "team"
  ) |>
  mutate(
    points = case_when(
      location == "away" & fulltime_result == "A" ~ 3,
      location == "home" & fulltime_result == "H" ~ 3,
      fulltime_result == "D" ~ 1,
      TRUE ~ 0
    )
  ) |>
  group_by(team, location) |>
  summarise(total_points = sum(points)) |> 
  pivot_wider(names_from = location, values_from = total_points, names_glue = "{location}_points" ) |> 
  mutate(total_points = away_points + home_points)

wld_count <- df |>
  select(home_team, away_team, fulltime_result) |>
  pivot_longer(
    cols = away_team:home_team,
    # Select all columns to pivot
    names_to = c("location", "var"),
    # New variable names for parts of the original names
    names_sep = "_",
    values_to = "team"
  ) |>
  
  group_by(team) |>
  summarise(
    n_win = sum(
      (fulltime_result == "H" &
         location == "home") |
        fulltime_result == "A" & location == "away"
    ),
    n_draw = sum(fulltime_result == "D"),
    n_loss = sum(
      (fulltime_result == "H" &
         location == "away") |
        fulltime_result == "A" & location == "home"
    )
  ) 


# WLD lists for fancy sparklines
wld_sparks <- df |> 
  select(date, time,team_home = home_team, team_away = away_team, fulltime_result) |> 
pivot_longer(contains('team'), names_to = 'home_away', values_to = 'team', names_prefix = 'team_') |> 
  mutate(
  result = case_when(home_away == 'home' & fulltime_result == "H" ~ 1,
                     home_away == 'away' & fulltime_result == "A" ~  1,
                     fulltime_result == "D" ~  0.5,
                     TRUE ~  -0)) |> 
  mutate(date = as.Date(date, "%d/%m/%Y")) |> 
  arrange(date) |> 
  group_by(team) |> 
  summarise(outcomes = list(result),.groups = "drop")


### TEAM FORTRESS 2: Who wins most at home and away?
# Percentage of home games won

df <- df |> 
  mutate(home_win_team = 
           case_when(fulltime_result == "H" ~ home_team),
         away_win_team = case_when(fulltime_result == "A" ~ away_team))

location_wins_counts <- df |> 
  pivot_longer(cols = c(home_win_team,away_win_team), names_to = "win_location", values_to = "team" ) |> 
  group_by(team, win_location ) |> 
  count(win_location) |> 
  pivot_wider(names_from = win_location, values_from = n)


### AGGREGATE STATS ----
# We sum up over our variables

spfl_totals <- spfl |>
  group_by(team)  |>
  summarise(across(where(is.numeric), sum, na.rm = TRUE))


spfl |> count(team)

clubs <- unique(df$home_team)


# GET THE LOGOS 

source("get_team_logos.R")

### STICKING THINGS TOGETHER
# Building a wide table

spfl_table <- spfl_totals |> 
  left_join(wld_points) |> 
  left_join(wld_count) |> 
  left_join(wld_sparks) |> 
  left_join(swing_counts) |> 
  left_join(location_wins_counts) |> 
  left_join(logos)


### CALCULATE PROPORTIONS

# Proportion of points won from draw and wins

spfl_table <- spfl_table |> 
  mutate(prop_point_win = n_win / (n_win + n_draw + n_loss),
         prop_point_draw = n_draw / (n_win + n_draw + n_loss),
         prop_point_loss = n_loss / (n_win + n_draw + n_loss)) 







