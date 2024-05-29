library(tidyverse)


# Read data downloaded from https://www.football-data.co.uk/mmz4281/2324/SC0.csv
df <- read.csv('data/SC0.csv')


glimpse(df)

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


 # Build a slightly longer data set, where we have a new var tracking the location
 # I'm sure there's a way in pivot_longer to do this.
 
away_df <- select(df, div,date, time, contains("away")) |> 
  rename_with(~str_remove(.,'away_')) |> 
  mutate(location = "away")

home_df <- select(df, div,date, time, contains("home")) |> 
  rename_with(~str_remove(.,'home_')) |> 
  mutate(location = "home")

# The final dataset to work with
spfl <- rbind(away_df, home_df) |> 
  select(div,date,time, team,location, everything()) |> 
  arrange(team)


# Win loss draw counts
wld <- df |> 
  select(home_team, away_team, fulltime_result) |> 
  pivot_longer( cols = away_team:home_team,            # Select all columns to pivot
                names_to = c("location", "var"), # New variable names for parts of the original names
                names_sep = "_",
                values_to = "team") |> 
  mutate(points = case_when(location == "away" & fulltime_result == "A" ~ 3,
         location == "home" & fulltime_result == "H" ~ 3,
fulltime_result == "D" ~ 1,
TRUE ~ 0 ) ) |> 
  pivot_wider(names_from = location, values_from = points)

### AGGREGATE STATS
# We sum up over our variables

spfl_totals <- spfl |> 
  group_by(team)  |> 
  summarise(across(where(is.numeric), sum, na.rm = TRUE))


spfl |> count(team)

clubs <- unique(df$HomeTeam)




