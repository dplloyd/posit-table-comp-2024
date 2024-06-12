# Building table

library(gt)
library(gtExtras)
library(tidyverse)

# Select the variables of interest
spfl_table_gt <-
  spfl_table |>
  select(position,
    url,
    team,
    half_time_full_time_goals_list,
    fouls_yellow_red,
    game_lost_FT_from_non_losing_HT,
    game_won_FT_from_non_win_HT,
    outcomes,
    n_win,
    n_win_median,
    total_points
  ) |> 
  arrange(position)


# Table subtitle, which we use later on
subtitle <- html("This table shows the final league position of football clubs competing in the 2023-24 Scottish Premiership, with selected season statistics, such as goals scored, penalty cards received and  matches won.<br><br>The division was formed in 2013 and constitutes 12 teams, each of which play 38 matches. Rangers and Celtic generally dominate the division owing to superior resources.")


# Building the table.
final_table <- spfl_table_gt |> 
  gt() |> 
  
  # Insert club logos
  text_transform(
    locations = cells_body(columns = url),
    fn = function(x) {
      web_image(
        url = spfl_table_gt$url
      )
    }
  ) |> 
  
  # Insert outcomes pill plot
  gtExtras::gt_plt_winloss(
    outcomes,
    type = "pill",
    width = 38,
    max_wins = 38,
    palette = c("#28A197", "#F46A25", "#3D3D3D")
  ) |> 
  
  # Insert plot showing matches won, relative to median since 2000-01 season as a bullet plot. A footnote # added later to explain how to read this.
  gtExtras::gt_plt_bullet(
    column = n_win,
    target = n_win_median,
    width = 30,
    palette =  c("#28A197", "black")
  ) |> 
  
  # Insert stacked barplot with aggregate yellow and red cards
  gtExtras::gt_plt_bar_stack(
    column = fouls_yellow_red,
    width = 50,
    palette =  c("#d5ab09", "#8F2831"),
    labels = c("Yellow","Red"),
    position = "stack"  ) |> 
  
  # Insert staked barplot showing what half goals scored in
  gtExtras::gt_plt_bar_stack(
    column = half_time_full_time_goals_list,
    width = 50,
    palette =  c("#28A197", "black"),
    labels = c("First half","Second half"),
    position = "stack"
  ) |> 
  
# Insert table title, subtitle (defined earlier) and official logo
  tab_header(
    title = gtExtras::add_text_img(html(
      "<b>2023–24 Scottish Premiership</b>"),
      url = "https://upload.wikimedia.org/wikipedia/en/a/a1/Cinch_premiership.png",
      height = 30
    ),
    subtitle = subtitle
  ) |> 
  
  # Rename column titles
  cols_label(
    position = "Position",
    url = "",
    team = "Club",
    total_points = "Points",
    game_lost_FT_from_non_losing_HT = "non-losing HT → FT loss",
    game_won_FT_from_non_win_HT = "non-win HT → FT win",
    outcomes = "Match outcomes",
    n_win = "Matches won relative to previous seasons"
  ) |> 
  
  # Add a series of column spanners which help explain what the sub-columns are showing
  tab_spanner(label = "Goals scored in the...", columns = half_time_full_time_goals_list) |> 
  
  tab_spanner(label = "Penalty cards received which were...", columns = fouls_yellow_red) |> 
  
  tab_spanner(label = "Swing: Changes between half-time and full-time results", columns = game_lost_FT_from_non_losing_HT:game_won_FT_from_non_win_HT) |> 
  
  
  # Choose the font
  opt_table_font(stack = "geometric-humanist") |>
  
  # Set alignment
  opt_align_table_header(align = "left") |> 

  # Insert note at the bottom of the table
  tab_source_note(source_note = html("<b>Data:</b> https://www.football-data.co.uk | <b>Table:</b> @dplloyd<br><br><b>Notes</b><br> The Scottish Premiership splits into equal-sized top and bottom groups after 33 matches. The remaining five matches played by each team are all within their respective group. As such, a team can finish with more points than another team with a better overall table placement.<br>") ) |>
  
  # Insert footnote providing some more information on what the buller plot shows
  tab_footnote(
    footnote = "Vertical lines are median matches won per season between 2000-01 and 2023-24, for each season the relevant team was in the Premiership. While the Premiership formed its current iteration in 2013 following a merger between the Scottish Premier League and the Scottish Football League, twelve teams have competed in the top-flight since 2000-01.",
    locations = cells_column_labels(columns = n_win)
  )
  
  # Optional for making the table interactive.
  # opt_interactive(
  # 
  #   active = TRUE,
  #   use_pagination = FALSE,
  #   use_pagination_info = FALSE,
  #   use_sorting = TRUE,
  #   use_search = FALSE,
  #   use_filters = FALSE,
  #   use_resizers = FALSE,
  #   use_highlight = TRUE,
  #   use_compact_mode = FALSE,
  #   use_text_wrapping = TRUE,
  #   use_page_size_select = FALSE,
  #   page_size_default = 12,
  #   page_size_values = c(10, 25, 50, 100),
  #   pagination_type = c("numbers", "jump", "simple")
  # )

