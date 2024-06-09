# Building table

library(gt)
library(gtExtras)

source("01_spfl_collect_data.R")

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

# Ideas
# win/loss/draw sparkline
# Distribution of points from WLD



final_table <- spfl_table_gt |> 
  gt() |> 
  text_transform(
    locations = cells_body(columns = url),
    fn = function(x) {
      web_image(
        url = spfl_table_gt$url
      )
    }
  ) |> 
  
  gtExtras::gt_plt_winloss(
    outcomes,
    type = "pill",
    width = 38,
    max_wins = 38,
    palette = c("#28A197", "#F46A25", "#3D3D3D")
  ) |> 
  
  gtExtras::gt_plt_bullet(
    column = n_win,
    target = n_win_median,
    width = 30,
    palette =  c("#28A197", "black")
  ) |> 
  
  gtExtras::gt_plt_bar_stack(
    column = fouls_yellow_red,
    width = 50,
    palette =  c("#d5ab09", "#8F2831"),
    labels = c("Yellow","Red"),
    position = "stack"  ) |> 
  
  gtExtras::gt_plt_bar_stack(
    column = half_time_full_time_goals_list,
    width = 50,
    palette =  c("#28A197", "black"),
    labels = c("First half","Second half"),
    position = "stack"
  ) |> 
  

  tab_header(
    title = gtExtras::add_text_img(
      "2023–24 Scottish Premiership",
      url = "https://upload.wikimedia.org/wikipedia/en/a/a1/Cinch_premiership.png",
      height = 30
    ),
    subtitle = "A season in numbers"
  ) |> 
  
  cols_label(
    position = "Position",
    url = "",
    team = "Club",
    total_points = "Points",
    game_lost_FT_from_non_losing_HT = "FT losses from non-losing HT",
    game_won_FT_from_non_win_HT = "FT wins from losing HT",
    outcomes = "Match outcomes",
    n_win = "Matches won relative to previous seasons"
  ) |> 
  
  
  tab_spanner(label = "Goals scored in the...", columns = half_time_full_time_goals_list) |> 
  
  tab_spanner(label = "Penalty cards received which were...", columns = fouls_yellow_red) |> 
  
  tab_spanner(label = "Swing between HT and FT results", columns = game_lost_FT_from_non_losing_HT:game_won_FT_from_non_win_HT) |> 
  
  
  
  opt_table_font(stack = "geometric-humanist") |>
  
  opt_align_table_header(align = "left") |> 

  tab_source_note(source_note = html("<b>NOTES</b><br>Data are from https://www.football-data.co.uk.<br> The Scottish Premiership splits into equal-sized top and bottom groups after 33 matches. The remaining five matches played by each team are all within their respective group. As such, a team can finish with more points than another team with a better overall table placement.") ) |> 
  
  tab_footnote(
    footnote = "The vertical line is the median matches won per season between 2000-01 and 2023-24, for each season the team was in the Premiership.",
    locations = cells_column_labels(columns = n_win)
  )
  
  
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

