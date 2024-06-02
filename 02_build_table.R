# Build table

library(gt)
library(gtExtras)

source("01_spfl_collect_data.R")

spfl_table_gt <- 
  spfl_table |> 
select(team, url, outcomes)

# Ideas
# win/loss/draw sparkline
# Distribution of points from WLD



spfl_table_gt |> 
  gt() |> 
  text_transform(
    locations = cells_body(columns = url),
    fn = function(x) {
      web_image(
        url = spfl_table_gt$url
      )
    }
  ) |> 
  
  gtExtras::gt_plt_winloss(outcomes, type = "pill", width = 38, max_wins = 38) |> 
  
  tab_header(
    title = gtExtras::add_text_img(
      "2023–24 Scottish Premiership",
      url = "https://upload.wikimedia.org/wikipedia/en/a/a1/Cinch_premiership.png",
      height = 30
    )
  ) |> 
  
  opt_interactive(
    
    active = TRUE,
    use_pagination = FALSE,
    use_pagination_info = FALSE,
    use_sorting = TRUE,
    use_search = FALSE,
    use_filters = FALSE,
    use_resizers = FALSE,
    use_highlight = TRUE,
    use_compact_mode = FALSE,
    use_text_wrapping = TRUE,
    use_page_size_select = FALSE,
    page_size_default = 12,
    page_size_values = c(10, 25, 50, 100),
    pagination_type = c("numbers", "jump", "simple")
  )

