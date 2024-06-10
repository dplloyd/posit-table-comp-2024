# Script pulls team logos from wikipedia

library(gt)

team <- read.csv('data/SC0.csv') |> 
  pull(HomeTeam) |> 
  unique() 

logo_url <- "https://upload.wikimedia.org/wikipedia/en/thumb/3/37/Hibernian_FC_logo.svg/1280px-Hibernian_FC_logo.svg.png"


logos <- tibble(team = team, url =
                  case_when(team == "Celtic" ~ "https://upload.wikimedia.org/wikipedia/en/7/71/Celtic_FC_crest.svg",
                            team == "Dundee" ~ "https://upload.wikimedia.org/wikipedia/commons/c/c2/Dundee_FC_logo.png",
                            team == "Livingston" ~ "https://upload.wikimedia.org/wikipedia/en/f/f8/Livingston_FC_club_badge_new.png",
                            team == "St Johnstone" ~ "https://upload.wikimedia.org/wikipedia/en/5/52/StJohnstoneFC_Logo.svg",
                            team == "Kilmarnock" ~ "https://upload.wikimedia.org/wikipedia/en/3/32/KilmarnockLogo.svg",
                            team == "Hibernian" ~ "https://upload.wikimedia.org/wikipedia/en/thumb/3/37/Hibernian_FC_logo.svg/1280px-Hibernian_FC_logo.svg.png",
                            team == "Rangers" ~ "https://upload.wikimedia.org/wikipedia/en/4/43/Rangers_FC.svg",
                            team == "Ross County" ~ "https://upload.wikimedia.org/wikipedia/en/5/57/Ross_County_F.C._logo.png",
                            team == "St Mirren" ~ "https://upload.wikimedia.org/wikipedia/en/a/a7/St_mirren_fc_logo.png",
                            team == "Aberdeen" ~ "https://upload.wikimedia.org/wikipedia/en/d/d4/Aberdeen_FC_logo.svg",
                            team == "Hearts" ~ "https://upload.wikimedia.org/wikipedia/en/6/61/Heart_of_Midlothian_FC_logo.svg",
                            team == "Motherwell" ~ "https://upload.wikimedia.org/wikipedia/commons/c/c8/Motherwell_FC_crest.svg"
                            ) )

gt::gt(logos) |> 
  text_transform(
    locations = cells_body(columns = url),
    fn = function(x) {
      web_image(
        url = logos$url
      )
    }
  ) |> 
  opt_interactive(
    
    active = TRUE,
    use_pagination = TRUE,
    use_pagination_info = TRUE,
    use_sorting = TRUE,
    use_search = FALSE,
    use_filters = FALSE,
    use_resizers = FALSE,
    use_highlight = FALSE,
    use_compact_mode = FALSE,
    use_text_wrapping = TRUE,
    use_page_size_select = FALSE,
    page_size_default = 10,
    page_size_values = c(10, 25, 50, 100),
    pagination_type = c("numbers", "jump", "simple")
  )
