# Script downloads and saves data back to 2000-01 season.

data_sources <- tibble(year_short = c("2324","2223","2122","2021","1920","1819","1718","1617","1516","1415","1314","1213","1112","1011","0910","0809","0708","0607","0506","0405","0304","0203","0102","0001"),
              data_url = glue::glue("https://www.football-data.co.uk/mmz4281/{year_short}/SC0.csv"),
              source = as.character( seq(1,24)) )

ts_df <- map_dfr(data_sources$data_url, ~readr::read_csv(.x) |> select(Div:AR), .id = "source") |> 
  left_join(data_sources)

write.csv(ts_df,"data/time_series.csv")