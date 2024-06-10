# Script downloads and saves data back to 2000-01 season.

data_sources <- tibble(year_short = c("23-24","22-23","21-22","20-21","19-20","18-19","17-18","16-17","15-16","14-15","13-14","12-13","11-12","10-11","09-10","08-09","07-08","06-07","05-06","04-05","03-04","02-03","01-02","00-01"),
                       year_url = str_remove(year_short,"-"),
              data_url = glue::glue("https://www.football-data.co.uk/mmz4281/{year_url}/SC0.csv"),
              source = as.character( seq(1,24)) )

ts_df <- map_dfr(data_sources$data_url, ~readr::read_csv(.x) |> select(Div:AR), .id = "source") |> 
  left_join(data_sources)

write.csv(ts_df,"data/time_series.csv")
