read_results <- function(filename) {
  year_handicap <- stringr::str_extract(string = filename, pattern = "[0-9]+")
  read_csv(file = filename, col_types = "iccccccc") %>%
    mutate(handicap = year_handicap)
}

results_files <- fs::dir_ls(path = "results", pattern = "*.csv")

all_results <- results_files %>%
  map_dfr(read_results) %>%
  mutate(
    running_time = period_to_seconds(lubridate::ms(as.character(running_time))),
    gross_time = period_to_seconds(lubridate::ms(as.character(gross_time)))
  ) %>%
  mutate(handicap_time = gross_time - running_time)