read_results = function(filename) {
  read_csv(file = filename, col_types = "icccccc")
}

results_files = file.path("results", list.files(path = "results", pattern = "*.csv"))

all_results = results_files %>% 
  map_df(read_results) %>%
  mutate(running_time = period_to_seconds(ms(as.character(running_time))),
         gross_time = period_to_seconds(ms(as.character(gross_time)))) %>%
  mutate(handicap_time = gross_time - running_time)