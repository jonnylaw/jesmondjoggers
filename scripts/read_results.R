read_results = function(filename) {
  # Determine if summer or winter
  summer = stringr::str_detect(string = filename, pattern = "summer")
  
  # Parse date
  date = lubridate::ymd(paste0(stringr::str_extract(string = filename, pattern = "[0-9]+"), "01"))
  
  read_csv(file = filename, col_types = "icccccc") %>% 
    add_column(season = paste(ifelse(summer, yes = "Summer", no = "Winter"), lubridate::year(date)))
}

results_files = file.path("results", list.files(path = "results", pattern = "*.csv"))

all_results = results_files %>% 
  map_df(read_results) %>%
  mutate(running_time = period_to_seconds(ms(as.character(running_time))),
         gross_time = period_to_seconds(ms(as.character(gross_time)))) %>%
  filter(handicap != "5") %>%
  mutate(handicap_time = gross_time - running_time)

# Final results of the winter handicap 2018
final_handicap = read_results("results/grand_prix_201804_winter.csv") %>%
  mutate(running_time = period_to_seconds(ms(as.character(running_time))))

# The last race is run from scratch so we must read the latest handicaps and calculate the "gross time"
average_results = all_results %>%
  inner_join(final_handicap %>% select(2:3)) %>%
  mutate(time = running_time) %>%
  group_by(first_name, second_name) %>%
  summarise(time = mean(time))

reigel_formula = function(time1, distance1 = 3.1, distance2 = 2.145) {
  time1 * (distance2/distance1) * 1.06
}

# Use 5k time to predict running time using the Reigel Formula
calculate_handicaps = function(slowest_runner = as.numeric(lubridate::ms("30:00")), race_distance = 2.145) {
  times = seq(from = as.numeric(lubridate::ms("16:00")), to = slowest_runner, by = 30)
  pretty_times = lubridate::seconds_to_period(times)
  
  data_frame(time_5k = pretty_times) %>%
    mutate(expected_finish_time = reigel_formula(time1 = as.numeric(time_5k)), distance1 = 3.1, distance2 = race_distance) %>%
    mutate(handicap_time = (max(expected_finish_time) - expected_finish_time)) %>%
    mutate(handicap_time_rounded = round(handicap_time, -1)) %>%
    select(time_5k, handicap_time_rounded, expected_finish_time) %>%
    mutate(handicap_time = lubridate::seconds_to_period(handicap_time_rounded),
           expected_finish_time = round(lubridate::seconds_to_period(expected_finish_time), 0)) %>%
    select(time_5k, expected_finish_time, handicap_time)
}

# Get current handicaps from 5k PB
current_handicaps = calculate_handicaps() %>%
  mutate(handicap = period_to_seconds(handicap_time)) %>% 
  .$handicap

# Round the raw handicap (longest_run - your_run_time) to one of the current handicaps
round_to_handicap = function(new_handicap_time, current_handicaps) {
  i = which.min(abs(new_handicap_time - current_handicaps))
  current_handicaps[i]
}

# Calculate the longest run of all runs
longest_run = max(average_results$time)

# Calculate new handicaps, only for the competitors in the final race
new_handicaps = average_results %>%
  mutate(new_handicap_raw = longest_run - time) %>%
  rowwise() %>%
  mutate(new_handicap = round_to_handicap(new_handicap_raw, current_handicaps)) %>%
  select(first_name, second_name, new_handicap) %>%
  arrange(new_handicap)

# Calculate estimated race time from 5k PB
calculated_handicaps = tibble(
  first_name = c("Chris", "Emma", "Ben", "Kate"),
  second_name = c("Lawrence", "Glover", "Bedlington", "Black"),
  time_5k = c(as.numeric(lubridate::ms("20:00")), 
              as.numeric(lubridate::ms("21:00")), 
              as.numeric(lubridate::ms("20:30")),
              as.numeric(lubridate::ms("21:30"))) 
) %>%
  mutate(race_time = reigel_formula(time_5k)) %>%
  mutate(handicap_time_raw = longest_run - race_time) %>%
  rowwise() %>%
  mutate(new_handicap = round_to_handicap(handicap_time_raw, current_handicaps)) %>%
  select(first_name, second_name, new_handicap)
  
final_handicap = final_handicap %>%
  select(-gross_time, -position) %>%
  left_join(new_handicaps %>% bind_rows(calculated_handicaps)) %>%
  rename(handicap_time = new_handicap) %>%
  mutate(gross_time = running_time + handicap_time) %>%
  mutate(position = row_number(gross_time)) %>%
  arrange(position)

all_results = all_results %>%
  bind_rows(final_handicap)
