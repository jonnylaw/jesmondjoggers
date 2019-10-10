# The last race is run from scratch so we must read the latest handicaps and calculate the "gross time"
average_results <- all_results %>%
  group_by(first_name, second_name) %>%
  summarise(time = mean(running_time))

reigel_formula <- function(time1, distance1 = 3.1, distance2 = 2.145) {
  time1 * (distance2 / distance1) * 1.06
}

# Use 5k time to predict running time using the Reigel Formula
calculate_handicaps <- function(slowest_runner = as.numeric(lubridate::ms("30:00")), race_distance = 2.145) {
  times <- seq(from = as.numeric(lubridate::ms("16:00")), to = slowest_runner, by = 30)
  pretty_times <- lubridate::seconds_to_period(times)
  
  data_frame(time_5k = pretty_times) %>%
    mutate(expected_finish_time = reigel_formula(time1 = as.numeric(time_5k)), distance1 = 3.1, distance2 = race_distance) %>%
    mutate(handicap_time = (max(expected_finish_time) - expected_finish_time)) %>%
    mutate(handicap_time_rounded = round(handicap_time, -1)) %>%
    select(time_5k, handicap_time_rounded, expected_finish_time) %>%
    mutate(
      handicap_time = lubridate::seconds_to_period(handicap_time_rounded),
      expected_finish_time = round(lubridate::seconds_to_period(expected_finish_time), 0)
    ) %>%
    select(time_5k, expected_finish_time, handicap_time)
}

# Get current handicaps from 5k PB
current_handicaps <- calculate_handicaps() %>%
  mutate(handicap = period_to_seconds(handicap_time)) %>%
  .$handicap

# Round the raw handicap (longest_run - your_run_time) to one of the current handicaps
round_to_handicap <- function(new_handicap_time, current_handicaps) {
  i <- which.min(abs(new_handicap_time - current_handicaps))
  current_handicaps[i]
}

# Calculate the longest run of all runs
longest_run <- max(average_results$time)

# Calculate new handicaps, only for the competitors in the final race
new_handicaps <- average_results %>%
  mutate(new_handicap_raw = longest_run - time) %>%
  rowwise() %>%
  mutate(new_handicap = round_to_handicap(new_handicap_raw, current_handicaps)) %>%
  select(first_name, second_name, new_handicap) %>%
  arrange(new_handicap)