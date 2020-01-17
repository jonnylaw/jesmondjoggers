
#' Title
#'
#' @param time1 
#' @param distance1 
#' @param distance2 
#'
#' @return
#' @export
#'
#' @examples
reigel_formula <- function(time1, distance1 = 3.1, distance2 = 2.145) {
  time1 * (distance2 / distance1) * 1.06
}

#' Use 5k time to predict running time using the Reigel Formula
#'
#' @param slowest_runner the time of the slowest runner
#' @param race_distance the distance of the predicted race
#'
#' @return
#' @export
#'
#' @examples
calculate_handicaps <- function(slowest_runner = as.numeric(lubridate::ms("30:00")), race_distance = 2.145) {
  times <- seq(from = as.numeric(lubridate::ms("16:00")), to = slowest_runner, by = 30)
  pretty_times <- lubridate::seconds_to_period(times)
  
  tibble::tibble(time_5k = pretty_times) %>%
    dplyr::mutate(expected_finish_time = reigel_formula(time1 = as.numeric(time_5k)), distance1 = 3.1, distance2 = race_distance) %>%
    dplyr::mutate(handicap_time = (max(expected_finish_time) - expected_finish_time)) %>%
    dplyr::mutate(handicap_time_rounded = round(handicap_time, -1)) %>%
    dplyr::select(time_5k, handicap_time_rounded, expected_finish_time) %>%
    dplyr::mutate(
      handicap_time = lubridate::seconds_to_period(handicap_time_rounded),
      expected_finish_time = round(lubridate::seconds_to_period(expected_finish_time), 0)
    ) %>%
    dplyr::select(time_5k, expected_finish_time, handicap_time)
}

#' Title
#'
#' Round the raw handicap (longest_run - your_run_time) 
#' to one of the current handicaps
#'
#' @param new_handicap_time 
#' @param current_handicaps 
#'
#' @return
#' @export
#'
#' @examples
round_to_handicap <- function(new_handicap_time, current_handicaps) {
  i <- which.min(abs(new_handicap_time - current_handicaps))
  current_handicaps[i]
}

#' Calculate new handicaps
#'
#' @param all_results 
#' @param summary_fn a function to use to summarise an athletes times, 
#' for example mean, min or median
#'
#' @return
#' @export
#'
#' @examples
get_new_handicaps <- function(all_results, summary_fn = median) {
  summarised_results <- all_results %>%
    dplyr::group_by(first_name, second_name) %>%
    dplyr::summarise(time = summary_fn(running_time))
  
  longest_run <- max(summarised_results$time)
  
  summarised_results %>%
    dplyr::mutate(new_handicap_raw = longest_run - time) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(new_handicap = round_to_handicap(new_handicap_raw, current_handicaps)) %>%
    dplyr::select(first_name, second_name, new_handicap) %>%
    dplyr::arrange(new_handicap)
}