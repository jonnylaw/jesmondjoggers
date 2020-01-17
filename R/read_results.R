#' Title
#'
#' @param filename 
#'
#' @return
#' @export
#'
#' @examples
read_results <- function(filename) {
  year_handicap <- stringr::str_extract(string = filename, pattern = "[0-9]+")
  readr::read_csv(file = filename, col_types = "iccccccc") %>%
    dplyr::mutate(handicap = year_handicap)
}

#' Read all results
#'
#' @param directory 
#'
#' @return
#' @export
#'
#' @examples
read_all_results <- function(directory = "results") {
  results_files <- fs::dir_ls(path = directory, pattern = "*.csv")
  
  results_files %>%
    purrr::map_dfr(read_results) %>%
    dplyr::mutate(
      running_time = as.numeric(lubridate::ms(as.character(running_time))),
      gross_time = as.numeric(lubridate::ms(as.character(gross_time)))
    ) %>%
    dplyr::mutate(handicap_time = gross_time - running_time)  
}

