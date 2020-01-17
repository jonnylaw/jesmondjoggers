library(jjshandicap)

all_results <- read_all_results()
readr::write_rds(all_results, "data/all_results.Rds")