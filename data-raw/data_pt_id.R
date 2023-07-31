## code to prepare `data_pt_id` dataset goes here
set.seed(478)  # For reproducibility
data_pt_id <- data.frame(
  x = stats::rnorm(10, mean = 0, sd = 1),
  y = stats::rnorm(10, mean = 0, sd = 1)
)

usethis::use_data(data_pt_id, overwrite = TRUE)
