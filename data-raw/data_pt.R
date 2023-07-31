## code to prepare `data_pt` dataset goes here
set.seed(498)  # For reproducibility
data_pt <- data.frame(
  x = stats::rnorm(10, mean = 0, sd = 1),
  y = stats::rnorm(10, mean = 1, sd = 1)
)

usethis::use_data(data_pt, overwrite = TRUE)
