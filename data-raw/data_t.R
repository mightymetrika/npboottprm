## code to prepare `data_t` dataset goes here
set.seed(493)  # For reproducibility
data_t <- data.frame(
  x = c(stats::rnorm(5, mean = 0, sd = 1), stats::rnorm(5, mean = 1, sd = 1)),
  grp = factor(rep(c("Group1", "Group2"), each = 5))
)

usethis::use_data(data_t, overwrite = TRUE)
