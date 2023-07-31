## code to prepare `data_f_id` dataset goes here
set.seed(347)  # For reproducibility
data_f_id <- data.frame(
  x = c(stats::rnorm(5, mean = 1, sd = 1),
        stats::rnorm(5, mean = 1, sd = 1),
        stats::rnorm(5, mean = 1, sd = 1)),
  grp = factor(rep(c("Group1", "Group2", "Group3"), each = 5))
)

usethis::use_data(data_f_id, overwrite = TRUE)
