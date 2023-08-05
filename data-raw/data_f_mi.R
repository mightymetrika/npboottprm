## code to prepare `data_f_mi` dataset goes here
set.seed(397)  # For reproducibility
data_f_mi <- data.frame(
  x = c(stats::rnorm(5, mean = 0, sd = 1),
        stats::rnorm(5, mean = 1, sd = 1),
        stats::rnorm(5, mean = 2, sd = 1)),
  grp = factor(rep(c("Group1", "Group2", "Group3"), each = 5))
)

# Add 1 missing value for an x value in each group
miss_idx <- sapply(levels(data_f_mi$grp), function(i) {
  idx <- which(data_f_mi$grp == i)
  sample(idx, 1)
})

data_f_mi$x[miss_idx] <- NA

rm(miss_idx)

usethis::use_data(data_f_mi, overwrite = TRUE)
