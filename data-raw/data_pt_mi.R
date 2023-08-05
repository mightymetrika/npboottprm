## code to prepare `data_pt_mi` dataset goes here
set.seed(498)  # For reproducibility
data_pt_mi <- data.frame(
  x = stats::rnorm(10, mean = 0, sd = 1),
  y = stats::rnorm(10, mean = 1, sd = 1)
)

# Add one missing value to each variable
miss_idx = vector(mode = "integer", length = 2)
for (i in 1:2) {
  idx = sample(1:10, 1)
  miss_idx[i] = idx
  data_pt_mi[[names(data_pt_mi)[i]]][miss_idx[i]] = NA
}

rm(miss_idx)
usethis::use_data(data_pt_mi, overwrite = TRUE)
