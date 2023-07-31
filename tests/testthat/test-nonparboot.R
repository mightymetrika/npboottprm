test_that("Test the nonparametric bootstrap t-test", {
  np_res <- nonparboot(iris[iris$Species != "virginica",],
                     x = "Sepal.Length",
                     grp = "Species",
                     nboot = 1000,
                     test = "t",
                     conf.level = 0.95)

  expect_equal(length(np_res), 4)
})

test_that("Test the nonparametric bootstrap t-test", {
  np_res <- nonparboot(iris,
                     x = "Sepal.Length",
                     y = "Petal.Length",
                     nboot = 1000,
                     test = "pt",
                     conf.level = 0.95)

  expect_equal(length(np_res), 4)
})

test_that("Test the nonparametric bootstrap F-test", {
  np_res <- nonparboot(iris,
                      x = "Sepal.Length",
                      grp = "Species",
                      nboot = 1000,
                      test = "F",
                      conf.level = 0.95)

  expect_equal(length(np_res), 4)
})

test_that("nonparboot function works correctly", {
  res_t <- nonparboot(data_t, x = "x", grp = "grp", nboot = 1000, test = "t", seed = 123)
  expect_type(res_t, "list")
  expect_type(res_t$p.value, "double")
  expect_type(res_t$effect.size, "double")
  expect_type(res_t$ci.effect.size, "double")
  expect_type(res_t$bootstrap.dist, "double")

  res_pt <- nonparboot(data_pt, x = "x", y = "y", nboot = 1000, test = "pt", seed = 123)
  expect_type(res_pt, "list")
  expect_type(res_pt$p.value, "double")
  expect_type(res_pt$effect.size, "double")
  expect_type(res_pt$ci.effect.size, "double")
  expect_type(res_pt$bootstrap.dist, "double")

  res_f <- nonparboot(data_f, x = "x", grp = "grp", nboot = 1000, test = "F", seed = 123)
  expect_type(res_f, "list")
  expect_type(res_f$p.value, "double")
  expect_type(res_f$effect.size, "double")
  expect_type(res_f$ci.effect.size, "double")
  expect_type(res_f$bootstrap.dist, "double")
})
