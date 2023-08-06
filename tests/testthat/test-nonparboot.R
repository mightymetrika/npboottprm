test_that("Test the nonparametric bootstrap t-test", {
  np_res <- nonparboot(iris[iris$Species != "virginica",],
                     x = "Sepal.Length",
                     grp = "Species",
                     nboot = 1000,
                     test = "t",
                     conf.level = 0.95)

  expect_equal(length(np_res), 7)
})

test_that("Test the nonparametric bootstrap t-test", {
  np_res <- nonparboot(iris,
                     x = "Sepal.Length",
                     y = "Petal.Length",
                     nboot = 1000,
                     test = "pt",
                     conf.level = 0.95)

  expect_equal(length(np_res), 7)
})

test_that("Test the nonparametric bootstrap F-test", {
  np_res <- nonparboot(iris,
                      x = "Sepal.Length",
                      grp = "Species",
                      nboot = 1000,
                      test = "F",
                      conf.level = 0.95)

  expect_equal(length(np_res), 7)
})

test_that("nonparboot function works correctly", {
  res_t <- nonparboot(data_t, x = "x", grp = "grp", nboot = 1000, test = "t", seed = 123)
  expect_type(res_t, "list")
  expect_type(res_t$p.value, "double")
  expect_type(res_t$orig.stat, "double")
  expect_type(res_t$ci.stat, "double")
  expect_type(res_t$bootstrap.stat.dist, "double")
  expect_type(res_t$effect.size, "double")
  expect_type(res_t$ci.effect.size, "double")
  expect_type(res_t$ci.effect.size, "double")
  expect_type(res_t$bootstrap.effect.dist, "double")

  res_pt <- nonparboot(data_pt, x = "x", y = "y", nboot = 1000, test = "pt", seed = 123)
  expect_type(res_pt, "list")
  expect_type(res_pt$p.value, "double")
  expect_type(res_pt$orig.stat, "double")
  expect_type(res_pt$ci.stat, "double")
  expect_type(res_pt$bootstrap.stat.dist, "double")
  expect_type(res_pt$effect.size, "double")
  expect_type(res_pt$ci.effect.size, "double")
  expect_type(res_pt$ci.effect.size, "double")
  expect_type(res_pt$bootstrap.effect.dist, "double")

  res_f <- nonparboot(data_f, x = "x", grp = "grp", nboot = 1000, test = "F", seed = 123)
  expect_type(res_f, "list")
  expect_type(res_f$p.value, "double")
  expect_type(res_f$orig.stat, "double")
  expect_type(res_f$ci.stat, "double")
  expect_type(res_f$bootstrap.stat.dist, "double")
  expect_type(res_f$effect.size, "double")
  expect_type(res_f$ci.effect.size, "double")
  expect_type(res_f$ci.effect.size, "double")
  expect_type(res_f$bootstrap.effect.dist, "double")

})


test_that("Test nonparboot 't' with missing values", {
  np_res <- nonparboot(data_t_mi,
                       x = "x",
                       grp = "grp",
                       nboot = 1000,
                       test = "t",
                       conf.level = 0.95,
                       na_rm = TRUE)

  expect_equal(length(np_res), 7)
})

test_that("Test nonparboot 'pt' with missing values", {
  np_res <- nonparboot(data_pt_mi,
                       x = "x",
                       y = "y",
                       nboot = 1000,
                       test = "pt",
                       conf.level = 0.95,
                       na_rm = TRUE)

  expect_equal(length(np_res), 7)
})

test_that("Test nonparboot 'F' with missing values", {
  np_res <- nonparboot(data_f_mi,
                       x = "x",
                       grp = "grp",
                       nboot = 1000,
                       test = "F",
                       conf.level = 0.95,
                       na_rm = TRUE)

  expect_equal(length(np_res), 7)
})

test_that("Test nonparboot 't' with missing group value", {

  data_t_mig <- data_t
  data_t_mig[3, 2] <- NA

  np_res <- nonparboot(data_t_mig,
                       x = "x",
                       grp = "grp",
                       nboot = 1000,
                       test = "t",
                       conf.level = 0.95,
                       na_rm = TRUE)

  expect_equal(length(np_res), 7)
})

test_that("Test nonparboot 'F' with missing group values", {
  data_f_mig <- data_f
  data_f_mig[3, 2] <- NA
  np_res <- nonparboot(data_f_mig,
                       x = "x",
                       grp = "grp",
                       nboot = 1000,
                       test = "F",
                       conf.level = 0.95,
                       na_rm = TRUE)

  expect_equal(length(np_res), 7)
})

test_that("Test nonparboot 't' with missing values fails when na_rm is FALSE", {
  expect_error(nonparboot(data_t_mi,
                       x = "x",
                       grp = "grp",
                       nboot = 1000,
                       test = "t",
                       conf.level = 0.95,
                       na_rm = FALSE))
})

test_that("Test nonparboot 'F' fails when x is not a character", {
  expect_error(nonparboot(data_f,
                       x = 3,
                       grp = "grp",
                       nboot = 1000,
                       test = "F",
                       conf.level = 0.95))
})
