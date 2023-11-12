test_that("t2_replext works", {

  # Test t2_replext_c1.1
  res <- t2_replext_c1.1(n1 = c(4,5), n2 = c(4,5), n_simulations = 5)
  expect_equal(nrow(res), 2)
  expect_equal(ncol(res), 7)
  expect_s3_class(res, "data.frame")
  rm(res)

  # Test t2_replext_c3.2
  res <- t2_replext_c3.2(n1 = c(4,5), n2 = c(4,5), n_simulations = 5)
  expect_equal(nrow(res), 2)
  expect_equal(ncol(res), 7)
  expect_s3_class(res, "data.frame")
})
