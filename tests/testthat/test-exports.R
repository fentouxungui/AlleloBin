library(testthat)
test_that("exports exist", {
  expect_true("allele_binning" %in% ls("package:AlleloBin"))
  expect_true("save_results" %in% ls("package:AlleloBin"))
  expect_true("AlleloBin" %in% ls("package:AlleloBin"))
})
