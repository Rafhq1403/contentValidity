test_that("scvi_ave returns the mean of the I-CVIs", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,    # I-CVI = 1.0
      3, 4, 4, 4, 3,    # I-CVI = 1.0
      2, 3, 3, 4, 3,    # I-CVI = 0.8
      1, 2, 3, 2, 3),   # I-CVI = 0.4
    nrow = 5
  )
  expect_equal(scvi_ave(ratings), 0.8)
})

test_that("scvi_ave works on a data frame", {
  df <- data.frame(
    item1 = c(4, 4, 4),
    item2 = c(3, 3, 3),
    item3 = c(2, 2, 2)
  )
  # I-CVIs: 1.0, 1.0, 0.0  ->  mean = 2/3
  expect_equal(scvi_ave(df), 2 / 3)
})

test_that("scvi_ave respects relevant_threshold", {
  ratings <- matrix(c(3, 3, 3, 4, 4, 4), nrow = 3)
  # threshold = 3: both items have I-CVI = 1.0 -> mean = 1.0
  expect_equal(scvi_ave(ratings), 1)
  # threshold = 4: item1 I-CVI = 0, item2 I-CVI = 1 -> mean = 0.5
  expect_equal(scvi_ave(ratings, relevant_threshold = 4), 0.5)
})

test_that("scvi_ave rejects a single-item vector", {
  expect_error(scvi_ave(c(4, 4, 3, 3)), "single item")
})

test_that("scvi_ave handles NA according to na.rm", {
  ratings <- matrix(
    c(4, NA, 3, 4,
      3,  4, 4, 4),
    nrow = 4
  )
  # With na.rm = FALSE (default), item1 I-CVI is NA, so mean is NA
  expect_true(is.na(scvi_ave(ratings)))
  # With na.rm = TRUE, item1 I-CVI = 3/3 = 1.0, item2 = 1.0, mean = 1.0
  expect_equal(scvi_ave(ratings, na.rm = TRUE), 1)
})


# ---------------------------------------------------------------------
# v0.2.0: bootstrap confidence intervals
# ---------------------------------------------------------------------

test_that("scvi_ave(ci = FALSE) preserves v0.1.0 scalar return", {
  ratings <- matrix(c(4, 4, 3, 4, 4, 1, 2, 3, 2, 3), nrow = 5)
  expect_type(scvi_ave(ratings), "double")
  expect_length(scvi_ave(ratings), 1)
})

test_that("scvi_ave(ci = TRUE) returns a one-row data frame", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      3, 4, 4, 4, 3,
      2, 3, 3, 4, 3,
      1, 2, 3, 2, 3),
    nrow = 5
  )
  result <- scvi_ave(ratings, ci = TRUE, n_boot = 200, seed = 1)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_named(result,
               c("item", "scvi_ave", "ci_lower", "ci_upper",
                 "ci_method", "conf_level", "n_boot"))
  expect_equal(result$item, "scale")
})

test_that("scvi_ave bootstrap point estimate matches non-CI version", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4, 3, 4, 4, 4, 3, 2, 3, 3, 4, 3, 1, 2, 3, 2, 3),
    nrow = 5
  )
  pe <- scvi_ave(ratings)
  with_ci <- scvi_ave(ratings, ci = TRUE, n_boot = 500, seed = 1)
  expect_equal(with_ci$scvi_ave, pe)
})

test_that("scvi_ave bootstrap CI brackets the point estimate and is in [0,1]", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4, 3, 4, 4, 4, 3, 2, 3, 3, 4, 3, 1, 2, 3, 2, 3),
    nrow = 5
  )
  result <- scvi_ave(ratings, ci = TRUE, n_boot = 500, seed = 42)
  expect_true(result$ci_lower <= result$scvi_ave)
  expect_true(result$ci_upper >= result$scvi_ave)
  expect_true(result$ci_lower >= 0)
  expect_true(result$ci_upper <= 1)
})

test_that("scvi_ave BCa interval is valid", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4, 3, 4, 4, 3, 4,
      2, 3, 3, 4, 3, 3, 2, 4, 3, 3),
    nrow = 10
  )
  result <- scvi_ave(ratings, ci = TRUE, ci_method = "bca",
                     n_boot = 500, seed = 1)
  expect_equal(result$ci_method, "bca")
  expect_true(result$ci_lower <= result$scvi_ave)
  expect_true(result$ci_upper >= result$scvi_ave)
})
