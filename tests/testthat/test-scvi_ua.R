test_that("scvi_ua returns the proportion of items with universal agreement", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,    # all relevant
      3, 4, 4, 4, 3,    # all relevant
      2, 3, 3, 4, 3,    # not all
      1, 2, 3, 2, 3),   # not all
    nrow = 5
  )
  expect_equal(scvi_ua(ratings), 0.5)
})

test_that("scvi_ua returns 1 when every item has universal agreement", {
  ratings <- matrix(c(4, 4, 4, 3, 3, 3), nrow = 3)
  expect_equal(scvi_ua(ratings), 1)
})

test_that("scvi_ua returns 0 when no item has universal agreement", {
  ratings <- matrix(c(2, 4, 1, 4), nrow = 2)
  expect_equal(scvi_ua(ratings), 0)
})

test_that("scvi_ua respects relevant_threshold", {
  ratings <- matrix(c(3, 3, 4, 4), nrow = 2)
  expect_equal(scvi_ua(ratings), 1)
  expect_equal(scvi_ua(ratings, relevant_threshold = 4), 0.5)
})

test_that("scvi_ua works on a data frame", {
  df <- data.frame(item1 = c(4, 4, 4), item2 = c(2, 3, 4))
  expect_equal(scvi_ua(df), 0.5)
})

test_that("scvi_ua rejects a single-item vector", {
  expect_error(scvi_ua(c(4, 4, 3, 3)), "single item")
})

test_that("scvi_ua rejects bad input", {
  expect_error(scvi_ua("not numeric"), "must be numeric")
  expect_error(scvi_ua(matrix(1:6, nrow = 3), relevant_threshold = "bad"),
               "single number")
})

test_that("scvi_ua handles NA according to na.rm", {
  ratings <- matrix(
    c(4, NA, 4, 4,
      2,  3, 4, 4),
    nrow = 4
  )
  expect_true(is.na(scvi_ua(ratings)))
  expect_equal(scvi_ua(ratings, na.rm = TRUE), 0.5)
})


# ---------------------------------------------------------------------
# v0.2.0: bootstrap confidence intervals
# ---------------------------------------------------------------------

test_that("scvi_ua(ci = FALSE) preserves v0.1.0 scalar return", {
  ratings <- matrix(c(4, 4, 3, 4, 4, 1, 2, 3, 2, 3), nrow = 5)
  expect_type(scvi_ua(ratings), "double")
  expect_length(scvi_ua(ratings), 1)
})

test_that("scvi_ua(ci = TRUE) returns a one-row data frame", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      3, 4, 4, 4, 3,
      2, 3, 3, 4, 3,
      1, 2, 3, 2, 3),
    nrow = 5
  )
  result <- scvi_ua(ratings, ci = TRUE, n_boot = 200, seed = 1)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_named(result,
               c("item", "scvi_ua", "ci_lower", "ci_upper",
                 "ci_method", "conf_level", "n_boot"))
  expect_equal(result$item, "scale")
})

test_that("scvi_ua bootstrap point estimate matches non-CI version", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4, 3, 4, 4, 4, 3, 2, 3, 3, 4, 3, 1, 2, 3, 2, 3),
    nrow = 5
  )
  pe <- scvi_ua(ratings)
  with_ci <- scvi_ua(ratings, ci = TRUE, n_boot = 500, seed = 1)
  expect_equal(with_ci$scvi_ua, pe)
})

test_that("scvi_ua bootstrap CI brackets the point estimate and is in [0,1]", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4, 3, 4, 4, 4, 3, 2, 3, 3, 4, 3, 1, 2, 3, 2, 3),
    nrow = 5
  )
  result <- scvi_ua(ratings, ci = TRUE, n_boot = 500, seed = 42)
  expect_true(result$ci_lower <= result$scvi_ua)
  expect_true(result$ci_upper >= result$scvi_ua)
  expect_true(result$ci_lower >= 0)
  expect_true(result$ci_upper <= 1)
})
