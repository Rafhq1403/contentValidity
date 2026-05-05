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
