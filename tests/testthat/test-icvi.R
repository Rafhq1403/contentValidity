test_that("icvi computes correctly for a single-item vector", {
  expect_equal(icvi(c(4, 4, 3, 2, 3)), 4 / 5)
  expect_equal(icvi(c(4, 4, 4, 4)), 1)
  expect_equal(icvi(c(1, 2, 1, 2)), 0)
})

test_that("icvi computes correctly for a matrix of items", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,    # Item 1: all 5 relevant -> 1.0
      3, 4, 4, 4, 3,    # Item 2: all 5 relevant -> 1.0
      2, 3, 3, 4, 3,    # Item 3: 4 of 5 relevant -> 0.8
      1, 2, 3, 2, 3),   # Item 4: 2 of 5 relevant -> 0.4
    nrow = 5
  )
  result <- icvi(ratings)

  expect_length(result, 4)
  expect_equal(unname(result[1]), 1)
  expect_equal(unname(result[2]), 1)
  expect_equal(unname(result[3]), 4 / 5)
  expect_equal(unname(result[4]), 2 / 5)
})

test_that("icvi accepts a data frame and preserves names", {
  df <- data.frame(item1 = c(4, 4, 3), item2 = c(2, 3, 4))
  result <- icvi(df)
  expect_named(result, c("item1", "item2"))
  expect_equal(unname(result), c(1, 2 / 3))
})

test_that("icvi respects relevant_threshold", {
  expect_equal(icvi(c(2, 3, 4)), 2 / 3)
  expect_equal(icvi(c(2, 3, 4), relevant_threshold = 4), 1 / 3)
})

test_that("icvi handles NA according to na.rm", {
  expect_true(is.na(icvi(c(4, 4, NA, 3))))
  expect_equal(icvi(c(4, 4, NA, 3), na.rm = TRUE), 1)
})

test_that("icvi rejects invalid input", {
  expect_error(icvi("not numeric"), "must be numeric")
  expect_error(icvi(c(1, 2, 3), relevant_threshold = "bad"),
               "single number")
  expect_error(icvi(c(1, 2, 3), na.rm = "yes"), "TRUE or FALSE")
})
