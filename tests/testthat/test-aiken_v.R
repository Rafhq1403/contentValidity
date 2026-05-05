test_that("aiken_v returns 1 when all ratings are at the maximum", {
  expect_equal(aiken_v(c(4, 4, 4, 4)), 1)
  expect_equal(aiken_v(c(5, 5, 5), lo = 1, hi = 5), 1)
})

test_that("aiken_v returns 0 when all ratings are at the minimum", {
  expect_equal(aiken_v(c(1, 1, 1, 1)), 0)
})

test_that("aiken_v matches manual calculation", {
  # mean = 3.8, V = (3.8 - 1) / 3
  expect_equal(aiken_v(c(4, 4, 3, 4, 4)), (3.8 - 1) / 3)
  # mean = 2.2, V = (2.2 - 1) / 3
  expect_equal(aiken_v(c(1, 2, 3, 2, 3)), (2.2 - 1) / 3)
})

test_that("aiken_v handles a matrix and preserves names", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, c("item1", "item2"))
  )
  result <- aiken_v(ratings)
  expect_named(result, c("item1", "item2"))
  expect_equal(unname(result[1]), (3.8 - 1) / 3)
  expect_equal(unname(result[2]), (2.2 - 1) / 3)
})

test_that("aiken_v accepts a data frame", {
  df <- data.frame(item1 = c(4, 4, 4), item2 = c(2, 2, 2))
  result <- aiken_v(df)
  expect_named(result, c("item1", "item2"))
  expect_equal(unname(result[1]), 1)
  expect_equal(unname(result[2]), (2 - 1) / 3)
})

test_that("aiken_v respects custom lo and hi", {
  expect_equal(aiken_v(c(7, 7, 7), lo = 0, hi = 10), 0.7)
  expect_equal(aiken_v(c(3, 3, 3), lo = 1, hi = 5), 0.5)
})

test_that("aiken_v rejects ratings outside [lo, hi]", {
  expect_error(aiken_v(c(1, 2, 5), lo = 1, hi = 4), "outside")
  expect_error(aiken_v(c(0, 2, 3), lo = 1, hi = 4), "outside")
})

test_that("aiken_v rejects bad input", {
  expect_error(aiken_v("not numeric"), "must be numeric")
  expect_error(aiken_v(c(1, 2, 3), lo = "bad"), "single number")
  expect_error(aiken_v(c(1, 2, 3), hi = "bad"), "single number")
  expect_error(aiken_v(c(1, 2, 3), lo = 4, hi = 4), "greater than")
  expect_error(aiken_v(c(1, 2, 3), lo = 5, hi = 4), "greater than")
  expect_error(aiken_v(c(1, 2, 3), na.rm = "yes"), "TRUE or FALSE")
})

test_that("aiken_v handles NA according to na.rm", {
  expect_true(is.na(aiken_v(c(4, 4, NA))))
  expect_equal(aiken_v(c(4, 4, NA), na.rm = TRUE), 1)
})
