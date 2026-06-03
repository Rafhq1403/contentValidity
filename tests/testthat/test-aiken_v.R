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


# ---------------------------------------------------------------------
# v0.2.0: bootstrap confidence intervals
# ---------------------------------------------------------------------

test_that("aiken_v(ci = FALSE) preserves v0.1.0 return type and values", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, c("item1", "item2"))
  )
  old_style <- aiken_v(ratings)
  expect_type(old_style, "double")
  expect_length(old_style, 2)
  expect_named(old_style, c("item1", "item2"))
})

test_that("aiken_v(ci = TRUE) returns a data frame with the documented columns", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, c("item1", "item2"))
  )
  result <- aiken_v(ratings, ci = TRUE, n_boot = 200, seed = 1)
  expect_s3_class(result, "data.frame")
  expect_named(result,
               c("item", "aiken_v", "ci_lower", "ci_upper",
                 "ci_method", "conf_level", "n_boot"))
  expect_equal(nrow(result), 2)
})

test_that("aiken_v bootstrap point estimate matches the non-CI version", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      2, 3, 3, 4, 3,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, paste0("item", 1:3))
  )
  pe <- aiken_v(ratings)
  with_ci <- aiken_v(ratings, ci = TRUE, n_boot = 500, seed = 1)
  expect_equal(with_ci$aiken_v, unname(pe))
})

test_that("aiken_v bootstrap CI brackets the point estimate", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      2, 3, 3, 4, 3,
      1, 2, 3, 2, 3),
    nrow = 5
  )
  result <- aiken_v(ratings, ci = TRUE, n_boot = 500, seed = 42)
  expect_true(all(result$ci_lower <= result$aiken_v))
  expect_true(all(result$ci_upper >= result$aiken_v))
  expect_true(all(result$ci_lower >= 0))
  expect_true(all(result$ci_upper <= 1))
})

test_that("aiken_v bootstrap is reproducible with seed", {
  ratings <- matrix(c(4, 4, 3, 4, 2, 3, 3, 4, 1, 2, 3, 2),
                    nrow = 4)
  a <- aiken_v(ratings, ci = TRUE, n_boot = 200, seed = 7)
  b <- aiken_v(ratings, ci = TRUE, n_boot = 200, seed = 7)
  expect_equal(a, b)
})

test_that("aiken_v BCa intervals are valid", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4, 3, 4, 4, 3, 4,
      2, 3, 3, 4, 3, 3, 2, 4, 3, 3),
    nrow = 10
  )
  result <- aiken_v(ratings, ci = TRUE, ci_method = "bca",
                    n_boot = 500, seed = 1)
  expect_equal(result$ci_method, rep("bca", 2))
  expect_true(all(result$ci_lower <= result$aiken_v))
  expect_true(all(result$ci_upper >= result$aiken_v))
})

test_that("aiken_v bootstrap respects custom lo/hi scale", {
  result <- aiken_v(c(5, 4, 5, 5, 4), lo = 1, hi = 5,
                    ci = TRUE, n_boot = 200, seed = 1)
  expect_s3_class(result, "data.frame")
  expect_equal(result$aiken_v, (4.6 - 1) / 4)
})

test_that("aiken_v bootstrap argument validation", {
  ratings <- matrix(c(4, 4, 3, 2, 3, 4), nrow = 3)
  expect_error(aiken_v(ratings, ci = TRUE, n_boot = 50),
               "n_boot.*100")
  expect_error(aiken_v(ratings, ci = TRUE, conf_level = 1.5),
               "between 0 and 1")
  expect_error(aiken_v(ratings, ci = "yes"), "TRUE or FALSE")
})
