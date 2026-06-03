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


# ---------------------------------------------------------------------
# v0.2.0: bootstrap confidence intervals
# ---------------------------------------------------------------------

test_that("icvi(ci = FALSE) preserves v0.1.0 return type and values", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, c("item1", "item2"))
  )
  old_style <- icvi(ratings)
  expect_type(old_style, "double")
  expect_length(old_style, 2)
  expect_named(old_style, c("item1", "item2"))
  expect_equal(unname(old_style), c(1, 2 / 5))
})

test_that("icvi(ci = TRUE) returns a data frame with the documented columns", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, c("item1", "item2"))
  )
  result <- icvi(ratings, ci = TRUE, n_boot = 200, seed = 1)
  expect_s3_class(result, "data.frame")
  expect_named(result,
               c("item", "icvi", "ci_lower", "ci_upper",
                 "ci_method", "conf_level", "n_boot"))
  expect_equal(nrow(result), 2)
  expect_equal(result$item, c("item1", "item2"))
  expect_equal(result$ci_method, rep("percentile", 2))
  expect_equal(result$conf_level, rep(0.95, 2))
  expect_equal(result$n_boot, rep(200L, 2))
})

test_that("icvi bootstrap point estimate matches the non-CI version", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      2, 3, 3, 4, 3,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, paste0("item", 1:3))
  )
  pe <- icvi(ratings)
  with_ci <- icvi(ratings, ci = TRUE, n_boot = 500, seed = 1)
  expect_equal(with_ci$icvi, unname(pe))
})

test_that("icvi bootstrap CI brackets the point estimate", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      2, 3, 3, 4, 3,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, paste0("item", 1:3))
  )
  result <- icvi(ratings, ci = TRUE, n_boot = 500, seed = 42)
  # Lower bound never exceeds the point estimate; upper bound never below
  expect_true(all(result$ci_lower <= result$icvi))
  expect_true(all(result$ci_upper >= result$icvi))
  # CIs are inside the [0, 1] proportion range
  expect_true(all(result$ci_lower >= 0))
  expect_true(all(result$ci_upper <= 1))
})

test_that("icvi bootstrap is reproducible with seed", {
  ratings <- matrix(c(4, 4, 3, 4, 2, 3, 3, 4, 1, 2, 3, 2),
                    nrow = 4)
  a <- icvi(ratings, ci = TRUE, n_boot = 200, seed = 7)
  b <- icvi(ratings, ci = TRUE, n_boot = 200, seed = 7)
  expect_equal(a, b)
})

test_that("icvi BCa intervals are valid for typical input", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4, 3, 4, 4, 3, 4,
      2, 3, 3, 4, 3, 3, 2, 4, 3, 3),
    nrow = 10,
    dimnames = list(NULL, c("item1", "item2"))
  )
  result <- icvi(ratings, ci = TRUE, ci_method = "bca",
                 n_boot = 500, seed = 1)
  expect_equal(result$ci_method, rep("bca", 2))
  expect_true(all(result$ci_lower <= result$icvi))
  expect_true(all(result$ci_upper >= result$icvi))
  expect_true(all(result$ci_lower >= 0))
  expect_true(all(result$ci_upper <= 1))
})

test_that("icvi bootstrap accepts a single-item vector", {
  result <- icvi(c(4, 4, 3, 3, 4), ci = TRUE, n_boot = 200, seed = 1)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$icvi, 1)
})

test_that("conf_level affects CI width", {
  ratings <- matrix(c(4, 4, 3, 2, 3, 4, 3, 3, 4, 3), nrow = 5)
  narrow <- icvi(ratings, ci = TRUE, conf_level = 0.50,
                 n_boot = 500, seed = 1)
  wide   <- icvi(ratings, ci = TRUE, conf_level = 0.99,
                 n_boot = 500, seed = 1)
  expect_true(all((wide$ci_upper - wide$ci_lower) >=
                  (narrow$ci_upper - narrow$ci_lower)))
})

test_that("bootstrap argument validation", {
  ratings <- matrix(c(4, 4, 3, 2, 3, 4), nrow = 3)
  expect_error(icvi(ratings, ci = TRUE, n_boot = 50),
               "n_boot.*100")
  expect_error(icvi(ratings, ci = TRUE, conf_level = 1.5),
               "between 0 and 1")
  expect_error(icvi(ratings, ci = "yes"), "TRUE or FALSE")
  expect_error(icvi(ratings, ci = TRUE, ci_method = "bootstrap-t"),
               "should be one of")
})
