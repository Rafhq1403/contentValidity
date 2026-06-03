test_that("mod_kappa returns 1 when all experts agree relevant", {
  expect_equal(mod_kappa(c(4, 4, 4, 4, 4)), 1)
  expect_equal(mod_kappa(c(3, 3, 3)), 1)
})

test_that("mod_kappa is slightly negative when no experts agree", {
  # N=5, A=0 -> Pc = 1 * 0.5^5 = 0.03125
  # kappa = (0 - 0.03125) / (1 - 0.03125)
  expected <- (0 - 0.5^5) / (1 - 0.5^5)
  expect_equal(mod_kappa(c(1, 1, 1, 1, 1)), expected)
  expect_lt(mod_kappa(c(1, 1, 1, 1, 1)), 0)
})

test_that("mod_kappa matches manual calculation", {
  # 4 of 5 experts rate relevant: I-CVI = 0.8, Pc = choose(5,4)*0.5^5 = 0.15625
  expected <- (0.8 - 5 * 0.5^5) / (1 - 5 * 0.5^5)
  expect_equal(mod_kappa(c(3, 3, 3, 4, 1)), expected)
})

test_that("mod_kappa handles a matrix and preserves item names", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, c("item1", "item2"))
  )
  result <- mod_kappa(ratings)
  expect_named(result, c("item1", "item2"))
  expect_equal(unname(result[1]), 1)
  expect_equal(unname(result[2]),
               (2/5 - choose(5, 2) * 0.5^5) / (1 - choose(5, 2) * 0.5^5))
})

test_that("mod_kappa accepts a data frame", {
  df <- data.frame(item1 = c(4, 4, 4), item2 = c(1, 1, 1))
  result <- mod_kappa(df)
  expect_named(result, c("item1", "item2"))
  expect_equal(unname(result[1]), 1)
})

test_that("mod_kappa respects relevant_threshold", {
  # All experts rate exactly 3
  expect_equal(mod_kappa(c(3, 3, 3)), 1)
  expect_lt(mod_kappa(c(3, 3, 3), relevant_threshold = 4), 0)
})

test_that("mod_kappa handles NA according to na.rm", {
  expect_true(is.na(mod_kappa(c(4, 4, NA, 4))))
  expect_equal(mod_kappa(c(4, 4, NA, 4), na.rm = TRUE), 1)
})

test_that("mod_kappa rejects bad input", {
  expect_error(mod_kappa("not numeric"), "must be numeric")
  expect_error(mod_kappa(c(1, 2, 3), relevant_threshold = "bad"),
               "single number")
  expect_error(mod_kappa(c(1, 2, 3), na.rm = "yes"), "TRUE or FALSE")
})


# ---------------------------------------------------------------------
# v0.2.0: bootstrap confidence intervals
# ---------------------------------------------------------------------

test_that("mod_kappa(ci = FALSE) preserves v0.1.0 return type and values", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, c("item1", "item2"))
  )
  old_style <- mod_kappa(ratings)
  expect_type(old_style, "double")
  expect_length(old_style, 2)
  expect_named(old_style, c("item1", "item2"))
})

test_that("mod_kappa(ci = TRUE) returns a data frame with the documented columns", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, c("item1", "item2"))
  )
  result <- mod_kappa(ratings, ci = TRUE, n_boot = 200, seed = 1)
  expect_s3_class(result, "data.frame")
  expect_named(result,
               c("item", "mod_kappa", "ci_lower", "ci_upper",
                 "ci_method", "conf_level", "n_boot"))
  expect_equal(nrow(result), 2)
  expect_equal(result$item, c("item1", "item2"))
  expect_equal(result$ci_method, rep("percentile", 2))
  expect_equal(result$conf_level, rep(0.95, 2))
  expect_equal(result$n_boot, rep(200L, 2))
})

test_that("mod_kappa bootstrap point estimate matches the non-CI version", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      2, 3, 3, 4, 3,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, paste0("item", 1:3))
  )
  pe <- mod_kappa(ratings)
  with_ci <- mod_kappa(ratings, ci = TRUE, n_boot = 500, seed = 1)
  expect_equal(with_ci$mod_kappa, unname(pe))
})

test_that("mod_kappa bootstrap CI brackets the point estimate", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      2, 3, 3, 4, 3,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, paste0("item", 1:3))
  )
  result <- mod_kappa(ratings, ci = TRUE, n_boot = 500, seed = 42)
  expect_true(all(result$ci_lower <= result$mod_kappa))
  expect_true(all(result$ci_upper >= result$mod_kappa))
})

test_that("mod_kappa bootstrap is reproducible with seed", {
  ratings <- matrix(c(4, 4, 3, 4, 2, 3, 3, 4, 1, 2, 3, 2),
                    nrow = 4)
  a <- mod_kappa(ratings, ci = TRUE, n_boot = 200, seed = 7)
  b <- mod_kappa(ratings, ci = TRUE, n_boot = 200, seed = 7)
  expect_equal(a, b)
})

test_that("mod_kappa BCa intervals are valid", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4, 3, 4, 4, 3, 4,
      2, 3, 3, 4, 3, 3, 2, 4, 3, 3),
    nrow = 10,
    dimnames = list(NULL, c("item1", "item2"))
  )
  result <- mod_kappa(ratings, ci = TRUE, ci_method = "bca",
                     n_boot = 500, seed = 1)
  expect_equal(result$ci_method, rep("bca", 2))
  expect_true(all(result$ci_lower <= result$mod_kappa))
  expect_true(all(result$ci_upper >= result$mod_kappa))
})

test_that("mod_kappa bootstrap accepts a single-item vector", {
  result <- mod_kappa(c(4, 4, 3, 3, 4), ci = TRUE, n_boot = 200, seed = 1)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$mod_kappa, 1)
})

test_that("mod_kappa bootstrap argument validation", {
  ratings <- matrix(c(4, 4, 3, 2, 3, 4), nrow = 3)
  expect_error(mod_kappa(ratings, ci = TRUE, n_boot = 50),
               "n_boot.*100")
  expect_error(mod_kappa(ratings, ci = TRUE, conf_level = 1.5),
               "between 0 and 1")
  expect_error(mod_kappa(ratings, ci = "yes"), "TRUE or FALSE")
})
