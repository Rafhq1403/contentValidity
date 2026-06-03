# ---------------------------------------------------------------------
# Validation against the canonical irrCAC implementation
# ---------------------------------------------------------------------

test_that("gwet_ac2 matches the published worked example", {
  # Worked example from manual calculation of irrCAC::gwet.ac1.raw() with
  # weights = "quadratic" and full categories 1:4:
  #   ratings = c(4, 4, 3, 4) on a 1-4 scale
  #   Quadratic W on c(1,2,3,4) sums to 11.5556 (= 1 + 0.8889 + 0.5556 +
  #     0 + 0.8889 + 1 + 0.8889 + 0.5556 + 0.5556 + 0.8889 + 1 + 0.8889 +
  #     0 + 0.5556 + 0.8889 + 1)
  #   n_k = (0, 0, 1, 3), r_i = 4
  #   p_a = 0.9444  (11.3333 / 12)
  #   p_e = 11.5556 * 0.375 / 12 = 0.3611
  #   AC2 = (0.9444 - 0.3611) / (1 - 0.3611) = 0.9130
  ratings <- matrix(c(4, 4, 3, 4), nrow = 4,
                    dimnames = list(NULL, "item1"))
  result <- gwet_ac2(ratings, weights = "quadratic", categories = 1:4)
  expect_equal(unname(result), 0.9130, tolerance = 1e-3)
})

test_that("gwet_ac2 collapses to a smaller value when categories are inferred", {
  # When categories are inferred from the data, only {3, 4} are observed,
  # the weight matrix becomes 2x2 with quadratic weights = identity,
  # and the result drops dramatically (this is the irrCAC gotcha).
  ratings <- matrix(c(4, 4, 3, 4), nrow = 4)
  expect_warning(
    result <- gwet_ac2(ratings, weights = "quadratic"),
    "categories.*not supplied"
  )
  expect_equal(unname(result), 0.2, tolerance = 1e-3)
})


# ---------------------------------------------------------------------
# Point-estimate behaviour
# ---------------------------------------------------------------------

test_that("gwet_ac2 returns 1 under perfect agreement", {
  expect_equal(gwet_ac2(c(4, 4, 4, 4), categories = 1:4,
                        weights = "quadratic"), 1)
  expect_equal(gwet_ac2(c(2, 2, 2), categories = 1:4,
                        weights = "linear"), 1)
})

test_that("gwet_ac2 with identity weights equals AC1 on raw categories", {
  # When weights = "identity" and the rating scale is binary (relevant /
  # not relevant), AC2 should reproduce AC1 on the same dichotomized data.
  # Here we use a 0/1 scale to keep it binary.
  ratings <- matrix(c(1, 1, 1, 1, 1, 0, 0, 0, 1, 1), nrow = 5,
                    dimnames = list(NULL, c("item1", "item2")))
  ac2_id <- gwet_ac2(ratings, weights = "identity", categories = 0:1)
  # gwet_ac1 dichotomizes at threshold; here we set threshold = 1 so
  # ratings >= 1 are treated as relevant. Item 1: 5 of 5; item 2: 2 of 5.
  ac1 <- gwet_ac1(ratings, relevant_threshold = 1)
  expect_equal(unname(ac2_id), unname(ac1), tolerance = 1e-10)
})

test_that("gwet_ac2 quadratic > linear > identity weights on ordinal data", {
  # Quadratic weights credit adjacent-category agreement more generously
  # than linear, and both more than identity. So AC2 should be ordered:
  # quadratic >= linear >= identity (when ratings cluster but disagree
  # by one category).
  ratings <- matrix(c(3, 3, 4, 4, 4), nrow = 5)
  ac2_q  <- gwet_ac2(ratings, weights = "quadratic", categories = 1:4)
  ac2_l  <- gwet_ac2(ratings, weights = "linear",    categories = 1:4)
  ac2_i  <- gwet_ac2(ratings, weights = "identity",  categories = 1:4)
  expect_gte(ac2_q, ac2_l)
  expect_gte(ac2_l, ac2_i)
})

test_that("gwet_ac2 handles a matrix and preserves names", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, c("item1", "item2"))
  )
  result <- gwet_ac2(ratings, categories = 1:4)
  expect_named(result, c("item1", "item2"))
})

test_that("gwet_ac2 handles NA according to na.rm", {
  expect_true(is.na(gwet_ac2(c(4, 4, NA, 4), categories = 1:4)))
  expect_equal(gwet_ac2(c(4, 4, NA, 4), categories = 1:4,
                        na.rm = TRUE), 1)
})

test_that("gwet_ac2 returns NA for fewer than two ratings", {
  expect_true(is.na(gwet_ac2(c(4), categories = 1:4)))
  expect_true(is.na(gwet_ac2(numeric(0), categories = 1:4)))
})

test_that("gwet_ac2 accepts custom weight matrix", {
  # 4-category quadratic weight matrix, computed manually
  cv <- 1:4
  W <- outer(cv, cv, function(k, l) 1 - (k - l)^2 / (max(cv) - min(cv))^2)
  result <- gwet_ac2(c(4, 4, 3, 4), weights = W, categories = 1:4)
  # Should match the worked example
  expect_equal(unname(result), 0.9130, tolerance = 1e-3)
})

test_that("gwet_ac2 rejects bad input", {
  expect_error(gwet_ac2("not numeric"), "must be numeric")
  expect_error(gwet_ac2(c(1, 2, 3), na.rm = "yes"), "TRUE or FALSE")
  expect_error(gwet_ac2(c(1, 2, 3), weights = matrix(1:6, 2, 3),
                        categories = 1:3),
               "square")
  expect_error(gwet_ac2(c(1, 2, 3), weights = diag(4),
                        categories = 1:3),
               "length must match")
  expect_error(gwet_ac2(c(1, 2, 3), weights = diag(3)),
               "categories.*must.*also be supplied")
})


# ---------------------------------------------------------------------
# Bootstrap confidence intervals
# ---------------------------------------------------------------------

test_that("gwet_ac2(ci = TRUE) returns a data frame with documented columns", {
  ratings <- matrix(c(4, 4, 3, 4, 4, 1, 2, 3, 2, 3), nrow = 5,
                    dimnames = list(NULL, c("item1", "item2")))
  result <- gwet_ac2(ratings, categories = 1:4, ci = TRUE,
                     n_boot = 200, seed = 1)
  expect_s3_class(result, "data.frame")
  expect_named(result,
               c("item", "gwet_ac2", "ci_lower", "ci_upper",
                 "ci_method", "conf_level", "n_boot"))
  expect_equal(nrow(result), 2)
})

test_that("gwet_ac2 bootstrap point estimate matches non-CI version", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4, 2, 3, 3, 4, 3, 1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, paste0("item", 1:3))
  )
  pe <- gwet_ac2(ratings, categories = 1:4)
  with_ci <- gwet_ac2(ratings, categories = 1:4,
                      ci = TRUE, n_boot = 500, seed = 1)
  expect_equal(with_ci$gwet_ac2, unname(pe))
})

test_that("gwet_ac2 bootstrap CI brackets the point estimate", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4, 2, 3, 3, 4, 3, 1, 2, 3, 2, 3),
    nrow = 5
  )
  result <- gwet_ac2(ratings, categories = 1:4,
                     ci = TRUE, n_boot = 500, seed = 42)
  expect_true(all(result$ci_lower <= result$gwet_ac2))
  expect_true(all(result$ci_upper >= result$gwet_ac2))
  expect_true(all(result$ci_upper <= 1))
})

test_that("gwet_ac2 bootstrap is reproducible with seed", {
  ratings <- matrix(c(4, 4, 3, 4, 2, 3, 3, 4, 1, 2, 3, 2), nrow = 4)
  a <- gwet_ac2(ratings, categories = 1:4,
                ci = TRUE, n_boot = 200, seed = 7)
  b <- gwet_ac2(ratings, categories = 1:4,
                ci = TRUE, n_boot = 200, seed = 7)
  expect_equal(a, b)
})
