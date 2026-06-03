# ---------------------------------------------------------------------
# Point-estimate behaviour
# ---------------------------------------------------------------------

test_that("gwet_ac1 returns 1 when all experts agree relevant", {
  # 5 of 5 relevant: n_R = 5, n_I = 0, p_a = (5*4 + 0)/20 = 1, pi = 1,
  #                  p_e = 2*1*0 = 0; AC1 = (1 - 0)/(1 - 0) = 1
  expect_equal(gwet_ac1(c(4, 4, 4, 4, 4)), 1)
  expect_equal(gwet_ac1(c(3, 3, 3, 3, 3)), 1)
})

test_that("gwet_ac1 returns 1 when all experts agree NOT relevant", {
  # 0 of 5 relevant: same situation by symmetry
  expect_equal(gwet_ac1(c(1, 1, 1, 1, 1)), 1)
  expect_equal(gwet_ac1(c(2, 2, 2, 2, 2)), 1)
})

test_that("gwet_ac1 matches manual calculation", {
  # 4 of 5 relevant: n_R=4, n_I=1
  # p_a = (4*3 + 1*0) / (5*4) = 12/20 = 0.6
  # pi = 0.8, p_e = 2 * 0.8 * 0.2 = 0.32
  # AC1 = (0.6 - 0.32) / (1 - 0.32) = 0.28 / 0.68
  expected <- (0.6 - 0.32) / (1 - 0.32)
  expect_equal(gwet_ac1(c(4, 4, 3, 4, 1)), expected)
})

test_that("gwet_ac1 and mod_kappa yield different answers at high prevalence", {
  # At high prevalence, the two indices use different chance models and
  # therefore give substantively different answers -- which is the main
  # reason to report both. Polit's modified kappa uses a fixed null
  # (P_c = C(N,A) * 0.5^N, the binomial probability), so when prevalence
  # is far from 0.5 the P_c term shrinks and modified kappa approaches
  # I-CVI. Gwet's AC1 uses a marginal-adjusted null (p_e = 2*pi*(1-pi)),
  # so it discounts a larger share of observed agreement as prevalence-
  # driven. The result: mod_kappa > AC1 when prevalence is high.
  ratings <- c(4, 4, 4, 4, 4, 4, 4, 4, 1, 1)  # 8 of 10 relevant
  ac1 <- gwet_ac1(ratings)
  mk  <- mod_kappa(ratings)
  expect_false(isTRUE(all.equal(ac1, mk)))
  # At this prevalence, modified kappa is the more generous index
  expect_gt(mk, ac1)
})

test_that("gwet_ac1 handles a matrix and preserves item names", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, c("item1", "item2"))
  )
  result <- gwet_ac1(ratings)
  expect_named(result, c("item1", "item2"))
  expect_equal(unname(result[1]), 1)  # 5 of 5 relevant
})

test_that("gwet_ac1 accepts a data frame", {
  df <- data.frame(item1 = c(4, 4, 4), item2 = c(1, 1, 1))
  result <- gwet_ac1(df)
  expect_named(result, c("item1", "item2"))
  # Both items have unanimous agreement (one direction or the other)
  expect_equal(unname(result), c(1, 1))
})

test_that("gwet_ac1 respects relevant_threshold", {
  # All 3 experts rate exactly 3 -- all "relevant" at threshold 3, all
  # "not relevant" at threshold 4. Both should give AC1 = 1 (unanimous).
  expect_equal(gwet_ac1(c(3, 3, 3)), 1)
  expect_equal(gwet_ac1(c(3, 3, 3), relevant_threshold = 4), 1)
})

test_that("gwet_ac1 handles NA according to na.rm", {
  expect_true(is.na(gwet_ac1(c(4, 4, NA, 4))))
  expect_equal(gwet_ac1(c(4, 4, NA, 4), na.rm = TRUE), 1)
})

test_that("gwet_ac1 returns NA for fewer than two experts", {
  expect_true(is.na(gwet_ac1(c(4))))
  expect_true(is.na(gwet_ac1(numeric(0))))
})

test_that("gwet_ac1 rejects invalid input", {
  expect_error(gwet_ac1("not numeric"), "must be numeric")
  expect_error(gwet_ac1(c(1, 2, 3), relevant_threshold = "bad"),
               "single number")
  expect_error(gwet_ac1(c(1, 2, 3), na.rm = "yes"), "TRUE or FALSE")
})


# ---------------------------------------------------------------------
# Bootstrap confidence intervals
# ---------------------------------------------------------------------

test_that("gwet_ac1(ci = TRUE) returns a data frame with documented columns", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, c("item1", "item2"))
  )
  result <- gwet_ac1(ratings, ci = TRUE, n_boot = 200, seed = 1)
  expect_s3_class(result, "data.frame")
  expect_named(result,
               c("item", "gwet_ac1", "ci_lower", "ci_upper",
                 "ci_method", "conf_level", "n_boot"))
  expect_equal(nrow(result), 2)
})

test_that("gwet_ac1 bootstrap point estimate matches non-CI version", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      2, 3, 3, 4, 3,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, paste0("item", 1:3))
  )
  pe <- gwet_ac1(ratings)
  with_ci <- gwet_ac1(ratings, ci = TRUE, n_boot = 500, seed = 1)
  expect_equal(with_ci$gwet_ac1, unname(pe))
})

test_that("gwet_ac1 bootstrap CI brackets the point estimate", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      2, 3, 3, 4, 3,
      1, 2, 3, 2, 3),
    nrow = 5
  )
  result <- gwet_ac1(ratings, ci = TRUE, n_boot = 500, seed = 42)
  expect_true(all(result$ci_lower <= result$gwet_ac1))
  expect_true(all(result$ci_upper >= result$gwet_ac1))
  # AC1 can be slightly negative when observed agreement is below chance,
  # but it cannot exceed 1.
  expect_true(all(result$ci_upper <= 1))
})

test_that("gwet_ac1 bootstrap is reproducible with seed", {
  ratings <- matrix(c(4, 4, 3, 4, 2, 3, 3, 4, 1, 2, 3, 2),
                    nrow = 4)
  a <- gwet_ac1(ratings, ci = TRUE, n_boot = 200, seed = 7)
  b <- gwet_ac1(ratings, ci = TRUE, n_boot = 200, seed = 7)
  expect_equal(a, b)
})

test_that("gwet_ac1 BCa intervals are valid", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4, 3, 4, 4, 3, 4,
      2, 3, 3, 4, 3, 3, 2, 4, 3, 3),
    nrow = 10
  )
  result <- gwet_ac1(ratings, ci = TRUE, ci_method = "bca",
                     n_boot = 500, seed = 1)
  expect_equal(result$ci_method, rep("bca", 2))
  expect_true(all(result$ci_lower <= result$gwet_ac1))
  expect_true(all(result$ci_upper >= result$gwet_ac1))
})

test_that("gwet_ac1 bootstrap argument validation", {
  ratings <- matrix(c(4, 4, 3, 2, 3, 4), nrow = 3)
  expect_error(gwet_ac1(ratings, ci = TRUE, n_boot = 50),
               "n_boot.*100")
  expect_error(gwet_ac1(ratings, ci = TRUE, conf_level = 1.5),
               "between 0 and 1")
  expect_error(gwet_ac1(ratings, ci = "yes"), "TRUE or FALSE")
})
