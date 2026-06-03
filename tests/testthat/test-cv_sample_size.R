# ---------------------------------------------------------------------
# Wald (closed-form) method
# ---------------------------------------------------------------------

test_that("Wald sample size matches the closed-form formula", {
  # n = ceiling(z^2 * p * (1-p) / w^2)
  z   <- qnorm(0.975)
  p   <- 0.85
  w   <- 0.10
  exp_n <- ceiling(z^2 * p * (1 - p) / w^2)
  expect_equal(cv_sample_size_icvi(expected = p, half_width = w,
                                    method = "wald"),
               as.integer(exp_n))
})

test_that("Wald respects confidence level", {
  # At conf = 0.99, z is larger, so n should be larger than at conf = 0.95
  n95 <- cv_sample_size_icvi(0.85, 0.10, conf_level = 0.95, method = "wald")
  n99 <- cv_sample_size_icvi(0.85, 0.10, conf_level = 0.99, method = "wald")
  expect_gt(n99, n95)
})

test_that("Wald sample size grows as half_width shrinks", {
  n_loose  <- cv_sample_size_icvi(0.85, 0.15, method = "wald")
  n_tight  <- cv_sample_size_icvi(0.85, 0.05, method = "wald")
  expect_gt(n_tight, n_loose)
})

test_that("Wald sample size is largest when p = 0.5", {
  # p(1-p) is maximised at p = 0.5
  n_extreme <- cv_sample_size_icvi(0.90, 0.10, method = "wald")
  n_middle  <- cv_sample_size_icvi(0.50, 0.10, method = "wald")
  expect_gt(n_middle, n_extreme)
})


# ---------------------------------------------------------------------
# Wilson method
# ---------------------------------------------------------------------

test_that("Wilson returns a positive integer for typical inputs", {
  n <- cv_sample_size_icvi(0.85, 0.10, method = "wilson")
  expect_type(n, "integer")
  expect_gt(n, 0)
})

test_that("Wilson achieves the requested half-width at the returned n", {
  # At n_returned, the Wilson half-width should be <= target.
  # At n_returned - 1, it should be > target (this validates the ceiling).
  p   <- 0.85
  w   <- 0.10
  z   <- qnorm(0.975)
  hw  <- function(n) z * sqrt(p*(1-p)/n + z^2/(4*n^2)) / (1 + z^2/n)
  n   <- cv_sample_size_icvi(p, w, method = "wilson")
  expect_lte(hw(n), w + 1e-9)
  expect_gt(hw(n - 1), w)
})

test_that("Wilson and Wald give similar results in the central range", {
  # For p around 0.5 the two methods should give very close numbers.
  n_wald   <- cv_sample_size_icvi(0.50, 0.10, method = "wald")
  n_wilson <- cv_sample_size_icvi(0.50, 0.10, method = "wilson")
  expect_lt(abs(n_wald - n_wilson), 5)
})

test_that("Wilson requires at least as many experts as Wald near the boundary", {
  # For p near 1 with a tight half-width, Wald is known to be
  # anti-conservative (under-covering the true proportion). Wilson
  # corrects for this by widening the interval at finite n, which means
  # achieving the same target half-width requires more experts.
  # Newcombe (1998) discusses this property; Agresti and Coull (1998)
  # recommend Wilson for proportion CIs precisely because Wald gives
  # nominal coverage that is too low here.
  n_wald   <- cv_sample_size_icvi(0.95, 0.05, method = "wald")
  n_wilson <- cv_sample_size_icvi(0.95, 0.05, method = "wilson")
  expect_gte(n_wilson, n_wald)
})

test_that("Wilson warns and returns NA when target exceeds max_n", {
  expect_warning(
    n <- cv_sample_size_icvi(0.5, 1e-4, method = "wilson", max_n = 100),
    "exceeds.*max_n"
  )
  expect_true(is.na(n))
})


# ---------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------

test_that("argument validation works", {
  expect_error(cv_sample_size_icvi(expected = 0, half_width = 0.1),
               "strictly between 0 and 1")
  expect_error(cv_sample_size_icvi(expected = 1, half_width = 0.1),
               "strictly between 0 and 1")
  expect_error(cv_sample_size_icvi(expected = 0.85, half_width = 0),
               "strictly between 0 and 1")
  expect_error(cv_sample_size_icvi(expected = 0.85, half_width = 1.5),
               "strictly between 0 and 1")
  expect_error(cv_sample_size_icvi(expected = 0.85, half_width = 0.1,
                                    conf_level = 0),
               "strictly between 0 and 1")
  expect_error(cv_sample_size_icvi(expected = "x", half_width = 0.1),
               "single number")
  expect_error(cv_sample_size_icvi(expected = c(0.8, 0.9),
                                    half_width = 0.1),
               "single number")
})


# ---------------------------------------------------------------------
# Sanity checks vs. published rule-of-thumb
# ---------------------------------------------------------------------

test_that("returns >= 6 for the typical Lynn (1986) target", {
  # Lynn's rule of thumb is "at least 6 experts" with I-CVI >= 0.78. For
  # an expected I-CVI of 0.85 with a moderate half-width (0.15), the
  # function should require more experts than the rule of thumb -- which
  # is the methodological point we want to make in the paper.
  n <- cv_sample_size_icvi(0.85, 0.15)
  expect_gte(n, 6)
})
