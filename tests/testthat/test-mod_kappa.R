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
