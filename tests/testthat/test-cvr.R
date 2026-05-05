# --- cvr() ---------------------------------------------------------------

test_that("cvr returns 1 when all experts say essential", {
  expect_equal(cvr(c(1, 1, 1, 1, 1)), 1)
})

test_that("cvr returns -1 when no expert says essential", {
  expect_equal(cvr(c(2, 3, 2, 3, 3)), -1)
})

test_that("cvr returns 0 when half say essential", {
  expect_equal(cvr(c(1, 1, 2, 2)), 0)
})

test_that("cvr matches the formula for partial agreement", {
  # 8 of 10 essential -> CVR = (8 - 5) / 5 = 0.6
  expect_equal(cvr(c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2)), 0.6)
})

test_that("cvr handles a matrix of items", {
  ratings <- matrix(
    c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2,    # 8/10 essential
      1, 1, 1, 2, 2, 2, 2, 3, 3, 3,    # 3/10 essential
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1),   # 10/10 essential
    nrow = 10,
    dimnames = list(NULL, paste0("item", 1:3))
  )
  result <- cvr(ratings)
  expect_named(result, c("item1", "item2", "item3"))
  expect_equal(unname(result[1]), 0.6)
  expect_equal(unname(result[2]), -0.4)
  expect_equal(unname(result[3]), 1)
})

test_that("cvr accepts a data frame", {
  df <- data.frame(item1 = c(1, 1, 2, 2), item2 = c(1, 1, 1, 1))
  result <- cvr(df)
  expect_equal(unname(result[1]), 0)
  expect_equal(unname(result[2]), 1)
})

test_that("cvr respects the `essential` argument", {
  # Use 0/1 binary coding instead of Lawshe's 1/2/3
  expect_equal(cvr(c(1, 1, 1, 0, 0), essential = 1), 0.2)
  # Multiple values can count as essential
  expect_equal(cvr(c(1, 2, 1, 2, 3), essential = c(1, 2)), 0.6)
})

test_that("cvr rejects bad input", {
  expect_error(cvr("not numeric"), "must be numeric")
  expect_error(cvr(c(1, 2, 3), essential = "bad"),
               "one or more numeric values")
  expect_error(cvr(c(1, 2, 3), na.rm = "yes"), "TRUE or FALSE")
})

test_that("cvr handles NA according to na.rm", {
  expect_true(is.na(cvr(c(1, 1, NA, 1))))
  expect_equal(cvr(c(1, 1, NA, 1), na.rm = TRUE), 1)
})


# --- cvr_critical() ------------------------------------------------------

test_that("cvr_critical matches exact binomial values", {
  # N=5: need all 5 essential -> CVR_crit = 1.0
  expect_equal(cvr_critical(5), 1)
  # N=10: need 9 essential -> CVR_crit = 0.8
  expect_equal(cvr_critical(10), 0.8)
  # N=20: need 15 essential -> CVR_crit = 0.5
  expect_equal(cvr_critical(20), 0.5)
})

test_that("cvr_critical respects alpha", {
  # Stricter alpha requires higher CVR
  expect_gte(cvr_critical(20, alpha = 0.01), cvr_critical(20, alpha = 0.05))
})

test_that("cvr_critical decreases with larger panels at fixed alpha", {
  expect_gte(cvr_critical(10), cvr_critical(20))
  expect_gte(cvr_critical(20), cvr_critical(40))
})

test_that("cvr_critical rejects bad input", {
  expect_error(cvr_critical("ten"), "single positive integer")
  expect_error(cvr_critical(0), "single positive integer")
  expect_error(cvr_critical(c(5, 10)), "single positive integer")
  expect_error(cvr_critical(10, alpha = 0), "between 0 and 1")
  expect_error(cvr_critical(10, alpha = 1), "between 0 and 1")
  expect_error(cvr_critical(10, alpha = "bad"), "between 0 and 1")
})
