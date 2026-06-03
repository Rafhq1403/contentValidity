test_that("plot.content_validity invisibly returns x", {
  data(cvi_example)
  cv <- content_validity(cvi_example)
  pdf(NULL)        # open null device so plot() doesn't render
  on.exit(dev.off(), add = TRUE)
  out <- plot(cv)
  expect_identical(out, cv)
})

test_that("plot.content_validity errors when y_index column is missing", {
  data(cvi_example)
  cv <- content_validity(cvi_example)
  cv$items$gwet_ac1 <- NULL
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(plot(cv, y_index = "gwet_ac1"),
               "does not carry")
})

test_that("plot.content_validity accepts each y_index without error", {
  data(cvi_example)
  cv <- content_validity(cvi_example)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_silent(plot(cv, y_index = "mod_kappa"))
  expect_silent(plot(cv, y_index = "gwet_ac1"))
  expect_silent(plot(cv, y_index = "gwet_ac2"))
  expect_silent(plot(cv, y_index = "aiken_v"))
})

test_that("plot.content_validity respects label argument", {
  data(cvi_example)
  cv <- content_validity(cvi_example)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_silent(plot(cv, label = "flagged"))
  expect_silent(plot(cv, label = "all"))
  expect_silent(plot(cv, label = "none"))
})

test_that("plot.content_validity respects custom thresholds", {
  data(cvi_example)
  cv <- content_validity(cvi_example)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_silent(plot(cv,
                     flag_threshold_icvi = 0.90,
                     flag_threshold_y    = 0.85))
})

test_that("plot.content_validity accepts each flag_logic value", {
  data(cvi_example)
  cv <- content_validity(cvi_example)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_silent(plot(cv, flag_logic = "any"))
  expect_silent(plot(cv, flag_logic = "icvi"))
  expect_silent(plot(cv, flag_logic = "y_index"))
  expect_silent(plot(cv, flag_logic = "both"))
})
