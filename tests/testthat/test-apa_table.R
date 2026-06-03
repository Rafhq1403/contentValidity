# Build a small content_validity object once for use across tests
make_cv <- function() {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,    # icvi 1.00, kappa 1.00 -> Excellent
      3, 4, 4, 4, 3,    # icvi 1.00, kappa 1.00 -> Excellent
      2, 3, 3, 4, 3,    # icvi 0.80, kappa 0.76 -> Excellent
      1, 2, 3, 2, 3),   # icvi 0.40, kappa 0.13 -> Poor
    nrow = 5,
    dimnames = list(NULL, paste0("item", 1:4))
  )
  content_validity(ratings)
}


test_that("apa_table returns a data frame by default", {
  cv <- make_cv()
  tbl <- apa_table(cv)
  expect_s3_class(tbl, "data.frame")
  expect_equal(nrow(tbl), 4)
})

test_that("apa_table includes the expected columns by default", {
  cv <- make_cv()
  tbl <- apa_table(cv)
  expect_true(all(c("Item", "I-CVI", "Modified Kappa", "Aiken's V",
                    "Gwet's AC1", "Gwet's AC2",
                    "Kappa Interpretation") %in% names(tbl)))
})

test_that("apa_table interpretation_index switches the cutoffs and column name", {
  cv <- make_cv()

  # AC1 interpretation uses Altman cutoffs
  tbl_ac1 <- apa_table(cv, interpretation_index = "gwet_ac1")
  expect_true("AC1 Interpretation" %in% names(tbl_ac1))
  expect_false("Kappa Interpretation" %in% names(tbl_ac1))

  # AC2 interpretation
  tbl_ac2 <- apa_table(cv, interpretation_index = "gwet_ac2")
  expect_true("AC2 Interpretation" %in% names(tbl_ac2))

  # I-CVI interpretation
  tbl_icvi <- apa_table(cv, interpretation_index = "icvi")
  expect_true("I-CVI Interpretation" %in% names(tbl_icvi))
})

test_that("apa_table errors when interpretation_index column is missing", {
  cv <- make_cv()
  # Simulate v0.1.0 object
  cv$items$gwet_ac1 <- NULL
  expect_error(apa_table(cv, interpretation_index = "gwet_ac1"),
               "does not carry that")
})

test_that("apa_table omits AC1/AC2 if the cv object lacks them (v0.1.0 compat)", {
  cv <- make_cv()
  # Strip AC1/AC2 from the items data frame to simulate a v0.1.0
  # content_validity object
  cv$items$gwet_ac1 <- NULL
  cv$items$gwet_ac2 <- NULL
  tbl <- apa_table(cv)
  expect_false("Gwet's AC1" %in% names(tbl))
  expect_false("Gwet's AC2" %in% names(tbl))
  # Original columns still present
  expect_true(all(c("Item", "I-CVI", "Modified Kappa", "Aiken's V") %in%
                  names(tbl)))
})

test_that("apa_table omits interpretation column when requested", {
  cv <- make_cv()
  tbl <- apa_table(cv, interpretation = FALSE)
  expect_false("Kappa Interpretation" %in% names(tbl))
  expect_false("Interpretation" %in% names(tbl))
})

test_that("apa_table assigns kappa interpretation labels correctly", {
  cv <- make_cv()
  tbl <- apa_table(cv)
  # Item 4 has kappa ~0.13 -> Poor
  expect_equal(tbl$`Kappa Interpretation`[4], "Poor")
  # Item 1 has kappa = 1.00 -> Excellent
  expect_equal(tbl$`Kappa Interpretation`[1], "Excellent")
})

test_that("apa_table rounds to the requested digits", {
  cv <- make_cv()
  tbl <- apa_table(cv, digits = 4)
  expect_equal(tbl$`I-CVI`[4], round(0.4, 4))
  expect_equal(tbl$`Modified Kappa`[4],
               round((0.4 - 10 * 0.5^5) / (1 - 10 * 0.5^5), 4))
})

test_that("apa_table produces markdown output when requested", {
  skip_if_not_installed("knitr")
  cv <- make_cv()
  out <- apa_table(cv, format = "markdown")
  out_text <- paste(as.character(out), collapse = "\n")
  expect_match(out_text, "I-CVI")
  expect_match(out_text, "S-CVI/Ave")
})

test_that("apa_table accepts a custom caption", {
  skip_if_not_installed("knitr")
  cv <- make_cv()
  out <- apa_table(cv, format = "markdown",
                   caption = "Custom Test Caption")
  out_text <- paste(as.character(out), collapse = "\n")
  expect_match(out_text, "Custom Test Caption")
})

test_that("apa_table dispatches via S3 generic", {
  cv <- make_cv()
  expect_silent(apa_table(cv))
  # Calling on a non-content_validity object should fail
  expect_error(apa_table(list(foo = 1)), "no applicable method")
})
