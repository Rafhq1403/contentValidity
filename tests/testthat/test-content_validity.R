test_that("content_validity returns the expected structure", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      3, 4, 4, 4, 3,
      2, 3, 3, 4, 3,
      1, 2, 3, 2, 3),
    nrow = 5,
    dimnames = list(NULL, paste0("item", 1:4))
  )
  result <- content_validity(ratings)

  expect_s3_class(result, "content_validity")
  expect_named(result, c("items", "scale", "n_experts", "n_items"))
  expect_equal(result$n_experts, 5)
  expect_equal(result$n_items, 4)
})

test_that("content_validity items data frame has correct columns and values", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,
      3, 4, 4, 4, 3,
      2, 3, 3, 4, 3,
      1, 2, 3, 2, 3),
    nrow = 5
  )
  result <- content_validity(ratings)

  expect_named(result$items, c("item", "icvi", "mod_kappa", "aiken_v",
                               "gwet_ac1", "gwet_ac2"))
  expect_equal(nrow(result$items), 4)
  expect_equal(result$items$icvi, c(1, 1, 0.8, 0.4))
  expect_equal(result$items$aiken_v[1], (3.8 - 1) / 3)
  expect_equal(result$items$aiken_v[4], (2.2 - 1) / 3)
  # AC1 and AC2 should also be present and finite
  expect_true(all(is.finite(result$items$gwet_ac1)))
  expect_true(all(is.finite(result$items$gwet_ac2)))
})

test_that("content_validity scale vector has all five indices", {
  ratings <- matrix(c(4, 4, 4, 3, 3, 3), nrow = 3)
  result <- content_validity(ratings)

  expect_named(result$scale, c("scvi_ave", "scvi_ua", "mean_kappa",
                               "mean_ac1", "mean_ac2"))
  expect_equal(unname(result$scale["scvi_ave"]), 1)
  expect_equal(unname(result$scale["scvi_ua"]), 1)
  expect_equal(unname(result$scale["mean_kappa"]), 1)
  expect_equal(unname(result$scale["mean_ac1"]), 1)
})


test_that("content_validity accepts custom categories and ac2_weights", {
  # 5-point scale, ratings at 4 and 5 only (relevant=4+)
  ratings <- matrix(c(4, 5, 4, 5, 5,
                      5, 4, 4, 5, 4),
                    nrow = 5,
                    dimnames = list(NULL, c("item1", "item2")))
  # With categories = 1:5 explicit and quadratic weights, AC2 should be
  # higher than with linear weights for these tightly-clustered ratings.
  res_q <- content_validity(ratings, lo = 1, hi = 5,
                            categories = 1:5, ac2_weights = "quadratic")
  res_l <- content_validity(ratings, lo = 1, hi = 5,
                            categories = 1:5, ac2_weights = "linear")
  expect_gte(res_q$items$gwet_ac2[1], res_l$items$gwet_ac2[1])
})

test_that("content_validity generates default item names when columns unnamed", {
  ratings <- matrix(c(4, 4, 4, 3, 3, 3), nrow = 3)
  result <- content_validity(ratings)
  expect_equal(result$items$item, c("item1", "item2"))
})

test_that("content_validity preserves user-supplied item names", {
  ratings <- matrix(
    c(4, 4, 4, 3, 3, 3),
    nrow = 3,
    dimnames = list(NULL, c("clarity", "relevance"))
  )
  result <- content_validity(ratings)
  expect_equal(result$items$item, c("clarity", "relevance"))
})

test_that("content_validity rejects vector input", {
  expect_error(content_validity(c(4, 4, 3, 3)),
               "matrix or data frame with multiple items")
})

test_that("content_validity rejects non-numeric input", {
  expect_error(content_validity("not numeric"), "must be numeric")
})

test_that("content_validity passes parameters through correctly", {
  ratings <- matrix(c(3, 3, 3, 4, 4, 4), nrow = 3)
  # threshold 4: item1 icvi = 0, item2 icvi = 1
  result <- content_validity(ratings, relevant_threshold = 4)
  expect_equal(result$items$icvi, c(0, 1))
})

test_that("content_validity respects custom lo/hi for Aiken's V", {
  ratings <- matrix(c(5, 5, 5, 1, 1, 1), nrow = 3)
  result <- content_validity(ratings, lo = 1, hi = 5)
  expect_equal(result$items$aiken_v, c(1, 0))
})

test_that("content_validity accepts a data frame", {
  df <- data.frame(item1 = c(4, 4, 4), item2 = c(3, 3, 3))
  result <- content_validity(df)
  expect_equal(result$items$item, c("item1", "item2"))
  expect_equal(result$items$icvi, c(1, 1))
})

test_that("print.content_validity produces output and returns invisibly", {
  ratings <- matrix(c(4, 4, 3, 3), nrow = 2)
  result <- content_validity(ratings)
  expect_output(print(result), "Content Validity Analysis")
  expect_output(print(result), "Item-level indices")
  expect_output(print(result), "Scale-level indices")
  invisible_result <- print(result)
  expect_identical(invisible_result, result)
})


# ---------------------------------------------------------------------
# v0.2.0: multi-dimensional / subscale support
# ---------------------------------------------------------------------

test_that("content_validity returns subscale-level indices when subscale supplied", {
  ratings <- matrix(
    c(4, 4, 3, 4, 4,    # item 1: subscale A
      3, 4, 4, 4, 3,    # item 2: subscale A
      2, 3, 3, 4, 3,    # item 3: subscale B
      1, 2, 3, 2, 3),   # item 4: subscale B
    nrow = 5,
    dimnames = list(NULL, paste0("item", 1:4))
  )
  result <- content_validity(ratings, subscale = c("A", "A", "B", "B"))

  expect_true(!is.null(result$subscales))
  expect_named(result$subscales,
               c("subscale", "n_items", "scvi_ave", "scvi_ua",
                 "mean_kappa", "mean_ac1", "mean_ac2"))
  expect_equal(nrow(result$subscales), 2)
  expect_equal(result$subscales$subscale, c("A", "B"))
  expect_equal(result$subscales$n_items, c(2L, 2L))

  # Items data frame should also carry the subscale assignment
  expect_true("subscale" %in% names(result$items))
  expect_equal(result$items$subscale, c("A", "A", "B", "B"))
})

test_that("subscale with a single item returns NA at scale level for that subscale", {
  ratings <- matrix(c(4, 4, 3, 4,    # A
                      3, 4, 4, 4,    # A
                      1, 2, 3, 2),   # B (one item only)
                    nrow = 4)
  result <- content_validity(ratings, subscale = c("A", "A", "B"))
  b_row <- result$subscales[result$subscales$subscale == "B", ]
  expect_equal(b_row$n_items, 1L)
  expect_true(is.na(b_row$scvi_ave))
  expect_true(is.na(b_row$scvi_ua))
})

test_that("subscale argument is validated", {
  # 3 items, but only 2 subscale assignments -- should error
  ratings <- matrix(c(4, 4, 3, 4, 4, 3, 2, 3, 3), nrow = 3)
  expect_error(content_validity(ratings, subscale = c("A", "B")),
               "one entry per item")
})

test_that("default behaviour (no subscale) leaves $subscales NULL", {
  ratings <- matrix(c(4, 4, 3, 4, 4, 3), nrow = 3)
  result <- content_validity(ratings)
  expect_null(result$subscales)
  expect_false("subscale" %in% names(result$items))
})
