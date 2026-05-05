#' Comprehensive content validity analysis
#'
#' Runs the standard relevance-scale content validity indices on a single
#' ratings matrix and returns a tidy summary. Computes Item-level CVI,
#' modified kappa, and Aiken's V at the item level, and S-CVI/Ave, S-CVI/UA,
#' and the mean modified kappa at the scale level.
#'
#' Lawshe's CVR is not included in this wrapper because it uses a different
#' rating convention (essential / useful but not essential / not necessary).
#' For CVR analyses, use [cvr()] and [cvr_critical()] directly.
#'
#' @param ratings A numeric matrix or data frame of expert ratings (rows =
#'   experts, columns = items) on a relevance scale.
#' @param relevant_threshold Integer. Minimum rating considered "relevant".
#'   Passed to [icvi()], [scvi_ave()], [scvi_ua()], and [mod_kappa()].
#'   Defaults to 3.
#' @param lo,hi Numeric. Minimum and maximum possible rating values on the
#'   scale; passed to [aiken_v()]. Defaults to 1 and 4.
#' @param na.rm Logical. Passed to all underlying functions. Defaults to
#'   `FALSE`.
#'
#' @return An object of class `"content_validity"`: a list containing
#'
#' - `items`: a data frame with one row per item and columns `item`,
#'   `icvi`, `mod_kappa`, and `aiken_v`.
#' - `scale`: a named numeric vector with `scvi_ave`, `scvi_ua`, and
#'   `mean_kappa`.
#' - `n_experts`: integer, number of experts (rows).
#' - `n_items`: integer, number of items (columns).
#'
#' @examples
#' ratings <- matrix(
#'   c(4, 4, 3, 4, 4,
#'     3, 4, 4, 4, 3,
#'     2, 3, 3, 4, 3,
#'     1, 2, 3, 2, 3),
#'   nrow = 5,
#'   dimnames = list(NULL, paste0("item", 1:4))
#' )
#' result <- content_validity(ratings)
#' result
#' result$items
#' result$scale
#'
#' @seealso [icvi()], [scvi_ave()], [scvi_ua()], [mod_kappa()],
#'   [aiken_v()], [cvr()]
#' @export
content_validity <- function(ratings,
                             relevant_threshold = 3,
                             lo = 1,
                             hi = 4,
                             na.rm = FALSE) {

  if (missing(ratings)) {
    stop("`ratings` is required.", call. = FALSE)
  }

  if (is.data.frame(ratings)) {
    ratings <- as.matrix(ratings)
  }

  if (!is.numeric(ratings)) {
    stop("`ratings` must be numeric.", call. = FALSE)
  }

  if (is.null(dim(ratings))) {
    stop("`ratings` must be a matrix or data frame with multiple items.",
         call. = FALSE)
  }

  # Item-level indices
  icvi_vals  <- icvi(ratings, relevant_threshold = relevant_threshold,
                     na.rm = na.rm)
  kappa_vals <- mod_kappa(ratings, relevant_threshold = relevant_threshold,
                          na.rm = na.rm)
  aiken_vals <- aiken_v(ratings, lo = lo, hi = hi, na.rm = na.rm)

  item_names <- colnames(ratings)
  if (is.null(item_names)) {
    item_names <- paste0("item", seq_len(ncol(ratings)))
  }

  items_df <- data.frame(
    item      = item_names,
    icvi      = unname(icvi_vals),
    mod_kappa = unname(kappa_vals),
    aiken_v   = unname(aiken_vals),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Scale-level indices
  scale_vec <- c(
    scvi_ave   = scvi_ave(ratings, relevant_threshold = relevant_threshold,
                          na.rm = na.rm),
    scvi_ua    = scvi_ua(ratings, relevant_threshold = relevant_threshold,
                         na.rm = na.rm),
    mean_kappa = mean(kappa_vals, na.rm = na.rm)
  )

  result <- list(
    items     = items_df,
    scale     = scale_vec,
    n_experts = nrow(ratings),
    n_items   = ncol(ratings)
  )

  class(result) <- "content_validity"
  result
}

#' Print method for content_validity objects
#'
#' @param x A `content_validity` object returned by [content_validity()].
#' @param digits Integer. Number of digits to round numeric output to.
#' @param ... Currently ignored.
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.content_validity <- function(x, digits = 4, ...) {
  cat("Content Validity Analysis\n")
  cat("-------------------------\n")
  cat("Experts: ", x$n_experts, "\n", sep = "")
  cat("Items:   ", x$n_items, "\n\n", sep = "")

  cat("Item-level indices:\n")
  items_print <- x$items
  numeric_cols <- vapply(items_print, is.numeric, logical(1))
  items_print[numeric_cols] <- lapply(items_print[numeric_cols], round,
                                      digits = digits)
  print(items_print, row.names = FALSE)

  cat("\nScale-level indices:\n")
  print(round(x$scale, digits))

  invisible(x)
}
