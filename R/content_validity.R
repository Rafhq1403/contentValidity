#' Comprehensive content validity analysis
#'
#' Runs the standard relevance-scale content validity indices on a single
#' ratings matrix and returns a tidy summary. Computes Item-level CVI,
#' modified kappa, Aiken's V, Gwet's AC1, and Gwet's AC2 at the item
#' level; S-CVI/Ave, S-CVI/UA, mean modified kappa, mean AC1, and mean
#' AC2 at the scale level. New AC1 and AC2 columns added in v0.2.0.
#'
#' Lawshe's CVR is not included in this wrapper because it uses a
#' different rating convention (essential / useful but not essential /
#' not necessary). For CVR analyses, use [cvr()] and [cvr_critical()]
#' directly.
#'
#' @param ratings A numeric matrix or data frame of expert ratings (rows =
#'   experts, columns = items) on a relevance scale.
#' @param relevant_threshold Integer. Minimum rating considered "relevant".
#'   Passed to [icvi()], [scvi_ave()], [scvi_ua()], [mod_kappa()], and
#'   [gwet_ac1()]. Defaults to 3.
#' @param lo,hi Numeric. Minimum and maximum possible rating values on the
#'   scale; passed to [aiken_v()]. Defaults to 1 and 4.
#' @param categories Numeric vector of all possible rating values, used by
#'   [gwet_ac2()]. Defaults to `seq(lo, hi)`, which is correct for the
#'   typical 4-point relevance scale.
#' @param ac2_weights Weighting scheme passed to [gwet_ac2()]. One of
#'   `"quadratic"` (default), `"linear"`, `"identity"`, or a custom
#'   square matrix.
#' @param subscale Optional character or factor vector of length
#'   `ncol(ratings)` assigning each item to a subscale (factor / domain).
#'   When supplied, the scale-level indices are computed both overall
#'   and per-subscale, and the result carries a `$subscales` data frame.
#'   Useful for multi-dimensional instruments where different items
#'   measure different constructs. Defaults to `NULL` (overall only).
#' @param na.rm Logical. Passed to all underlying functions. Defaults to
#'   `FALSE`.
#'
#' @return An object of class `"content_validity"`: a list containing
#'
#' - `items`: a data frame with one row per item and columns `item`,
#'   `icvi`, `mod_kappa`, `aiken_v`, `gwet_ac1`, `gwet_ac2`.
#' - `scale`: a named numeric vector with `scvi_ave`, `scvi_ua`,
#'   `mean_kappa`, `mean_ac1`, `mean_ac2`.
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
#'   [aiken_v()], [gwet_ac1()], [gwet_ac2()], [cvr()]
#' @export
content_validity <- function(ratings,
                             relevant_threshold = 3,
                             lo = 1,
                             hi = 4,
                             categories = NULL,
                             ac2_weights = "quadratic",
                             subscale = NULL,
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

  # Default the AC2 categories to the same scale span used for Aiken's V,
  # so AC2 works out-of-the-box on the typical 1-4 relevance scale.
  if (is.null(categories)) {
    categories <- seq(from = lo, to = hi)
  }

  # Item-level indices
  icvi_vals  <- icvi(ratings, relevant_threshold = relevant_threshold,
                     na.rm = na.rm)
  kappa_vals <- mod_kappa(ratings, relevant_threshold = relevant_threshold,
                          na.rm = na.rm)
  aiken_vals <- aiken_v(ratings, lo = lo, hi = hi, na.rm = na.rm)
  ac1_vals   <- gwet_ac1(ratings, relevant_threshold = relevant_threshold,
                         na.rm = na.rm)
  ac2_vals   <- gwet_ac2(ratings, weights = ac2_weights,
                         categories = categories, na.rm = na.rm)

  item_names <- colnames(ratings)
  if (is.null(item_names)) {
    item_names <- paste0("item", seq_len(ncol(ratings)))
  }

  items_df <- data.frame(
    item      = item_names,
    icvi      = unname(icvi_vals),
    mod_kappa = unname(kappa_vals),
    aiken_v   = unname(aiken_vals),
    gwet_ac1  = unname(ac1_vals),
    gwet_ac2  = unname(ac2_vals),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Scale-level indices
  scale_vec <- c(
    scvi_ave   = scvi_ave(ratings, relevant_threshold = relevant_threshold,
                          na.rm = na.rm),
    scvi_ua    = scvi_ua(ratings, relevant_threshold = relevant_threshold,
                         na.rm = na.rm),
    mean_kappa = mean(kappa_vals, na.rm = na.rm),
    mean_ac1   = mean(ac1_vals,   na.rm = na.rm),
    mean_ac2   = mean(ac2_vals,   na.rm = na.rm)
  )

  result <- list(
    items     = items_df,
    scale     = scale_vec,
    n_experts = nrow(ratings),
    n_items   = ncol(ratings)
  )

  # Optional multi-dimensional / subscale analysis. When the user assigns
  # items to subscales, recompute the scale-level indices for each
  # subscale separately and attach the result as a $subscales data frame.
  # Scale-level indices are only meaningful for subscales with at least
  # two items; subscales with one item are reported with NA at the
  # scale level.
  if (!is.null(subscale)) {
    if (length(subscale) != ncol(ratings)) {
      stop("`subscale` must have one entry per item (column of ratings).",
           call. = FALSE)
    }
    subscale <- as.character(subscale)
    unique_subs <- unique(subscale)
    sub_rows <- lapply(unique_subs, function(s) {
      cols <- which(subscale == s)
      sub_mat <- ratings[, cols, drop = FALSE]
      n_sub_items <- ncol(sub_mat)
      if (n_sub_items < 2L) {
        return(data.frame(
          subscale   = s,
          n_items    = n_sub_items,
          scvi_ave   = NA_real_,
          scvi_ua    = NA_real_,
          mean_kappa = NA_real_,
          mean_ac1   = NA_real_,
          mean_ac2   = NA_real_,
          stringsAsFactors = FALSE
        ))
      }
      data.frame(
        subscale   = s,
        n_items    = n_sub_items,
        scvi_ave   = scvi_ave(sub_mat,
                              relevant_threshold = relevant_threshold,
                              na.rm = na.rm),
        scvi_ua    = scvi_ua(sub_mat,
                             relevant_threshold = relevant_threshold,
                             na.rm = na.rm),
        mean_kappa = mean(kappa_vals[cols], na.rm = na.rm),
        mean_ac1   = mean(ac1_vals[cols],   na.rm = na.rm),
        mean_ac2   = mean(ac2_vals[cols],   na.rm = na.rm),
        stringsAsFactors = FALSE
      )
    })
    result$subscales <- do.call(rbind, sub_rows)
    result$items$subscale <- subscale
  }

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
  cat("Items:   ", x$n_items, "\n", sep = "")
  if (!is.null(x$subscales)) {
    cat("Subscales: ", nrow(x$subscales), "\n", sep = "")
  }
  cat("\n")

  cat("Item-level indices:\n")
  items_print <- x$items
  numeric_cols <- vapply(items_print, is.numeric, logical(1))
  items_print[numeric_cols] <- lapply(items_print[numeric_cols], round,
                                      digits = digits)
  print(items_print, row.names = FALSE)

  cat("\nScale-level indices (overall):\n")
  print(round(x$scale, digits))

  if (!is.null(x$subscales)) {
    cat("\nSubscale-level indices:\n")
    sub_print <- x$subscales
    sub_numeric <- vapply(sub_print, is.numeric, logical(1))
    sub_print[sub_numeric] <- lapply(sub_print[sub_numeric], round,
                                     digits = digits)
    print(sub_print, row.names = FALSE)
  }

  invisible(x)
}
