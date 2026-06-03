#' Scale-level Content Validity Index, Average method (S-CVI/Ave)
#'
#' Computes the Scale-level Content Validity Index using the averaging method,
#' defined as the mean of the Item-level Content Validity Indices (I-CVI)
#' across all items in the instrument.
#'
#' Optional bootstrap confidence intervals are available via `ci = TRUE`.
#' Resampling is performed at the expert (row) level, matching the standard
#' inferential frame for inter-rater reliability analyses (Gwet, 2014).
#'
#' @param ratings A numeric matrix or data frame of expert ratings (rows =
#'   experts, columns = items) on a relevance scale.
#' @param relevant_threshold Integer. Minimum rating considered "relevant".
#'   Defaults to 3.
#' @param na.rm Logical. Passed through to [icvi()]. Defaults to `FALSE`.
#' @param ci Logical. If `TRUE`, returns a data frame with a bootstrap
#'   confidence interval alongside the point estimate. Defaults to `FALSE`
#'   (returns a single numeric value, identical to the package's pre-0.2.0
#'   behaviour).
#' @param n_boot Integer. Number of bootstrap replicates when `ci = TRUE`.
#'   Defaults to 2000 (Davison & Hinkley, 1997; Hesterberg, 2015).
#' @param ci_method Character. One of `"percentile"` (default; Efron &
#'   Tibshirani, 1993) or `"bca"` (bias-corrected and accelerated;
#'   DiCiccio & Efron, 1996).
#' @param conf_level Numeric. Confidence level between 0 and 1. Defaults to
#'   0.95.
#' @param seed Integer or `NULL`. If supplied, passed to [set.seed()] for
#'   reproducible bootstrap samples. Defaults to `NULL`.
#'
#' @return When `ci = FALSE` (default), a single numeric value: the average
#'   I-CVI across items. When `ci = TRUE`, a one-row data frame with columns
#'   `item` (set to `"scale"`), `scvi_ave`, `ci_lower`, `ci_upper`,
#'   `ci_method`, `conf_level`, `n_boot`.
#'
#' @details
#' S-CVI/Ave >= 0.90 is generally considered excellent content validity at the
#' scale level (Polit & Beck, 2006). Note that S-CVI is undefined for a single
#' item; supply a matrix or data frame with two or more item columns.
#'
#' @references
#' Polit, D. F., & Beck, C. T. (2006). The content validity index: Are you
#' sure you know what's being reported? Critique and recommendations.
#' *Research in Nursing & Health*, 29(5), 489-497.
#' \doi{10.1002/nur.20147}
#'
#' Davison, A. C., & Hinkley, D. V. (1997). *Bootstrap methods and their
#' application*. Cambridge University Press. \doi{10.1017/CBO9780511802843}
#'
#' DiCiccio, T. J., & Efron, B. (1996). Bootstrap confidence intervals.
#' *Statistical Science*, 11(3), 189-228. \doi{10.1214/ss/1032280214}
#'
#' Efron, B., & Tibshirani, R. J. (1993). *An introduction to the bootstrap*.
#' Chapman and Hall. \doi{10.1201/9780429246593}
#'
#' Gwet, K. L. (2014). *Handbook of inter-rater reliability* (4th ed.).
#' Advanced Analytics, LLC.
#'
#' Hesterberg, T. C. (2015). What teachers should know about the bootstrap:
#' Resampling in the undergraduate statistics curriculum. *The American
#' Statistician*, 69(4), 371-386. \doi{10.1080/00031305.2015.1089789}
#'
#' @examples
#' ratings <- matrix(
#'   c(4, 4, 3, 4, 4,
#'     3, 4, 4, 4, 3,
#'     2, 3, 3, 4, 3,
#'     1, 2, 3, 2, 3),
#'   nrow = 5
#' )
#' scvi_ave(ratings)
#'
#' # With bootstrap confidence interval (new in v0.2.0)
#' scvi_ave(ratings, ci = TRUE, n_boot = 1000, seed = 1)
#'
#' @seealso [icvi()]
#' @export
scvi_ave <- function(ratings,
                     relevant_threshold = 3,
                     na.rm = FALSE,
                     ci = FALSE,
                     n_boot = 2000,
                     ci_method = c("percentile", "bca"),
                     conf_level = 0.95,
                     seed = NULL) {

  if (missing(ratings)) {
    stop("`ratings` is required.", call. = FALSE)
  }

  if (is.data.frame(ratings)) {
    ratings <- as.matrix(ratings)
  }

  if (!is.numeric(ratings)) {
    stop("`ratings` must be numeric.", call. = FALSE)
  }

  # Reject vector input -- S-CVI only makes sense for a scale (multiple items)
  if (is.null(dim(ratings))) {
    stop("`ratings` must be a matrix or data frame with multiple items. ",
         "S-CVI is undefined for a single item.", call. = FALSE)
  }

  if (!is.numeric(relevant_threshold) || length(relevant_threshold) != 1) {
    stop("`relevant_threshold` must be a single number.", call. = FALSE)
  }

  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(ci) || length(ci) != 1) {
    stop("`ci` must be TRUE or FALSE.", call. = FALSE)
  }

  ci_method <- match.arg(ci_method)

  # Scale-level engine: per-item I-CVIs averaged across items.
  scvi_ave_engine <- function(x, relevant_threshold, na.rm) {
    item_cvis <- icvi(x,
                      relevant_threshold = relevant_threshold,
                      na.rm = na.rm)
    mean(item_cvis, na.rm = na.rm)
  }

  # Point estimate only -- preserve v0.1.0 behaviour exactly.
  if (!ci) {
    return(scvi_ave_engine(ratings,
                           relevant_threshold = relevant_threshold,
                           na.rm = na.rm))
  }

  # CI requested.
  result <- bootstrap_ci(
    ratings    = ratings,
    index_fn   = scvi_ave_engine,
    n_boot     = n_boot,
    ci_method  = ci_method,
    conf_level = conf_level,
    seed       = seed,
    relevant_threshold = relevant_threshold,
    na.rm      = na.rm
  )

  names(result)[names(result) == "estimate"] <- "scvi_ave"
  result
}
