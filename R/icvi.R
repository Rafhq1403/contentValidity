#' Item-level Content Validity Index (I-CVI)
#'
#' Computes the Item-level Content Validity Index (I-CVI) for one or more items
#' rated by a panel of experts on a relevance scale. Following Lynn (1986) and
#' Polit & Beck (2006), I-CVI is calculated as the proportion of experts who
#' rate an item as 3 (relevant) or 4 (highly relevant) on a 4-point relevance
#' scale.
#'
#' Optional bootstrap confidence intervals are available via `ci = TRUE`. When
#' requested, the function resamples experts (rows) with replacement and
#' recomputes I-CVI on each replicate. Resampling experts (rather than items)
#' matches the standard inferential frame for inter-rater reliability
#' analyses: experts are the random sample from a population of potential
#' raters, while items are fixed by the study design (Gwet, 2014).
#'
#' @param ratings A numeric matrix or data frame of expert ratings, where rows
#'   represent experts and columns represent items. Values are typically on a
#'   1-4 relevance scale. A numeric vector is also accepted, treated as a
#'   single item.
#' @param relevant_threshold Integer. The minimum rating considered "relevant".
#'   Defaults to 3 (i.e., ratings of 3 or 4 count as relevant on a 4-point
#'   scale).
#' @param na.rm Logical. If `TRUE`, missing ratings are excluded from the
#'   calculation. Defaults to `FALSE`, in which case any `NA` produces `NA`
#'   for the affected item.
#' @param ci Logical. If `TRUE`, returns a data frame with bootstrap
#'   confidence intervals in addition to the point estimate. Defaults to
#'   `FALSE` (returns a numeric vector, identical to the package's pre-0.2.0
#'   behaviour).
#' @param n_boot Integer. Number of bootstrap replicates when `ci = TRUE`.
#'   Defaults to 2000, following Davison and Hinkley (1997, ch. 5), who
#'   recommend at least 1000 replicates for stable percentile intervals, and
#'   Hesterberg (2015), who notes that 1000 is sufficient and 10,000 is ideal
#'   on modern hardware. 2000 balances stability against compute time.
#' @param ci_method Character. One of `"percentile"` (default) or `"bca"`
#'   (bias-corrected and accelerated). Percentile (Efron & Tibshirani, 1993)
#'   respects the `[0, 1]` bounds of I-CVI naturally. BCa (DiCiccio & Efron,
#'   1996) is preferred when the bootstrap distribution is skewed, which is
#'   common for I-CVI values near 1.0.
#' @param conf_level Numeric. Confidence level between 0 and 1. Defaults to
#'   0.95.
#' @param seed Integer or `NULL`. If supplied, passed to [set.seed()] for
#'   reproducible bootstrap samples. Defaults to `NULL`.
#'
#' @return When `ci = FALSE` (default), a named numeric vector of I-CVI
#'   values, one per item (or a single numeric value if `ratings` is a
#'   vector). When `ci = TRUE`, a data frame with one row per item and
#'   columns `item`, `icvi`, `ci_lower`, `ci_upper`, `ci_method`,
#'   `conf_level`, `n_boot`.
#'
#' @details
#' Common interpretation guidelines (Polit & Beck, 2006):
#'
#' - I-CVI >= 0.78: excellent content validity (with 6 or more experts).
#' - I-CVI 0.70-0.78: acceptable, item may need revision.
#' - I-CVI < 0.70: item should be revised or eliminated.
#'
#' With fewer than six experts, Lynn (1986) recommends a stricter cutoff of
#' I-CVI = 1.00 for unanimous agreement.
#'
#' @references
#' Lynn, M. R. (1986). Determination and quantification of content validity.
#' *Nursing Research*, 35(6), 382-385.
#' \doi{10.1097/00006199-198611000-00017}
#'
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
#' # Five experts rating four items on a 1-4 relevance scale
#' ratings <- matrix(
#'   c(4, 4, 3, 4, 4,    # Item 1
#'     3, 4, 4, 4, 3,    # Item 2
#'     2, 3, 3, 4, 3,    # Item 3
#'     1, 2, 3, 2, 3),   # Item 4
#'   nrow = 5,
#'   dimnames = list(NULL, paste0("item", 1:4))
#' )
#' icvi(ratings)
#'
#' # Single item supplied as a vector
#' icvi(c(4, 4, 3, 3, 4))
#'
#' # Stricter threshold (only highest rating counts as relevant)
#' icvi(ratings, relevant_threshold = 4)
#'
#' # With bootstrap confidence intervals (new in v0.2.0)
#' set.seed(1)
#' icvi(ratings, ci = TRUE, n_boot = 1000)
#'
#' # BCa intervals, recommended when I-CVI values cluster near 1.0
#' icvi(ratings, ci = TRUE, ci_method = "bca", n_boot = 1000, seed = 1)
#'
#' @export
icvi <- function(ratings,
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

  # Core point-estimate engine (used both for direct return and for
  # bootstrap replicates).
  icvi_engine <- function(x, relevant_threshold, na.rm) {
    if (is.null(dim(x))) {
      relevant <- x >= relevant_threshold
      return(mean(relevant, na.rm = na.rm))
    }
    apply(x, 2, function(item) {
      relevant <- item >= relevant_threshold
      mean(relevant, na.rm = na.rm)
    })
  }

  # Point estimate (no CI requested) -- preserve v0.1.0 behaviour exactly.
  if (!ci) {
    return(icvi_engine(ratings,
                       relevant_threshold = relevant_threshold,
                       na.rm = na.rm))
  }

  # CI requested. For bootstrap, vector input is treated as a one-column
  # matrix internally so the resampling engine can operate uniformly.
  result <- bootstrap_ci(
    ratings    = ratings,
    index_fn   = icvi_engine,
    n_boot     = n_boot,
    ci_method  = ci_method,
    conf_level = conf_level,
    seed       = seed,
    relevant_threshold = relevant_threshold,
    na.rm      = na.rm
  )

  # Rename the generic `estimate` column to the canonical index name.
  names(result)[names(result) == "estimate"] <- "icvi"
  result
}
