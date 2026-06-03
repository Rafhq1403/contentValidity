#' Aiken's V coefficient of content validity
#'
#' Computes Aiken's V (Aiken, 1985), an index of content validity that uses
#' the full rating scale rather than dichotomizing responses as in I-CVI.
#' Aiken's V ranges from 0 to 1, where 1 indicates all experts gave the
#' maximum rating and 0 indicates all gave the minimum.
#'
#' Optional bootstrap confidence intervals are available via `ci = TRUE`.
#' Resampling is performed at the expert (row) level, matching the standard
#' inferential frame for inter-rater reliability analyses (Gwet, 2014).
#'
#' @param ratings A numeric matrix or data frame of expert ratings (rows =
#'   experts, columns = items). A numeric vector is also accepted, treated
#'   as a single item.
#' @param lo Numeric. Minimum possible rating on the scale. Default 1.
#' @param hi Numeric. Maximum possible rating on the scale. Default 4.
#' @param na.rm Logical. If `TRUE`, missing ratings are excluded.
#'   Defaults to `FALSE`.
#' @param ci Logical. If `TRUE`, returns a data frame with bootstrap
#'   confidence intervals alongside the point estimate. Defaults to `FALSE`
#'   (returns a numeric vector, identical to the package's pre-0.2.0
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
#' @return When `ci = FALSE` (default), a named numeric vector of V values,
#'   one per item (or a single numeric value if `ratings` is a vector).
#'   When `ci = TRUE`, a data frame with one row per item and columns
#'   `item`, `aiken_v`, `ci_lower`, `ci_upper`, `ci_method`, `conf_level`,
#'   `n_boot`.
#'
#' @details
#' Aiken's V is calculated as:
#'
#' \deqn{V = (\bar{X} - lo) / (hi - lo)}
#'
#' where \eqn{\bar{X}} is the mean expert rating across raters, and `lo` and
#' `hi` are the minimum and maximum possible scale values, respectively.
#'
#' A common cutoff is V >= 0.70 for adequate content validity, though
#' stricter thresholds are sometimes applied depending on panel size and
#' research context. Unlike I-CVI, Aiken's V uses the full rating scale, so
#' a rating of 4 contributes more than a rating of 3 (rather than both being
#' counted equally as "relevant").
#'
#' @references
#' Aiken, L. R. (1985). Three coefficients for analyzing the reliability and
#' validity of ratings. *Educational and Psychological Measurement*, 45(1),
#' 131-142. \doi{10.1177/0013164485451012}
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
#'   nrow = 5,
#'   dimnames = list(NULL, paste0("item", 1:4))
#' )
#' aiken_v(ratings)
#'
#' # 5-point scale
#' aiken_v(c(5, 4, 5, 5, 4), lo = 1, hi = 5)
#'
#' # With bootstrap confidence intervals (new in v0.2.0)
#' aiken_v(ratings, ci = TRUE, n_boot = 1000, seed = 1)
#'
#' @seealso [icvi()]
#' @export
aiken_v <- function(ratings,
                    lo = 1,
                    hi = 4,
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

  if (!is.numeric(lo) || length(lo) != 1) {
    stop("`lo` must be a single number.", call. = FALSE)
  }

  if (!is.numeric(hi) || length(hi) != 1) {
    stop("`hi` must be a single number.", call. = FALSE)
  }

  if (hi <= lo) {
    stop("`hi` must be greater than `lo`.", call. = FALSE)
  }

  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(ci) || length(ci) != 1) {
    stop("`ci` must be TRUE or FALSE.", call. = FALSE)
  }

  ci_method <- match.arg(ci_method)

  # Range check on the supplied data
  if (any(ratings < lo | ratings > hi, na.rm = TRUE)) {
    stop("Some ratings are outside the [lo, hi] range.", call. = FALSE)
  }

  # Core point-estimate engine, used both for direct return and bootstrap.
  aiken_v_engine <- function(x, lo, hi, na.rm) {
    one_item <- function(item) {
      if (na.rm) {
        item <- item[!is.na(item)]
      }
      if (anyNA(item) || length(item) == 0) return(NA_real_)
      (mean(item) - lo) / (hi - lo)
    }
    if (is.null(dim(x))) {
      return(one_item(x))
    }
    apply(x, 2, one_item)
  }

  # Point estimate only -- preserve v0.1.0 behaviour exactly.
  if (!ci) {
    return(aiken_v_engine(ratings, lo = lo, hi = hi, na.rm = na.rm))
  }

  # CI requested.
  result <- bootstrap_ci(
    ratings    = ratings,
    index_fn   = aiken_v_engine,
    n_boot     = n_boot,
    ci_method  = ci_method,
    conf_level = conf_level,
    seed       = seed,
    lo         = lo,
    hi         = hi,
    na.rm      = na.rm
  )

  names(result)[names(result) == "estimate"] <- "aiken_v"
  result
}
