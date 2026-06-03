#' Modified kappa - I-CVI adjusted for chance agreement
#'
#' Computes modified kappa for each item, as proposed by Polit, Beck,
#' and Owen (2007). Modified kappa adjusts the Item-level Content Validity
#' Index (I-CVI) for chance agreement under the assumption that each expert
#' independently rates an item as relevant with probability 0.5.
#'
#' Optional bootstrap confidence intervals are available via `ci = TRUE`.
#' Resampling is performed at the expert (row) level, matching the standard
#' inferential frame for inter-rater reliability analyses (Gwet, 2014).
#'
#' @param ratings A numeric matrix or data frame of expert ratings (rows =
#'   experts, columns = items). A numeric vector is also accepted, treated
#'   as a single item.
#' @param relevant_threshold Integer. Minimum rating considered "relevant".
#'   Defaults to 3.
#' @param na.rm Logical. If `TRUE`, missing ratings are excluded when
#'   counting experts and agreements. Defaults to `FALSE`.
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
#' @return When `ci = FALSE` (default), a named numeric vector of
#'   modified-kappa values, one per item (or a single numeric value if
#'   `ratings` is a vector). When `ci = TRUE`, a data frame with one row
#'   per item and columns `item`, `mod_kappa`, `ci_lower`, `ci_upper`,
#'   `ci_method`, `conf_level`, `n_boot`.
#'
#' @details
#' The formula is:
#'
#' \deqn{\kappa^* = (\mathrm{I\text{-}CVI} - P_c) / (1 - P_c)}
#'
#' where the chance agreement probability is
#'
#' \deqn{P_c = \binom{N}{A} \times 0.5^N}
#'
#' with N = number of experts and A = number of experts rating the item as
#' relevant.
#'
#' Common interpretation cutoffs (Cicchetti and Sparrow, 1981; adopted by
#' Polit et al., 2007):
#'
#' - kappa* < 0.40: poor
#' - kappa* 0.40-0.59: fair
#' - kappa* 0.60-0.74: good
#' - kappa* > 0.74: excellent
#'
#' @references
#' Cicchetti, D. V., & Sparrow, S. A. (1981). Developing criteria for
#' establishing interrater reliability of specific items: Applications to
#' assessment of adaptive behavior. *American Journal of Mental Deficiency*,
#' 86(2), 127-137.
#'
#' Polit, D. F., Beck, C. T., & Owen, S. V. (2007). Is the CVI an acceptable
#' indicator of content validity? Appraisal and recommendations.
#' *Research in Nursing & Health*, 30(4), 459-467.
#' \doi{10.1002/nur.20199}
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
#' mod_kappa(ratings)
#'
#' # With bootstrap confidence intervals (new in v0.2.0)
#' mod_kappa(ratings, ci = TRUE, n_boot = 1000, seed = 1)
#'
#' @seealso [icvi()]
#' @export
mod_kappa <- function(ratings,
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

  # Core point-estimate engine, used both for direct return and bootstrap
  # replicates. Computes modified kappa for a single item, or for each
  # column of a matrix.
  mod_kappa_engine <- function(x, relevant_threshold, na.rm) {
    one_item <- function(item) {
      if (na.rm) {
        item <- item[!is.na(item)]
      }
      if (anyNA(item)) return(NA_real_)
      N <- length(item)
      if (N == 0) return(NA_real_)
      A <- sum(item >= relevant_threshold)
      icvi_val <- A / N
      Pc <- choose(N, A) * 0.5^N
      if (Pc >= 1) return(NA_real_)
      (icvi_val - Pc) / (1 - Pc)
    }

    if (is.null(dim(x))) {
      return(one_item(x))
    }
    apply(x, 2, one_item)
  }

  # Point estimate (no CI requested) -- preserve v0.1.0 behaviour exactly.
  if (!ci) {
    return(mod_kappa_engine(ratings,
                            relevant_threshold = relevant_threshold,
                            na.rm = na.rm))
  }

  # CI requested.
  result <- bootstrap_ci(
    ratings    = ratings,
    index_fn   = mod_kappa_engine,
    n_boot     = n_boot,
    ci_method  = ci_method,
    conf_level = conf_level,
    seed       = seed,
    relevant_threshold = relevant_threshold,
    na.rm      = na.rm
  )

  names(result)[names(result) == "estimate"] <- "mod_kappa"
  result
}
