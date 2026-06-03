#' Lawshe's Content Validity Ratio (CVR)
#'
#' Computes Lawshe's (1975) Content Validity Ratio for one or more items
#' rated by an expert panel. Each expert classifies an item as "essential",
#' "useful but not essential", or "not necessary"; CVR captures the
#' proportion of experts endorsing "essential" relative to chance.
#'
#' @param ratings A numeric matrix or data frame of expert ratings (rows =
#'   experts, columns = items). A numeric vector is also accepted, treated
#'   as a single item.
#' @param essential Numeric vector. Rating value(s) that indicate an expert
#'   classified the item as "essential". Defaults to `1`, matching Lawshe's
#'   (1975) original 3-point scale where 1 = essential, 2 = useful but not
#'   essential, 3 = not necessary. Pass a vector if multiple values count
#'   as essential.
#' @param na.rm Logical. If `TRUE`, missing ratings are excluded when
#'   counting experts. Defaults to `FALSE`.
#'
#' @return A named numeric vector of CVR values per item, ranging from -1
#'   to +1. If `ratings` is a vector, returns a single numeric value.
#'
#' @details
#' The formula is:
#'
#' \deqn{CVR = (n_e - N/2) / (N/2)}
#'
#' where \eqn{n_e} is the number of experts rating the item as essential
#' and N is the total number of experts.
#'
#' Use [cvr_critical()] to obtain the minimum CVR considered statistically
#' significant for a given panel size, following the corrected critical
#' values of Wilson, Pan, and Schumsky (2012).
#'
#' @references
#' Lawshe, C. H. (1975). A quantitative approach to content validity.
#' *Personnel Psychology*, 28(4), 563-575.
#' \doi{10.1111/j.1744-6570.1975.tb01393.x}
#'
#' Wilson, F. R., Pan, W., & Schumsky, D. A. (2012). Recalculation of the
#' critical values for Lawshe's content validity ratio. *Measurement and
#' Evaluation in Counseling and Development*, 45(3), 197-210.
#' \doi{10.1177/0748175612440286}
#'
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
#' @examples
#' # 10 experts rating 3 items on Lawshe's 3-point scale
#' # (1 = essential, 2 = useful, 3 = not necessary)
#' ratings <- matrix(
#'   c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2,    # 8 of 10 essential
#'     1, 1, 1, 2, 2, 2, 2, 3, 3, 3,    # 3 of 10 essential
#'     1, 1, 1, 1, 1, 1, 1, 1, 1, 1),   # 10 of 10 essential
#'   nrow = 10,
#'   dimnames = list(NULL, paste0("item", 1:3))
#' )
#' cvr(ratings)
#'
#' # Compare to the critical value for N = 10
#' cvr_critical(10)
#'
#' # With bootstrap confidence intervals
#' cvr(ratings, ci = TRUE, n_boot = 1000, seed = 1)
#'
#' @seealso [cvr_critical()]
#' @export
cvr <- function(ratings,
                essential = 1,
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

  if (!is.numeric(essential) || length(essential) < 1) {
    stop("`essential` must be one or more numeric values.", call. = FALSE)
  }

  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(ci) || length(ci) != 1) {
    stop("`ci` must be TRUE or FALSE.", call. = FALSE)
  }

  ci_method <- match.arg(ci_method)

  # Core point-estimate engine.
  cvr_engine <- function(x, essential, na.rm) {
    one_item <- function(item) {
      if (na.rm) {
        item <- item[!is.na(item)]
      }
      if (anyNA(item) || length(item) == 0) return(NA_real_)
      N <- length(item)
      n_e <- sum(item %in% essential)
      (n_e - N / 2) / (N / 2)
    }
    if (is.null(dim(x))) {
      return(one_item(x))
    }
    apply(x, 2, one_item)
  }

  # Point estimate only -- preserve v0.1.0 behaviour exactly.
  if (!ci) {
    return(cvr_engine(ratings, essential = essential, na.rm = na.rm))
  }

  # CI requested.
  result <- bootstrap_ci(
    ratings    = ratings,
    index_fn   = cvr_engine,
    n_boot     = n_boot,
    ci_method  = ci_method,
    conf_level = conf_level,
    seed       = seed,
    essential  = essential,
    na.rm      = na.rm
  )

  names(result)[names(result) == "estimate"] <- "cvr"
  result
}


#' Critical CVR value for a given panel size
#'
#' Returns the minimum Content Validity Ratio considered statistically
#' significant for a panel of N experts at the specified alpha level. The
#' calculation uses the exact binomial distribution under the null
#' hypothesis that each expert independently rates "essential" with
#' probability 0.5, following the corrected approach of Wilson, Pan, and
#' Schumsky (2012).
#'
#' @param n_experts Positive integer. Number of experts on the panel.
#' @param alpha Numeric. One-tailed significance level. Defaults to 0.05.
#'
#' @return Numeric. The critical CVR value. CVR values at or above this
#'   threshold are statistically significant. Returns `NA_real_` if no CVR
#'   value can reach significance at the specified alpha (which can happen
#'   for very small panels with stringent alpha).
#'
#' @details
#' The critical value is determined as the smallest \eqn{k} such that
#' \eqn{P(X \geq k) \leq \alpha} when \eqn{X \sim Binomial(N, 0.5)}, then
#' transformed to the CVR scale via \eqn{CVR_{crit} = (k - N/2) / (N/2)}.
#'
#' Wilson, Pan, and Schumsky (2012) demonstrated that Lawshe's (1975)
#' original critical-value table contained errors, especially for small
#' panels. The exact binomial computation used here is their recommended
#' replacement.
#'
#' @references
#' Wilson, F. R., Pan, W., & Schumsky, D. A. (2012). Recalculation of the
#' critical values for Lawshe's content validity ratio. *Measurement and
#' Evaluation in Counseling and Development*, 45(3), 197-210.
#' \doi{10.1177/0748175612440286}
#'
#' @examples
#' cvr_critical(10)         # 0.80 -- need 9 of 10 experts to call it essential
#' cvr_critical(20)         # 0.50
#' cvr_critical(40)         # 0.25
#' cvr_critical(10, alpha = 0.01)
#'
#' @seealso [cvr()]
#' @export
cvr_critical <- function(n_experts, alpha = 0.05) {

  if (!is.numeric(n_experts) || length(n_experts) != 1 || n_experts < 1) {
    stop("`n_experts` must be a single positive integer.", call. = FALSE)
  }

  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }

  n_experts <- as.integer(n_experts)
  k_vals <- 0:n_experts
  upper_tail <- pbinom(k_vals - 1L, n_experts, 0.5, lower.tail = FALSE)
  valid_k <- k_vals[upper_tail <= alpha]

  if (length(valid_k) == 0) {
    return(NA_real_)
  }

  k_crit <- min(valid_k)
  (k_crit - n_experts / 2) / (n_experts / 2)
}
