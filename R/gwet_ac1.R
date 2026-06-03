#' Gwet's AC1 - chance-corrected agreement
#'
#' Computes Gwet's AC1 coefficient (Gwet, 2008) for each item rated by an
#' expert panel on a relevance scale. AC1 is a chance-corrected agreement
#' index that uses a marginal-adjusted null model: chance agreement is
#' computed under the assumption that each expert rates "relevant" with
#' probability equal to the observed marginal proportion. This is
#' methodologically distinct from the modified kappa of Polit, Beck, and
#' Owen (2007), which uses a fixed null (each expert independently rates
#' relevant with probability 0.5). The two indices can therefore yield
#' substantively different answers for the same data, particularly when
#' the prevalence of "relevant" ratings is far from 0.5 (the typical case
#' in content-validity work). Reporting both -- alongside I-CVI -- gives a
#' more complete picture of inter-rater agreement than any single index.
#' Wongpakaran et al. (2013, *BMC Medical Research Methodology*)
#' recommended AC1 over Cohen's traditional kappa for high-prevalence
#' rating contexts.
#'
#' Optional bootstrap confidence intervals are available via `ci = TRUE`.
#' Resampling is performed at the expert (row) level, matching the standard
#' inferential frame for inter-rater reliability analyses (Gwet, 2014).
#'
#' @param ratings A numeric matrix or data frame of expert ratings (rows =
#'   experts, columns = items). A numeric vector is also accepted, treated
#'   as a single item.
#' @param relevant_threshold Integer. Minimum rating considered "relevant".
#'   Ratings are dichotomized at this threshold before AC1 is computed,
#'   following standard practice in content-validity work (Polit, Beck, &
#'   Owen, 2007). Defaults to 3.
#' @param na.rm Logical. If `TRUE`, missing ratings are excluded when
#'   counting experts. Defaults to `FALSE`.
#' @param ci Logical. If `TRUE`, returns a data frame with bootstrap
#'   confidence intervals alongside the point estimate. Defaults to `FALSE`.
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
#' @return When `ci = FALSE` (default), a named numeric vector of AC1
#'   values, one per item (or a single numeric value if `ratings` is a
#'   vector). When `ci = TRUE`, a data frame with columns `item`,
#'   `gwet_ac1`, `ci_lower`, `ci_upper`, `ci_method`, `conf_level`,
#'   `n_boot`.
#'
#' @details
#' The formula is:
#'
#' \deqn{\mathrm{AC1} = (p_a - p_e) / (1 - p_e)}
#'
#' For a single item with N experts of whom \eqn{n_R} rate as relevant:
#'
#' \deqn{p_a = [n_R(n_R - 1) + (N - n_R)(N - n_R - 1)] / [N(N - 1)]}
#' \deqn{p_e = 2 \pi (1 - \pi), \quad \pi = n_R / N}
#'
#' This is Gwet's binary-rating form (Gwet, 2008, equation 5). The chance
#' agreement term \eqn{p_e = 2\pi(1-\pi)} is maximised at 0.5 when
#' \eqn{\pi = 0.5} and approaches zero as \eqn{\pi} approaches either
#' extreme.
#'
#' Note that the "kappa paradox" (Feinstein & Cicchetti, 1990) and the
#' Wongpakaran et al. (2013) comparison both refer to *Cohen's* kappa,
#' whose chance-agreement term \eqn{\pi^2 + (1 - \pi)^2} approaches 1 at
#' the prevalence extremes. The modified kappa of Polit et al. (2007),
#' implemented in this package as [mod_kappa()], uses a different
#' chance-correction (\eqn{C(N, A) \times 0.5^N}, a fixed binomial null)
#' and does not behave like Cohen's kappa under high prevalence. The
#' practical consequence is that mod_kappa and AC1 typically diverge
#' when prevalence is far from 0.5 -- modified kappa approaches I-CVI
#' while AC1 discounts more of the observed agreement as
#' prevalence-driven. Both are defensible; they answer different
#' questions about chance.
#'
#' Common interpretation cutoffs follow Altman (1991), as adapted to AC1
#' by Wongpakaran et al. (2013):
#'
#' - AC1 < 0.20: poor
#' - AC1 0.20-0.39: fair
#' - AC1 0.40-0.59: moderate
#' - AC1 0.60-0.80: good
#' - AC1 > 0.80: very good
#'
#' (Boundary values fall in the higher tier, matching the classifier
#' used by [apa_table()] with `interpretation_index = "gwet_ac1"`.)
#'
#' @references
#' Altman, D. G. (1991). *Practical statistics for medical research*.
#' Chapman and Hall.
#'
#' Feinstein, A. R., & Cicchetti, D. V. (1990). High agreement but low
#' kappa: I. The problems of two paradoxes. *Journal of Clinical
#' Epidemiology*, 43(6), 543-549. \doi{10.1016/0895-4356(90)90158-L}
#'
#' Gwet, K. L. (2008). Computing inter-rater reliability and its variance
#' in the presence of high agreement. *British Journal of Mathematical
#' and Statistical Psychology*, 61(1), 29-48.
#' \doi{10.1348/000711006X126600}
#'
#' Gwet, K. L. (2014). *Handbook of inter-rater reliability* (4th ed.).
#' Advanced Analytics, LLC.
#'
#' Polit, D. F., Beck, C. T., & Owen, S. V. (2007). Is the CVI an acceptable
#' indicator of content validity? Appraisal and recommendations.
#' *Research in Nursing & Health*, 30(4), 459-467.
#' \doi{10.1002/nur.20199}
#'
#' Wongpakaran, N., Wongpakaran, T., Wedding, D., & Gwet, K. L. (2013). A
#' comparison of Cohen's Kappa and Gwet's AC1 when calculating inter-rater
#' reliability coefficients: A study conducted with personality disorder
#' samples. *BMC Medical Research Methodology*, 13(1), 61.
#' \doi{10.1186/1471-2288-13-61}
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
#' Hesterberg, T. C. (2015). What teachers should know about the bootstrap:
#' Resampling in the undergraduate statistics curriculum. *The American
#' Statistician*, 69(4), 371-386. \doi{10.1080/00031305.2015.1089789}
#'
#' @examples
#' ratings <- matrix(
#'   c(4, 4, 3, 4, 4,    # 5 of 5 relevant
#'     3, 4, 4, 4, 3,    # 5 of 5 relevant
#'     2, 3, 3, 4, 3,    # 4 of 5 relevant
#'     1, 2, 3, 2, 3),   # 2 of 5 relevant
#'   nrow = 5,
#'   dimnames = list(NULL, paste0("item", 1:4))
#' )
#' gwet_ac1(ratings)
#'
#' # Compare with modified kappa to see Gwet's advantage at extremes
#' mod_kappa(ratings)
#'
#' # With bootstrap confidence intervals
#' gwet_ac1(ratings, ci = TRUE, n_boot = 1000, seed = 1)
#'
#' @seealso [mod_kappa()], [icvi()]
#' @export
gwet_ac1 <- function(ratings,
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

  # Core point-estimate engine. For each item: dichotomise at
  # `relevant_threshold`, compute pairwise observed agreement and Gwet's
  # chance-agreement, then combine via (p_a - p_e) / (1 - p_e).
  gwet_ac1_engine <- function(x, relevant_threshold, na.rm) {
    one_item <- function(item) {
      if (na.rm) {
        item <- item[!is.na(item)]
      }
      if (anyNA(item)) return(NA_real_)
      N <- length(item)
      if (N < 2) return(NA_real_)
      n_R <- sum(item >= relevant_threshold)
      n_I <- N - n_R
      # Observed pairwise agreement (Gwet 2008, eq. 5 binary form)
      p_a <- (n_R * (n_R - 1) + n_I * (n_I - 1)) / (N * (N - 1))
      # Chance agreement under Gwet's null model
      pi_hat <- n_R / N
      p_e <- 2 * pi_hat * (1 - pi_hat)
      if (p_e >= 1) return(NA_real_)
      (p_a - p_e) / (1 - p_e)
    }
    if (is.null(dim(x))) {
      return(one_item(x))
    }
    apply(x, 2, one_item)
  }

  # Point estimate only -- preserve the same return shape as the other
  # item-level indices in the package.
  if (!ci) {
    return(gwet_ac1_engine(ratings,
                           relevant_threshold = relevant_threshold,
                           na.rm = na.rm))
  }

  # CI requested.
  result <- bootstrap_ci(
    ratings    = ratings,
    index_fn   = gwet_ac1_engine,
    n_boot     = n_boot,
    ci_method  = ci_method,
    conf_level = conf_level,
    seed       = seed,
    relevant_threshold = relevant_threshold,
    na.rm      = na.rm
  )

  names(result)[names(result) == "estimate"] <- "gwet_ac1"
  result
}
