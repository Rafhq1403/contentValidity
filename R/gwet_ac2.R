#' Gwet's AC2 - weighted chance-corrected agreement for ordinal ratings
#'
#' Computes Gwet's AC2 coefficient (Gwet, 2008, 2014) for ordinal ratings,
#' which generalizes AC1 (see [gwet_ac1()]) to the case where rating
#' categories are ordered and partial agreement between adjacent categories
#' should count. Where AC1 dichotomizes ratings before computing chance-
#' corrected agreement, AC2 preserves the full ordinal information through
#' a weight matrix that assigns higher weights to pairs of ratings that are
#' close together (e.g., a rating of 3 and 4) and lower weights to pairs
#' that are far apart (e.g., 1 and 4).
#'
#' Optional bootstrap confidence intervals are available via `ci = TRUE`.
#' Resampling is performed at the expert (row) level, matching the standard
#' inferential frame for inter-rater reliability analyses (Gwet, 2014).
#'
#' @param ratings A numeric matrix or data frame of expert ratings (rows =
#'   experts, columns = items). A numeric vector is also accepted, treated
#'   as a single item.
#' @param weights One of `"quadratic"` (default), `"linear"`,
#'   `"identity"`, or a custom \eqn{q \times q} numeric weight matrix.
#'   Quadratic weights emphasize closeness between rating categories more
#'   strongly than linear weights. Identity weights reduce AC2 to AC1 on
#'   the raw (non-dichotomized) categories.
#' @param categories Numeric vector of all possible rating values. Strongly
#'   recommended for content-validity work, where some categories may not
#'   appear in a given dataset. If `NULL` (the default), categories are
#'   inferred from the observed ratings, which can silently produce
#'   incorrect AC2 values when extreme categories are unused. See Details.
#' @param na.rm Logical. If `TRUE`, missing ratings are excluded when
#'   counting experts on a per-item basis. Defaults to `FALSE`.
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
#' @return When `ci = FALSE` (default), a named numeric vector of AC2
#'   values, one per item (or a single numeric value if `ratings` is a
#'   vector). When `ci = TRUE`, a data frame with columns `item`,
#'   `gwet_ac2`, `ci_lower`, `ci_upper`, `ci_method`, `conf_level`,
#'   `n_boot`.
#'
#' @details
#' For a single item with N experts whose ratings populate the q-category
#' counts \eqn{n_k} (\eqn{k = 1, \ldots, q}) and weight matrix
#' \eqn{W = (w_{kl})}:
#'
#' \deqn{p_a = \sum_k n_k (n_k^W - 1) / [N (N - 1)]}
#'
#' where \eqn{n_k^W = \sum_l w_{kl} n_l} is the weighted count for category
#' k. Chance agreement uses Gwet's marginal-adjusted null:
#'
#' \deqn{p_e = T_w \sum_k \pi_k (1 - \pi_k)}
#'
#' with \eqn{T_w = \sum_{k,l} w_{kl} / [q (q - 1)]} and
#' \eqn{\pi_k = n_k / N}. The coefficient is
#' \eqn{\mathrm{AC2} = (p_a - p_e) / (1 - p_e)}.
#'
#' This implementation reproduces the formulas used by the \code{irrCAC}
#' package (by Kilem Gwet, the original author of AC1/AC2) so that AC2
#' values from this function are bit-for-bit equivalent to those from
#' \code{gwet.ac1.raw()} from \code{irrCAC} on the same data with the
#' same weight matrix and category list.
#'
#' Quadratic and linear weights are computed as in Gwet (2014):
#'
#' \deqn{w^{quad}_{kl} = 1 - (c_k - c_l)^2 / (c_q - c_1)^2}
#' \deqn{w^{lin}_{kl}  = 1 - |c_k - c_l| / |c_q - c_1|}
#'
#' where \eqn{c_1, \ldots, c_q} are the (sorted) category values.
#'
#' **Important**: the `categories` argument should typically be set
#' explicitly to the full theoretical rating scale (e.g., `categories = 1:4`
#' for a standard relevance scale), not left at `NULL`. If a particular
#' item's ratings happen to use only a subset of categories (e.g., all
#' experts rated 3 or 4), the default category-inference logic will produce
#' a smaller weight matrix and substantially different AC2 values. This
#' caveat matches the documented behavior of \code{gwet.ac1.raw()} from the \code{irrCAC} package.
#'
#' @references
#' Gwet, K. L. (2008). Computing inter-rater reliability and its variance
#' in the presence of high agreement. *British Journal of Mathematical
#' and Statistical Psychology*, 61(1), 29-48.
#' \doi{10.1348/000711006X126600}
#'
#' Gwet, K. L. (2014). *Handbook of inter-rater reliability* (4th ed.).
#' Advanced Analytics, LLC.
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
#' # Standard 4-point relevance scale, 5 experts on 4 items
#' ratings <- matrix(
#'   c(4, 4, 3, 4, 4,
#'     3, 4, 4, 4, 3,
#'     2, 3, 3, 4, 3,
#'     1, 2, 3, 2, 3),
#'   nrow = 5,
#'   dimnames = list(NULL, paste0("item", 1:4))
#' )
#'
#' # Quadratic weights are the default and most common choice for
#' # ordinal data. Pass the full rating scale explicitly.
#' gwet_ac2(ratings, categories = 1:4)
#'
#' # Linear weights are an alternative
#' gwet_ac2(ratings, weights = "linear", categories = 1:4)
#'
#' # With bootstrap confidence intervals
#' gwet_ac2(ratings, categories = 1:4, ci = TRUE,
#'          n_boot = 1000, seed = 1)
#'
#' @seealso [gwet_ac1()], [mod_kappa()]
#' @export
gwet_ac2 <- function(ratings,
                     weights = c("quadratic", "linear", "identity"),
                     categories = NULL,
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

  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(ci) || length(ci) != 1) {
    stop("`ci` must be TRUE or FALSE.", call. = FALSE)
  }

  ci_method <- match.arg(ci_method)

  # Build the weight matrix once, before any bootstrap loop.
  if (is.numeric(weights) && length(dim(weights)) == 2L) {
    weights_mat <- weights
    if (nrow(weights_mat) != ncol(weights_mat)) {
      stop("Custom `weights` matrix must be square.", call. = FALSE)
    }
    if (is.null(categories)) {
      stop("When passing a custom `weights` matrix, `categories` must ",
           "also be supplied so the matrix can be aligned to the data.",
           call. = FALSE)
    }
    if (length(categories) != nrow(weights_mat)) {
      stop("`categories` length must match the dimensions of the ",
           "`weights` matrix.", call. = FALSE)
    }
  } else {
    weights <- match.arg(weights)
    if (is.null(categories)) {
      categories <- sort(unique(stats::na.omit(as.vector(ratings))))
      warning("`categories` not supplied; inferred from observed ",
              "ratings. For content-validity work the full theoretical ",
              "rating scale should usually be passed explicitly ",
              "(e.g., `categories = 1:4`) to avoid silently collapsing ",
              "the weight matrix.", call. = FALSE)
    }
    categories <- sort(unique(categories))
    weights_mat <- gwet_weight_matrix(categories, weights)
  }

  # Core engine, called both for the point estimate and for bootstrap
  # replicates. Follows irrCAC::gwet.ac1.raw() exactly.
  gwet_ac2_engine <- function(x, categories, weights_mat, na.rm) {
    one_item <- function(item) {
      if (na.rm) {
        item <- item[!is.na(item)]
      }
      if (anyNA(item)) return(NA_real_)
      n_total <- length(item)
      if (n_total < 2) return(NA_real_)
      q <- length(categories)
      # Per-category counts n_k for this item
      n_k <- vapply(categories, function(cat) sum(item == cat),
                    FUN.VALUE = integer(1))
      r_i <- sum(n_k)
      if (r_i < 2) return(NA_real_)
      # Weighted category counts (W * n_k')
      n_k_w <- as.numeric(weights_mat %*% n_k)
      # Observed weighted agreement
      sum_q <- sum(n_k * (n_k_w - 1))
      p_a <- sum_q / (r_i * (r_i - 1))
      # Marginal proportions
      pi_k <- n_k / r_i
      # Gwet chance agreement
      if (q < 2) return(NA_real_)
      p_e <- sum(weights_mat) * sum(pi_k * (1 - pi_k)) / (q * (q - 1))
      if (p_e >= 1) return(NA_real_)
      (p_a - p_e) / (1 - p_e)
    }
    if (is.null(dim(x))) {
      return(one_item(x))
    }
    apply(x, 2, one_item)
  }

  # Point estimate only.
  if (!ci) {
    return(gwet_ac2_engine(ratings,
                           categories = categories,
                           weights_mat = weights_mat,
                           na.rm = na.rm))
  }

  # CI requested.
  result <- bootstrap_ci(
    ratings    = ratings,
    index_fn   = gwet_ac2_engine,
    n_boot     = n_boot,
    ci_method  = ci_method,
    conf_level = conf_level,
    seed       = seed,
    categories = categories,
    weights_mat = weights_mat,
    na.rm      = na.rm
  )

  names(result)[names(result) == "estimate"] <- "gwet_ac2"
  result
}


#' Construct a Gwet weight matrix from named categories
#'
#' Internal helper that builds the q x q weight matrix corresponding to
#' the named weighting scheme, exactly matching the conventions used by
#' the `irrCAC` package's `gwet.ac1.raw()` function (Gwet, 2014). The
#' categories are sorted numerically before the weight matrix is built
#' so that the indexing is unambiguous.
#'
#' @param categories Numeric vector of distinct category values.
#' @param weights One of `"quadratic"`, `"linear"`, or `"identity"`.
#'
#' @return A square numeric matrix with `length(categories)` rows and
#'   columns and diagonal entries equal to 1.
#'
#' @keywords internal
#' @noRd
gwet_weight_matrix <- function(categories, weights) {
  q <- length(categories)
  w <- diag(q)
  if (q == 1) return(w)
  cv <- sort(categories)
  xmin <- min(cv); xmax <- max(cv)
  if (weights == "identity") return(w)
  for (k in seq_len(q)) {
    for (l in seq_len(q)) {
      w[k, l] <- switch(
        weights,
        quadratic = 1 - (cv[k] - cv[l])^2 / (xmax - xmin)^2,
        linear    = 1 - abs(cv[k] - cv[l]) / abs(xmax - xmin)
      )
    }
  }
  w
}
