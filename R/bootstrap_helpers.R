#' Bootstrap confidence intervals for content validity indices
#'
#' Internal helper that computes bootstrap confidence intervals for any
#' content validity index function. Resamples experts (rows) with
#' replacement and recomputes the index on each bootstrap sample.
#'
#' The resampling unit is the expert (row), not the item (column),
#' following the standard inferential frame for inter-rater reliability
#' analyses: experts are the random sample from a population of potential
#' raters, while items are fixed by the study design (Gwet, 2014).
#'
#' Two confidence interval methods are supported:
#'
#' - `"percentile"` (default): the empirical 2.5th and 97.5th percentiles of
#'   the bootstrap distribution (Efron & Tibshirani, 1993). Respects the
#'   `[0, 1]` bounds of proportion-based indices naturally.
#' - `"bca"`: bias-corrected and accelerated intervals (DiCiccio & Efron,
#'   1996). Preferred for skewed distributions, which is common for I-CVI
#'   values close to 1.0.
#'
#' The default number of bootstrap replicates (`n_boot = 2000`) follows
#' Davison and Hinkley (1997, ch. 5) who recommend at least 1000 replicates
#' for percentile intervals; Hesterberg (2015) notes that 1000 is sufficient
#' for moderate accuracy and 10,000 is ideal on modern hardware. 2000 is a
#' balanced default.
#'
#' @param ratings Numeric matrix or vector of expert ratings.
#' @param index_fn A function that computes the index from a ratings matrix
#'   or vector. Must accept `ratings` as its first argument and return either
#'   a numeric scalar (vector input) or named numeric vector (matrix input).
#' @param n_boot Integer. Number of bootstrap replicates. Defaults to 2000.
#' @param ci_method Character. One of `"percentile"` (default) or `"bca"`.
#' @param conf_level Numeric. Confidence level, between 0 and 1. Defaults
#'   to 0.95.
#' @param seed Integer or `NULL`. If supplied, passed to [set.seed()] for
#'   reproducibility. Defaults to `NULL` (no seeding).
#' @param ... Additional arguments passed to `index_fn` on each bootstrap
#'   replicate.
#'
#' @return A data frame with one row per item, columns:
#'
#' - `item`: item name (or `"item"` for vector input).
#' - `<index>`: the point estimate (column name set by the calling function).
#' - `ci_lower`, `ci_upper`: bootstrap CI bounds.
#' - `ci_method`: `"percentile"` or `"bca"`.
#' - `conf_level`: confidence level used.
#' - `n_boot`: number of bootstrap replicates.
#'
#' @references
#' Davison, A. C., & Hinkley, D. V. (1997). *Bootstrap methods and their
#' application*. Cambridge University Press. \doi{10.1017/CBO9780511802843}
#'
#' DiCiccio, T. J., & Efron, B. (1996). Bootstrap confidence intervals.
#' *Statistical Science*, 11(3), 189-228.
#' \doi{10.1214/ss/1032280214}
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
#' @keywords internal
#' @noRd
bootstrap_ci <- function(ratings,
                         index_fn,
                         n_boot = 2000,
                         ci_method = c("percentile", "bca"),
                         conf_level = 0.95,
                         seed = NULL,
                         ...) {

  ci_method <- match.arg(ci_method)

  if (!is.numeric(n_boot) || length(n_boot) != 1 || n_boot < 100) {
    stop("`n_boot` must be a single integer >= 100.", call. = FALSE)
  }

  if (!is.numeric(conf_level) || length(conf_level) != 1 ||
      conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single number between 0 and 1.",
         call. = FALSE)
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("`seed` must be a single integer or NULL.", call. = FALSE)
    }
    # Save the user's current RNG state so we leave the global environment
    # unchanged when this function returns (CRAN good practice).
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
      get(".Random.seed", envir = .GlobalEnv)
    } else {
      NULL
    }
    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(as.integer(seed))
  }

  # Detect vector vs matrix input and standardise to matrix for resampling
  is_vector_input <- is.null(dim(ratings))
  if (is_vector_input) {
    mat <- matrix(ratings, ncol = 1,
                  dimnames = list(NULL, "item"))
  } else {
    mat <- ratings
    if (is.null(colnames(mat))) {
      colnames(mat) <- paste0("item", seq_len(ncol(mat)))
    }
  }

  n_experts <- nrow(mat)
  n_items <- ncol(mat)

  # Point estimate on the observed sample. The number of output values
  # (`n_out`) drives the bootstrap matrix shape -- this lets item-level
  # indices (one value per column) and scale-level indices (a single
  # scalar for the whole scale) share the same engine.
  point <- index_fn(mat, ...)
  n_out <- length(point)

  # Assign default names where missing
  if (is.null(names(point))) {
    if (n_out == n_items) {
      names(point) <- colnames(mat)
    } else if (n_out == 1) {
      names(point) <- "scale"
    } else {
      names(point) <- paste0("out", seq_len(n_out))
    }
  }

  # Bootstrap distribution: resample experts (rows) with replacement,
  # recompute the index on each replicate.
  boot_mat <- matrix(NA_real_, nrow = n_boot, ncol = n_out)
  colnames(boot_mat) <- names(point)

  for (b in seq_len(n_boot)) {
    idx <- sample.int(n_experts, n_experts, replace = TRUE)
    boot_mat[b, ] <- index_fn(mat[idx, , drop = FALSE], ...)
  }

  # Compute confidence intervals per item
  alpha <- 1 - conf_level
  probs <- c(alpha / 2, 1 - alpha / 2)

  if (ci_method == "percentile") {
    ci <- apply(boot_mat, 2, stats::quantile,
                probs = probs, na.rm = TRUE, names = FALSE)
  } else {
    # BCa: bias-corrected and accelerated. For scale-level indices
    # (single output) the jackknife runs once; for item-level it runs
    # for each item separately.
    ci <- vapply(seq_len(n_out), function(j) {
      bca_ci(boot_replicates = boot_mat[, j],
             point_estimate  = point[j],
             ratings_mat     = mat,
             index_fn        = index_fn,
             item_idx        = j,
             probs           = probs,
             ...)
    }, FUN.VALUE = numeric(2))
  }

  # Coerce the BCa or percentile output to a 2 x n_out matrix so the
  # data-frame construction works uniformly for n_out == 1 and n_out > 1.
  if (is.null(dim(ci))) {
    ci <- matrix(ci, nrow = 2)
  }

  out <- data.frame(
    item       = names(point),
    estimate   = unname(point),
    ci_lower   = ci[1, ],
    ci_upper   = ci[2, ],
    ci_method  = ci_method,
    conf_level = conf_level,
    n_boot     = as.integer(n_boot),
    stringsAsFactors = FALSE
  )

  rownames(out) <- NULL
  out
}


#' BCa confidence interval for one item
#'
#' Internal helper implementing the bias-corrected and accelerated
#' bootstrap interval of DiCiccio and Efron (1996), with jackknife-based
#' acceleration. Called from `bootstrap_ci()` when `ci_method = "bca"`.
#'
#' @param boot_replicates Numeric vector of bootstrap replicate values for
#'   one item.
#' @param point_estimate The point estimate on the observed sample.
#' @param ratings_mat Original ratings matrix (full sample).
#' @param index_fn Index function.
#' @param item_idx Column index of the item in `ratings_mat`.
#' @param probs Lower and upper probabilities for the CI (e.g. c(0.025, 0.975)).
#' @param ... Passed to `index_fn`.
#'
#' @return Numeric vector of length 2 with lower and upper CI bounds.
#'
#' @keywords internal
#' @noRd
bca_ci <- function(boot_replicates, point_estimate, ratings_mat,
                   index_fn, item_idx, probs, ...) {

  finite_boots <- boot_replicates[is.finite(boot_replicates)]
  if (length(finite_boots) < 10) {
    return(c(NA_real_, NA_real_))
  }

  # Bias correction (z0): proportion of replicates below the point estimate
  prop_below <- mean(finite_boots < point_estimate, na.rm = TRUE)
  if (prop_below == 0 || prop_below == 1) {
    # Degenerate -- fall back to percentile
    return(stats::quantile(finite_boots, probs = probs,
                           na.rm = TRUE, names = FALSE))
  }
  z0 <- stats::qnorm(prop_below)

  # Acceleration (a): from jackknife on the original sample
  n <- nrow(ratings_mat)
  jack <- numeric(n)
  for (i in seq_len(n)) {
    jack_vals <- index_fn(ratings_mat[-i, , drop = FALSE], ...)
    jack[i] <- jack_vals[item_idx]
  }
  jack_mean <- mean(jack, na.rm = TRUE)
  num <- sum((jack_mean - jack)^3, na.rm = TRUE)
  den <- 6 * (sum((jack_mean - jack)^2, na.rm = TRUE))^1.5
  a <- if (den == 0) 0 else num / den

  z_alpha <- stats::qnorm(probs)
  adj <- z0 + (z0 + z_alpha) / (1 - a * (z0 + z_alpha))
  adj_probs <- stats::pnorm(adj)

  stats::quantile(finite_boots, probs = adj_probs,
                  na.rm = TRUE, names = FALSE)
}
