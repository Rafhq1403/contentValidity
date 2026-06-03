#' Sample-size planning for content-validity studies
#'
#' Computes the minimum number of expert raters required to estimate an
#' Item-level Content Validity Index (I-CVI) within a specified
#' confidence-interval half-width at a chosen confidence level. Two
#' methods are supported:
#'
#' - `"wald"` (default): the closed-form normal approximation. Fast and
#'   widely used in introductory sample-size formulas. Slightly
#'   anti-conservative for I-CVI values near 0 or 1.
#' - `"wilson"`: the Wilson score interval (Wilson, 1927), solved
#'   numerically via [stats::uniroot()]. More accurate for proportions
#'   near 0 or 1, which is the common case in content-validity work
#'   where I-CVI is typically high (e.g., 0.80--0.95). Recommended by
#'   Newcombe (1998) and Agresti & Coull (1998) for proportion CIs in
#'   small-to-moderate samples.
#'
#' The result fills a documented gap in the content-validity literature.
#' Lynn (1986) and Polit & Beck (2006) provide rule-of-thumb
#' recommendations (typically 5--10 experts) without statistical
#' justification; this function gives a precision-based answer suitable
#' for justification in study protocols and grant applications.
#'
#' @param expected Numeric in `[0, 1]`. Anticipated I-CVI value. Common
#'   values are 0.80--0.95 for items that pass review.
#' @param half_width Numeric in `(0, 1)`. Desired half-width of the
#'   confidence interval. Smaller half-widths require more experts.
#'   Typical choices are 0.05--0.15.
#' @param conf_level Numeric in `(0, 1)`. Confidence level. Default 0.95.
#' @param method One of `"wald"` (default) or `"wilson"`.
#' @param max_n Upper bound on the bisection search for the Wilson
#'   method. Defaults to 1000. If the required sample size exceeds this,
#'   the function returns `NA` with a warning.
#'
#' @return An integer: the minimum number of experts required.
#'
#' @details
#'
#' **Wald formula:**
#' \deqn{n = \lceil z^2 \pi (1 - \pi) / w^2 \rceil}
#'
#' where \eqn{z = \Phi^{-1}(1 - \alpha/2)}, \eqn{\pi} is the expected
#' I-CVI, and \eqn{w} is the target half-width.
#'
#' **Wilson formula:**
#' The Wilson score interval has half-width:
#' \deqn{w(n) = z \sqrt{\pi (1 - \pi) / n + z^2 / (4 n^2)} / (1 + z^2 / n)}
#'
#' which is decreasing in n. The function uses [stats::uniroot()] to
#' find the smallest n such that \eqn{w(n) \le w_{target}}.
#'
#' At \eqn{\pi = 0.85}, \eqn{w = 0.10}, \eqn{1 - \alpha = 0.95}:
#'
#' - Wald gives n = ceiling(1.96^2 * 0.85 * 0.15 / 0.10^2) = 49
#' - Wilson gives n = 49 (essentially identical in the central range)
#'
#' At \eqn{\pi = 0.95}, \eqn{w = 0.05}:
#'
#' - Wald gives n = 73
#' - Wilson gives n = 83 (more conservative near the boundary)
#'
#' For typical content-validity targets (e.g., expected I-CVI 0.85,
#' half-width 0.15), both methods recommend roughly 19--22 experts,
#' well above Lynn's (1986) rule-of-thumb minimum of 6 -- a useful
#' caveat to flag in study design and grant applications.
#'
#' @references
#' Agresti, A., & Coull, B. A. (1998). Approximate is better than
#' "exact" for interval estimation of binomial proportions. *The
#' American Statistician*, 52(2), 119-126.
#' \doi{10.1080/00031305.1998.10480550}
#'
#' Lynn, M. R. (1986). Determination and quantification of content
#' validity. *Nursing Research*, 35(6), 382-385.
#' \doi{10.1097/00006199-198611000-00017}
#'
#' Newcombe, R. G. (1998). Two-sided confidence intervals for the
#' single proportion: Comparison of seven methods. *Statistics in
#' Medicine*, 17(8), 857-872.
#' \doi{10.1002/(SICI)1097-0258(19980430)17:8<857::AID-SIM777>3.0.CO;2-E}
#'
#' Polit, D. F., & Beck, C. T. (2006). The content validity index: Are
#' you sure you know what's being reported? Critique and
#' recommendations. *Research in Nursing & Health*, 29(5), 489-497.
#' \doi{10.1002/nur.20147}
#'
#' Wilson, E. B. (1927). Probable inference, the law of succession, and
#' statistical inference. *Journal of the American Statistical
#' Association*, 22(158), 209-212.
#' \doi{10.1080/01621459.1927.10502953}
#'
#' @examples
#' # Common scenario: anticipated I-CVI = 0.85, want half-width <= 0.10
#' cv_sample_size_icvi(expected = 0.85, half_width = 0.10)
#'
#' # More precision (half-width <= 0.05) needs more experts
#' cv_sample_size_icvi(expected = 0.85, half_width = 0.05)
#'
#' # Wilson method is more accurate near the upper bound
#' cv_sample_size_icvi(expected = 0.95, half_width = 0.05,
#'                     method = "wilson")
#'
#' # Sensitivity table over a range of expected I-CVIs
#' sapply(seq(0.70, 0.95, by = 0.05), function(p) {
#'   cv_sample_size_icvi(expected = p, half_width = 0.10)
#' })
#'
#' @seealso [icvi()]
#' @export
cv_sample_size_icvi <- function(expected,
                                half_width,
                                conf_level = 0.95,
                                method = c("wald", "wilson"),
                                max_n = 1000) {

  method <- match.arg(method)

  if (!is.numeric(expected) || length(expected) != 1 ||
      expected <= 0 || expected >= 1) {
    stop("`expected` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }

  if (!is.numeric(half_width) || length(half_width) != 1 ||
      half_width <= 0 || half_width >= 1) {
    stop("`half_width` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }

  if (!is.numeric(conf_level) || length(conf_level) != 1 ||
      conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }

  if (!is.numeric(max_n) || length(max_n) != 1 || max_n < 2) {
    stop("`max_n` must be a single integer >= 2.", call. = FALSE)
  }

  alpha <- 1 - conf_level
  z <- stats::qnorm(1 - alpha / 2)
  p <- expected
  w <- half_width

  if (method == "wald") {
    n <- ceiling(z^2 * p * (1 - p) / w^2)
    # Wald requires n >= 1; for very wide CIs and very small p(1-p) the
    # formula can return 0 or 1, both of which are unrealistic. Floor
    # at 2.
    return(as.integer(max(n, 2L)))
  }

  # Wilson method: solve w(n) = w_target via uniroot
  wilson_hw <- function(n) {
    se <- sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
    z * se / (1 + z^2 / n)
  }

  # The Wilson half-width is monotonically decreasing in n. Find the
  # smallest integer n where it falls at or below the target.
  if (wilson_hw(max_n) > w) {
    warning("Required sample size exceeds `max_n = ", max_n,
            "`. Returning NA. Increase `max_n` or relax `half_width`.",
            call. = FALSE)
    return(NA_integer_)
  }

  # Edge case: 2 experts are already precise enough. uniroot() would
  # error out with same-sign endpoints; short-circuit instead.
  if (wilson_hw(2) <= w) {
    return(2L)
  }

  root <- stats::uniroot(function(n) wilson_hw(n) - w,
                         interval = c(2, max_n))$root
  as.integer(ceiling(root))
}
