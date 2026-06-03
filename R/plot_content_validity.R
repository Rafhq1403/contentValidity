#' Plot a content validity analysis
#'
#' Produces an I-CVI / chance-corrected agreement scatter plot for the
#' item-level results of a [content_validity()] analysis, parallel to the
#' difficulty-discrimination scatter used in classical item analysis.
#' Items that fall outside the conventional adequacy region are flagged
#' in red and labeled by default.
#'
#' @param x A `content_validity` object returned by [content_validity()].
#' @param y Ignored (required by the S3 plot generic).
#' @param y_index Character. Which agreement index to display on the
#'   y-axis. One of `"mod_kappa"` (default), `"gwet_ac1"`, `"gwet_ac2"`,
#'   or `"aiken_v"`.
#' @param label Character. One of `"flagged"` (default, label only items
#'   outside the adequacy region), `"all"`, or `"none"`.
#' @param flag_logic Character. Which axis (or axes) drive the flagging.
#'   One of `"any"` (default; flag items below either threshold, useful
#'   for "items that need any review"), `"icvi"` (flag only items below
#'   the I-CVI threshold), `"y_index"` (flag only items below the
#'   y-axis threshold, useful when the plot is presenting one index
#'   specifically), or `"both"` (strict; flag only items below both
#'   thresholds).
#' @param flag_threshold_icvi Numeric. Lower I-CVI threshold marking the
#'   adequacy region (Polit & Beck, 2006). Defaults to 0.78.
#' @param flag_threshold_y Numeric. Lower threshold on the y-axis index.
#'   Defaults depend on `y_index`: 0.74 for mod_kappa (Cicchetti &
#'   Sparrow, 1981), 0.60 for AC1 and AC2 (Altman, 1991), 0.70 for
#'   Aiken's V (Aiken, 1985).
#' @param point_cex Numeric. Point expansion factor. Default 1.4.
#' @param label_cex Numeric. Label expansion factor. Default 0.75.
#' @param ... Currently ignored.
#'
#' @return Invisibly returns `x`. Called for its side effect (a base R
#'   plot drawn on the current graphics device).
#'
#' @references
#' Aiken, L. R. (1985). Three coefficients for analyzing the reliability
#' and validity of ratings. *Educational and Psychological Measurement*,
#' 45(1), 131-142. \doi{10.1177/0013164485451012}
#'
#' Altman, D. G. (1991). *Practical statistics for medical research*.
#' Chapman and Hall.
#'
#' Cicchetti, D. V., & Sparrow, S. A. (1981). Developing criteria for
#' establishing interrater reliability of specific items. *American
#' Journal of Mental Deficiency*, 86(2), 127-137.
#'
#' Polit, D. F., & Beck, C. T. (2006). The content validity index: Are
#' you sure you know what's being reported? *Research in Nursing &
#' Health*, 29(5), 489-497. \doi{10.1002/nur.20147}
#'
#' @examples
#' data(cvi_example)
#' result <- content_validity(cvi_example)
#' plot(result)
#' plot(result, y_index = "gwet_ac2")
#' plot(result, y_index = "aiken_v", label = "all")
#'
#' @export
plot.content_validity <- function(x,
                                  y = NULL,
                                  y_index = c("mod_kappa", "gwet_ac1",
                                              "gwet_ac2", "aiken_v"),
                                  label = c("flagged", "all", "none"),
                                  flag_logic = c("any", "icvi",
                                                 "y_index", "both"),
                                  flag_threshold_icvi = 0.78,
                                  flag_threshold_y = NULL,
                                  point_cex = 1.4,
                                  label_cex = 0.75,
                                  ...) {

  y_index    <- match.arg(y_index)
  label      <- match.arg(label)
  flag_logic <- match.arg(flag_logic)

  # Pull the I-CVI and chosen y-axis values out of the items table.
  x_vals <- x$items$icvi
  y_vals <- x$items[[y_index]]
  if (is.null(y_vals)) {
    stop("`x` does not carry a `", y_index, "` column. Use a v0.2.0 ",
         "content_validity object or pick a different `y_index`.",
         call. = FALSE)
  }

  if (is.null(flag_threshold_y)) {
    flag_threshold_y <- switch(
      y_index,
      mod_kappa = 0.74,  # Cicchetti & Sparrow (1981): excellent cutoff
      gwet_ac1  = 0.60,  # Altman (1991): good agreement
      gwet_ac2  = 0.60,  # Altman (1991): good agreement
      aiken_v   = 0.70   # Aiken (1985): conventional acceptance
    )
  }

  y_label <- switch(
    y_index,
    mod_kappa = "Modified kappa",
    gwet_ac1  = "Gwet's AC1",
    gwet_ac2  = "Gwet's AC2",
    aiken_v   = "Aiken's V"
  )

  below_icvi <- (x_vals < flag_threshold_icvi) | is.na(x_vals)
  below_y    <- (y_vals < flag_threshold_y)    | is.na(y_vals)

  flagged <- switch(
    flag_logic,
    any     = below_icvi | below_y,
    icvi    = below_icvi,
    y_index = below_y,
    both    = below_icvi & below_y
  )

  # Build a legend label that reflects the chosen flagging rule
  flag_legend <- switch(
    flag_logic,
    any     = sprintf("Below I-CVI or %s threshold", y_label),
    icvi    = "Below I-CVI threshold",
    y_index = sprintf("Below %s threshold", y_label),
    both    = sprintf("Below I-CVI AND %s thresholds", y_label)
  )

  colors <- ifelse(flagged, "#B22222", "#404040")  # firebrick / dark grey
  pchs   <- ifelse(flagged, 19, 19)

  # Determine y-axis range that always shows the threshold line, the
  # data, and a small headroom band.
  y_min_data <- min(c(y_vals, flag_threshold_y), na.rm = TRUE)
  y_max_data <- max(c(y_vals, 1), na.rm = TRUE)
  y_range    <- c(min(-0.2, y_min_data - 0.05),
                  max( 1.0, y_max_data + 0.05))

  graphics::plot(x_vals, y_vals,
                 xlim = c(0, 1.05),
                 ylim = y_range,
                 xlab = "I-CVI",
                 ylab = y_label,
                 main = "Content Validity Index Scatter",
                 sub  = sprintf(
                   "Adequacy: I-CVI >= %.2f, %s >= %.2f",
                   flag_threshold_icvi, y_label, flag_threshold_y),
                 pch  = pchs,
                 col  = colors,
                 cex  = point_cex)

  graphics::abline(v = flag_threshold_icvi, lty = 2, col = "grey60")
  graphics::abline(h = flag_threshold_y,    lty = 2, col = "grey60")
  graphics::abline(h = 0, lty = 3, col = "grey80")

  # Labels
  items_to_label <- switch(
    label,
    flagged = which(flagged),
    all     = seq_along(x_vals),
    none    = integer(0)
  )

  if (length(items_to_label) > 0) {
    graphics::text(x_vals[items_to_label],
                   y_vals[items_to_label],
                   labels = x$items$item[items_to_label],
                   pos    = 4,
                   cex    = label_cex,
                   col    = colors[items_to_label])
  }

  graphics::legend(
    "bottomright",
    legend = c("Acceptable item", flag_legend),
    pch    = c(19, 19),
    col    = c("#404040", "#B22222"),
    bty    = "n",
    cex    = 0.8
  )

  invisible(x)
}
