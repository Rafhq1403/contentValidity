# contentValidity 0.2.0

## New features

* All relevance-scale indices (`icvi()`, `mod_kappa()`, `aiken_v()`,
  `scvi_ave()`, `scvi_ua()`) and `cvr()` now accept an optional `ci = TRUE`
  argument that returns bootstrap confidence intervals alongside the point
  estimate. Default `ci = FALSE` preserves the exact v0.1.0 return type
  (named numeric vector for item-level indices, scalar for scale-level
  indices), so existing code does not break.
* Bootstrap CIs support both the percentile method (default; Efron &
  Tibshirani, 1993) and the bias-corrected accelerated (BCa) method
  (DiCiccio & Efron, 1996). Default 2000 replicates, following Davison &
  Hinkley (1997) and Hesterberg (2015).
* Resampling is performed at the expert (row) level rather than the item
  (column) level, matching the standard inferential frame for inter-rater
  reliability analyses (Gwet, 2014).

## New indices

* `gwet_ac2()` — Gwet's AC2 weighted chance-corrected agreement
  coefficient (Gwet, 2008, 2014) for ordinal ratings. Unlike AC1 (which
  dichotomizes ratings before computing agreement), AC2 preserves the
  full ordinal information through a weight matrix. Supports
  `weights = "quadratic"` (default), `"linear"`, `"identity"`, or a
  custom q x q matrix; requires a `categories` argument to specify the
  full theoretical rating scale (e.g., `1:4` for a standard relevance
  scale). Implementation matches `irrCAC::gwet.ac1.raw()` exactly so
  that AC2 values are bit-for-bit reproducible against the canonical
  reference by Gwet (the original author).
* `gwet_ac1()` — Gwet's AC1 chance-corrected agreement coefficient
  (Gwet, 2008). AC1 uses a marginal-adjusted chance-correction
  (p_e = 2*pi*(1-pi)), which is methodologically distinct from the
  fixed binomial null (C(N,A) * 0.5^N) used by Polit's modified kappa.
  The two indices answer different questions about chance agreement and
  typically diverge when the prevalence of "relevant" ratings is far
  from 0.5; reporting both -- alongside I-CVI -- provides a more
  complete picture of inter-rater agreement than any single index.
  Wongpakaran et al. (2013, *BMC Medical Research Methodology*)
  compared AC1 with Cohen's traditional kappa and recommended AC1 for
  high-prevalence rating contexts; the present package implements both
  AC1 and Polit's modified kappa so the analyst can report whichever
  null model is most defensible. Accepts the bootstrap CI arguments
  described above.

## Visualization

* `plot.content_validity()` — S3 plot method for `content_validity`
  objects. Produces an I-CVI vs. agreement-index scatter with reference
  lines at conventional adequacy cutoffs and automatic flagging of items
  outside the adequacy region. The y-axis index is selectable via
  `y_index` (`"mod_kappa"`, `"gwet_ac1"`, `"gwet_ac2"`, or
  `"aiken_v"`). Parallel in spirit to the difficulty-discrimination
  scatter used in classical item analysis (`mcqAnalysis`).

## Multi-dimensional / subscale analysis

* `content_validity()` accepts a new `subscale` argument that maps
  items to subscales (factors / domains). When supplied, the function
  computes scale-level indices (S-CVI/Ave, S-CVI/UA, mean kappa, mean
  AC1, mean AC2) per subscale in addition to the overall scale, and
  returns the per-subscale results as a `$subscales` data frame.
  Subscales with fewer than two items are reported with `NA` at the
  scale level. This reflects how multi-construct instruments are
  typically structured in practice (e.g., a depression scale with
  separate cognitive and somatic subscales).

## Sample-size planning

* `cv_sample_size_icvi()` — computes the minimum number of expert
  raters required to estimate I-CVI within a specified
  confidence-interval half-width at a chosen confidence level.
  Supports both the closed-form Wald (normal approximation) and the
  Wilson score interval (Wilson, 1927; recommended by Newcombe, 1998
  and Agresti & Coull, 1998 for proportion CIs near the boundary).
  Fills a documented gap in the content-validity literature: Lynn
  (1986) and Polit & Beck (2006) recommend 5--10 experts as a rule of
  thumb without statistical justification, while this function gives a
  precision-based answer suitable for inclusion in study protocols and
  grant applications.

## Wrapper updates

* `content_validity()` now includes `gwet_ac1` and `gwet_ac2` columns in
  its per-item table and `mean_ac1` and `mean_ac2` entries in its
  scale-level vector. New arguments: `categories` (defaults to
  `seq(lo, hi)` so AC2 works on the standard 4-point relevance scale
  with no extra input) and `ac2_weights` (defaults to `"quadratic"`).
  All existing fields are preserved unchanged for backward
  compatibility.
* `apa_table()` now includes "Gwet's AC1" and "Gwet's AC2" columns when
  the underlying `content_validity` object carries them, and silently
  omits them for v0.1.0-style objects that lack the fields.

## Internals

* New internal helpers `bootstrap_ci()` and `bca_ci()` in
  `R/bootstrap_helpers.R` provide a single resampling engine reused by
  every index function (no code duplication across files).


# contentValidity 0.1.0

Initial CRAN release.

## Item-level indices

* `icvi()` — Item-level Content Validity Index (Lynn, 1986).
* `mod_kappa()` — Modified kappa adjusted for chance agreement
  (Polit, Beck, & Owen, 2007).
* `aiken_v()` — Aiken's V coefficient using the full rating scale
  (Aiken, 1985).

## Scale-level indices

* `scvi_ave()` — Scale-level Content Validity Index, average method
  (Polit & Beck, 2006).
* `scvi_ua()` — Scale-level Content Validity Index, universal agreement
  (Polit & Beck, 2006).

## Lawshe's Content Validity Ratio

* `cvr()` — Lawshe's CVR (Lawshe, 1975).
* `cvr_critical()` — exact-binomial critical CVR values
  (Wilson, Pan, & Schumsky, 2012).

## Reporting and convenience

* `content_validity()` — wrapper returning all relevance-scale indices in
  a tidy `content_validity` object, with a custom `print()` method.
* `apa_table()` — publication-ready APA-style tables in data frame,
  markdown, HTML, or LaTeX format.

## Data

* `cvi_example` — simulated 6-expert by 10-item depression screening
  ratings for use in examples and the package vignette.
