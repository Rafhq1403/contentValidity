## Submission notes

This is a feature update to contentValidity 0.1.0 (currently on CRAN since
2026-05-15). Version 0.2.0 adds substantive methodological extensions
that respond to peer-review feedback received on the original release.

### What's new in 0.2.0

* All five v0.1.0 relevance-scale indices (`icvi`, `mod_kappa`,
  `aiken_v`, `scvi_ave`, `scvi_ua`) and Lawshe's `cvr` now support
  optional bootstrap confidence intervals (percentile and BCa methods)
  through a unified internal engine. Backward-compatible: existing
  v0.1.0 calls return the same numeric vectors / scalars with no change
  in interpretation.

* Two new chance-corrected agreement indices for content-validity
  contexts: `gwet_ac1()` (binary, with dichotomization at the relevance
  threshold) and `gwet_ac2()` (ordinal, weighted; quadratic / linear /
  identity / custom weight matrices supported). The AC2 implementation
  follows the canonical irrCAC formulation by Kilem Gwet and produces
  values bit-for-bit equivalent to `irrCAC::gwet.ac1.raw()` on the same
  inputs.

* A new sample-size planning function `cv_sample_size_icvi()` returns
  the minimum number of expert raters required to estimate I-CVI within
  a specified confidence-interval half-width. Both Wald (closed-form)
  and Wilson (numerically solved) methods are provided.

* The `content_validity()` wrapper now reports AC1 and AC2 alongside
  the original indices and supports a `subscale` argument for
  multi-construct instruments.

* A new `plot.content_validity()` S3 method produces an I-CVI vs.
  agreement-index scatter with reference cutoffs and automatic flagging
  of items outside the adequacy region.

* `apa_table()` now supports per-index interpretation columns (mod_kappa,
  gwet_ac1, gwet_ac2, or icvi cutoffs) with the column positioned
  immediately adjacent to its source index for unambiguous reading.

No existing functions changed in argument order or default behaviour, so
all v0.1.0 code continues to work unchanged.

## Test environments

* Local: macOS aarch64-apple-darwin20, R 4.5.2
* Win-builder: R-devel and R-release
* GitHub Actions: macOS-latest, windows-latest, ubuntu-latest
  (release and devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

This is an update to an existing CRAN package. No reverse dependencies
exist as of the v0.2.0 submission date.
