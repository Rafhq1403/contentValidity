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
