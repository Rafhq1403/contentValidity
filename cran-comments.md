## Test environments

* Local: macOS aarch64-apple-darwin20, R 4.5.2

## R CMD check results

0 errors | 0 warnings | 3 notes

All three notes are expected and either standard for new submissions or
specific to the developer's local environment:

* `checking CRAN incoming feasibility ... NOTE`
  - "New submission" — this is the package's first submission to CRAN.

* `checking for future file timestamps ... NOTE`
  - "unable to verify current time" — the developer's machine could not
    reach the time-verification server during the check. This is a local
    network issue and is not expected to occur on CRAN's check machines.

* `checking HTML version of manual ... NOTE`
  - "Skipping checking HTML validation: 'tidy' doesn't look like recent
    enough HTML Tidy." — the developer's local HTML Tidy installation is
    older than the version expected by R's HTML validator. CRAN's check
    machines have a current HTML Tidy installed, so this note will not
    appear in CRAN's own checks.

## Reverse dependencies

This is a new package, so there are no reverse dependencies.
