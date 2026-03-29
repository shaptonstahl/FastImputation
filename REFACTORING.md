## Goals

1. Refactor the R package to use tidyverse tools and syntax conventions.

2. Support integrated use with tidyverse tools.

3. Preserve speed of scoring.

## Scope

We're only changing the R package. No changes to the Python package. Everything in the R package may be changed.

## Approach / phases

### Phase 1 — Package infrastructure (DONE)
Bumped version to 3.0.0. Added tidyverse imports: `dplyr`, `tidyr`, `purrr`, `tibble`, `tidyselect`, `rlang`, `recipes`. Added vignette infrastructure.

### Phase 2 — Rename internal helpers (DONE)
`NormalizeBoundedVariable` → `normalize_bounded_variable`, `BoundNormalizedVariable` → `bound_normalized_variable`, `CovarianceWithMissing` → `covariance_with_missing`. All three are now internal (unexported).

### Phase 3 — New public API (DONE)
- `TrainFastImputation()` → `train_fast_imputation()` with tidyselect column selection
- `FastImputation()` → `fast_imputation()` with pipe-friendly signature, tibble-aware output, `verbose = FALSE` default
- New class name: `fast_imputation_patterns`

### Phase 4 — S3 methods (DONE)
`print.fast_imputation_patterns()` and `tidy.fast_imputation_patterns()` added in `R/methods.R`.

### Phase 5 — recipes integration (DONE)
`step_fast_imputation()` added in `R/step_fast_imputation.R` with `prep`, `bake`, and `tidy` methods.

### Phase 6 — Tests and docs (DONE)
All test files updated. `vignettes/fastimputation.Rmd` added.

### Remaining: run roxygenise and CRAN check
```r
roxygen2::roxygenise()
devtools::test()
covr::package_coverage()
devtools::check(args = "--as-cran")
```

## Constraints

- This is a breaking change. The versions will be 3.x.y.

## Definition of done

1. Passes all tests for publishing to CRAN.

2. 100% test coverage.

3. Thorough documentation, including a vignette.

## API syntax decisions

| Topic | Decision |
|-------|----------|
| Function naming | snake_case: `train_fast_imputation`, `fast_imputation` |
| Column selection | tidyselect (bare names + helpers like `starts_with()`) |
| recipes integration | Yes — `step_fast_imputation()` |
| Helper functions | Internal only (unexported), renamed to snake_case |
| `verbose` default | FALSE |
| Class name | `fast_imputation_patterns` (was `FastImputationPatterns`) |
