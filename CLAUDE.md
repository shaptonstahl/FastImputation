# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Current work

- Branch `claude-achin-for-bacon`: R package refactored to snake_case API (v3.0.0) with tidymodels `step_fast_imputation()` support; Python implementation fully ported from stub to working code; linting/formatting toolchain added (ruff, lintr, styler, pre-commit, GitHub Actions).

## Project Overview

FastImputation is an R package for statistical imputation of missing data using conditional multivariate normal distributions. It is published on CRAN. The package is optimized for fast single-row imputation, making it suitable for use before running machine learning prediction algorithms. A Python implementation also lives in this repo.

## Repository Layout

```
R/                        — all R development
  FastImputation.Rproj    — open this in RStudio
  dev/FastImputation/     — the R package source (all R package work happens here)
    R/                    — R source files
    tests/testthat/       — testthat unit tests
    man/                  — auto-generated roxygen2 docs (do not edit manually)
    data/                 — example datasets (FI_train, FI_test, FI_true)
  archive/                — historical released versions
python/
  dev/fastimputation/     — Python package source
  dev/tests/              — Python tests
pyproject.toml            — Python package config (root-level)
```

## Common Commands

### R

All commands below are run from an R console with the working directory set to `R/dev/FastImputation/` (or from the project root after opening `R/FastImputation.Rproj` in RStudio).

**Generate documentation and NAMESPACE:**

```r
roxygen2::roxygenise()
```

**Build the package:**

```r
devtools::build()
```

**Run all tests:**

```r
devtools::test()
```

**Run a single test file:**

```r
testthat::test_file("tests/testthat/test_train_fast_imputation.R")
```

**Check package (CRAN standards):**

```r
devtools::check(args = "--as-cran")
```

**Install package locally:**

```r
devtools::install()
```

### Python

From the repo root:

```bash
pip install -e .
pytest python/dev/tests/
```

**Lint and format (Python):**

```bash
ruff check python/
ruff format python/
```

### Pre-commit hooks

```bash
pre-commit install        # install hooks once
pre-commit run --all-files  # run manually
```

## Architecture

### Core Algorithm (R)

The package implements two phases:

1. **Training (`train_fast_imputation`)**: Takes a data frame and produces a `fast_imputation_patterns` object containing means, covariance matrix, variable bounds, and categorical level mappings.

2. **Imputation (`fast_imputation`)**: Uses the stored patterns with the conditional multivariate normal formula to impute missing values row-by-row:
$$E[Y|X] = μY + ΣYX * ΣXX⁻¹ * (X - μX)$$
### Variable Transformation Pipeline

Both phases apply transformations before/after the MVN calculation:

- **Bounded variables**: Normalized via `normalize_bounded_variable` (log transform for one-sided bounds; inverse normal CDF `qnorm` for two-sided bounds), then recovered via `bound_normalized_variable`.
- **Categorical variables**: One-hot encoded via `fastDummies::dummy_cols` before training/imputation; after imputation the max-index dummy is selected and mapped back to the original factor levels.
- **Ignored columns** (e.g., ID columns passed via `ignore_cols`): Removed before processing and restored afterward.
- **Tidymodels integration**: `step_fast_imputation()` wraps the two-phase algorithm as a `recipes` step, using `tidyselect` column selectors.

### Key Design Details

- **Covariance with missing data**: `covariance_with_missing` uses the Lounici (2012) unbiased estimator rather than imputing before estimating covariance.
- **Singular covariance handling**: One-hot encoding can produce near-singular covariance matrices; `Matrix::nearPD()` regularizes eigenvalues below 0.01.
- **Factor handling**: Factor columns are converted inline to character or numeric (trying numeric first) using a base R `lapply` in `train_fast_imputation`.

### R Dependencies

- **Imports**: `methods`, `Matrix`, `fastDummies`, `generics`, `dplyr`, `tibble`, `tidyselect`, `rlang`, `recipes`
- **Suggests**: `testthat`, `caret`, `e1071`, `knitr`, `rmarkdown`

## Documentation

All R documentation is written as roxygen2 comments in source files. Running `roxygen2::roxygenise()` regenerates the `.Rd` files in `man/` and the `NAMESPACE` file. Never edit `man/*.Rd` or `NAMESPACE` directly.

## Known Issues

None at this time.