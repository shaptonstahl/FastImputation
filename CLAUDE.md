# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

FastImputation is an R package for statistical imputation of missing data using conditional multivariate normal distributions. It is published on CRAN. The package is optimized for fast single-row imputation, making it suitable for use before running machine learning prediction algorithms.

## Repository Layout

- `dev/FastImputation/` — the actual R package source (all R development happens here)
- `dev/FastImputation/R/` — R source files
- `dev/FastImputation/tests/testthat/` — testthat unit tests
- `dev/FastImputation/man/` — auto-generated roxygen2 documentation (.Rd files, do not edit manually)
- `dev/FastImputation/data/` — example datasets (FI_train, FI_test, FI_true)

## Common Commands

All commands below are run from an R console with the working directory set to `dev/FastImputation/` (or from the project root after opening `FastImputation.Rproj` in RStudio).

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
testthat::test_file("tests/testthat/test_TrainFastImputation.R")
```

**Check package (CRAN standards):**
```r
devtools::check(args = "--as-cran")
```

**Install package locally:**
```r
devtools::install()
```

## Architecture

### Core Algorithm

The package implements two phases:

1. **Training (`TrainFastImputation`)**: Takes a data frame and produces a `FastImputationPatterns` object containing means, covariance matrix, variable bounds, and categorical level mappings.

2. **Imputation (`FastImputation`)**: Uses the stored patterns with the conditional multivariate normal formula to impute missing values row-by-row:
   `E[Y|X] = μY + ΣYX * ΣXX⁻¹ * (X - μX)`

### Variable Transformation Pipeline

Both phases apply transformations before/after the MVN calculation:

- **Bounded variables**: Normalized via `NormalizeBoundedVariable` (log transform for one-sided bounds; inverse normal CDF `qnorm` for two-sided bounds), then recovered via `BoundNormalizedVariable`.
- **Categorical variables**: One-hot encoded via `fastDummies::dummy_cols` before training/imputation; after imputation the max-index dummy is selected and mapped back to the original factor levels.
- **Ignored columns** (e.g., ID columns passed via `ignore_cols`): Removed before processing and restored afterward.

### Key Design Details

- **Covariance with missing data**: `CovarianceWithMissing` uses the Lounici (2012) unbiased estimator rather than imputing before estimating covariance.
- **Singular covariance handling**: One-hot encoding can produce near-singular covariance matrices; `Matrix::nearPD()` regularizes eigenvalues below 0.01.
- **Factor handling**: `UnfactorColumns` converts factor columns to character or numeric before processing. `WhichAreFactors` identifies factor columns.

### Parameter Names (Current Branch: `categorical`)

The `categorical` branch renamed parameters from older versions:
- `idvars` → `ignore_cols`
- `cols_categorical` → `categorical`

### Dependencies

- **Imports**: `methods`, `Matrix`, `fastDummies`
- **Suggests**: `testthat`, `caret`, `e1071`

## Documentation

All documentation is written as roxygen2 comments in source files. Running `roxygen2::roxygenise()` regenerates the `.Rd` files in `man/` and the `NAMESPACE` file. Never edit `man/*.Rd` or `NAMESPACE` directly.
