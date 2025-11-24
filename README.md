Fast Imputation
==============
[![Build Status](https://travis-ci.org/shaptonstahl/FastImputation.svg?branch=master)](https://travis-ci.org/shaptonstahl/FastImputation)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/FastImputation)](http://cran.r-project.org/package=FastImputation)

`R` and Python packages for filling in missing data. The Fast Imputation algorithm works well for imputing before running a machine learning or statistical prediction algorithm.

**Caution**: Imputation adds new values that are not actual data, so it will increase the variance of the model. It is recommended to use imputation as a preprocessing step, and to use cross-validation to tune the hyperparameters of the model.

## How does it work?
Fast Imputation trains on observations with missing values and uses the trained model to impute the missing values, either in the same data frame or in a new data frame. It's fast, so it can be used to impute a single observation, an entire data frame, or in a pipeline.

### Numeric features
Suppose you have a data frame of all numeric features. When training, Fast Imputation estiamtes the covariance matrix of the data, ignoring missing values. When imputing, it generates the mean of each missing value conditional on the non-missing values.

### Bounded numeric features
These are converted to the entire real line before applying the covariance matrix formula. The imputed value is automatically converted back to the original bounded numeric variable.

### Categorical features
These are converted to dummy variables before applying the covariance matrix formula. The imputed value is automatically converted back to the original categorical variable.

### But missing values bias the covariance matrix estimate!
A normal call (below) like would provide a biased estimate of the covariance matrix when there are missing values.

```R
# R, biased estimate of covariance matrix when there are missing values
cov(df) 🚫
```
```python
# Python, biased estimate of covariance matrix when there are missing values
np.cov(df) 🚫
```

To estimate the covariance matrix $\Sigma$, Fast Imputation uses the estimator given in [Lounici (2012), Equation 1.4](https://arxiv.org/abs/1201.2577v5). Let $X_{i\times n}$ be the data matrix with the $n$ columns normalized to have mean zero, and $Y_i$ the $i^\text{th}$ observation (row) of $X$. Set $\delta$ to be the fraction of observed _cells_ in the data matrix ($\delta=1$ means no missing data).


The empirical covariance matrix is given by

$$
\Sigma_n^{(\delta)} = \frac{1}{n} \sum_{i=1}^n Y_i \otimes Y_i
$$

Then the unbiased estimate of the covariance matrix $\Sigma$ is given by Equation 1.4:

$$
\tilde{\Sigma}_n = (\delta^{-1}-\delta^{-2}) \text{diag}(\Sigma_n^{(\delta)}) + \delta^{-2}\Sigma_n^{(\delta)}
$$

This provides an unbiased estimate of the covariance matrix when the data has missing values. It is a scalable algorithm because $\Sigma_n^{(\delta)}$ is generated one observation at a time, so it can be trained on very large datasets using appropriate parallelization.


# Installation

## R
```R
install.packages("FastImputation")
```

## Python
```python
pip install fastimputation
```

# How to use
Consider a data frame `df` that you will use to train/tune/test a prediction model. Let `df_train` be the data frame used to train the model, `df_tune` be the data frame used to tune the model, and `df_test` be the data frame used to test the model.

## Only unbounded numeric features

Suppose `df` only has unbounded numeric features. Then we can impute the missing values in `df_train`, `df_tune`, and `df_test` using the Fast Imputation algorithm.

1. Train/fit the Fast Imputation model on the training data frame `df_train`.
2. Impute (fill in estimates for) the training data frame `df_train`.
3. Impute the tuning data frame `df_tune`.
4. Impute the testing data frame `df_test`.

Note that we only look at the training data frame `df_train` to train the Fast Imputation model. We do not look at the tuning data frame `df_tune` or the testing data frame `df_test` to train the Fast Imputation model because that would introduce leakage into the prediction model.

```R
# R
library(FastImputation)

fi_model <- TrainFastImputation(df_train)  # Train the Fast Imputation model

df_train_imputed <- FastImputation(df_train, fi_model)  # Impute the training data frame
df_tune_imputed <- FastImputation(df_tune, fi_model)  # Impute the tuning data frame
df_test_imputed <- FastImputation(df_test, fi_model)  # Impute the testing data frame
```
```python
# Python
from fastimputation import FastImputer

fi = FastImputer()
df_train_imputed = fi.fit_transform(df_train)  # Train the Fast Imputation model and impute the training data frame
df_tune_imputed = fi.transform(df_tune)  # Impute the tuning data frame
df_test_imputed = fi.transform(df_test)  # Impute the testing data frame
```

## Some bounded numeric features
Fast Imputation can handle constrained/bounded numeric features. They can be bounded below, bounded above, or both.

Suppose `df` has some bounded numeric features. Specifically:
- `feature_a` is positive.
- `feature_b` is negative.
- `feature_c` is in (0,1).

We follow the same procedure as above, but we need to specify the bounded numeric features.

```R
# R
library(FastImputation)

fi_model <- TrainFastImputation(
    df_train,
    constraints=list(
        list("feature_a", list(lower=0)),
        list("feature_b", list(upper=0)),
        list("feature_c", list(lower=0, upper=1))
    )
)

df_train_imputed <- FastImputation(df_train, fi_model)
df_tune_imputed <- FastImputation(df_tune, fi_model)
df_test_imputed <- FastImputation(df_test, fi_model)
```
```python
# Python
from fastimputation import FastImputer

...
```

## Some categorical features

Suppose `df` has a categorical feature, specifically `feature_d`.

```R
# R
library(FastImputation)

fi_model <- TrainFastImputation(df_train, 
    constraints=list(list(categorical="feature_d"))
)

df_train_imputed <- FastImputation(df_train, fi_model)
df_tune_imputed <- FastImputation(df_tune, fi_model)
df_test_imputed <- FastImputation(df_test, fi_model)
```
```python
# Python
from fastimputation import FastImputer

...
```

## Columns we want to ignore

We may have columns we want to ignore, such as an identifier column, a free text field, etc.

Suppose in `df_train`, `df_tune`, and `df_test` we want to ignore the column `id`.

```R
# R
library(FastImputation)

fi_model <- TrainFastImputation(df_train, 
    idvars=list("id")
)

df_train_imputed <- FastImputation(df_train, fi_model)
df_tune_imputed <- FastImputation(df_tune, fi_model)
df_test_imputed <- FastImputation(df_test, fi_model)
```
```python
# Python
from fastimputation import FastImputer

...
```
