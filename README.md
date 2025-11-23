Fast Imputation
==============
[![Build Status](https://travis-ci.org/shaptonstahl/FastImputation.svg?branch=master)](https://travis-ci.org/shaptonstahl/FastImputation)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/FastImputation)](http://cran.r-project.org/package=FastImputation)

`R` and Python packages for filling in missing data. Work well for imputing before running a machine learning prediction algorithm.

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

To estimate the covariance matrix $\Sigma$, Fast Imputation uses the estimator given in [Lounici (2012), Equation 1.4](https://arxiv.org/abs/1201.2577v5). Let $X$ be the data matrix, and $Y_i$ the $i^\text{th}$ row of $X$. Set $\delta$ to be the fraction of observed _cells_ in the data matrix ($\delta=1$ means no missing data).


The empirical covariance matrix is given by

$$
\Sigma_n^{(\delta)} = \frac{1}{n} \sum_{i=1}^n Y_i \otimes Y_i
$$

Then the unbiased estimate of the covariance matrix is given by

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
Let `df` be the data frame to be imputed.

## Only unbounded numeric features

1. Train Fit the FastImputer object on the data frame
3. Impute the data frame

```R
# R
library(FastImputation)

fi_model <- TrainFastImputation(df)
df_imputed <- FastImputation(df, fi_model)
```
```python
# Python
from fastimputation import FastImputer

fi = FastImputer()
df_imputed = fi.fit_transform(df)
```


## Some bounded numeric features


## Some categorical features


## How to suppress using some columns (like ids) when imputing

