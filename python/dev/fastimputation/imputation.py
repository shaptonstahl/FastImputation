"""
FastImputer: conditional multivariate-normal imputation.

Ports the R FastImputation package algorithm to Python, following
the same two-phase design:

  1. fit()  — learn means, covariance, variable metadata from training data.
  2. transform()  — impute missing values row-by-row via
     E[Y|X] = μY + ΣYX ΣXX⁻¹ (X - μX).

Bounded variables are transformed to the real line before MVN estimation
and back-transformed after imputation (log for one-sided; inverse-normal
CDF for two-sided).  Categorical variables are one-hot encoded internally;
the imputed dummy block is decoded by argmax.

The covariance estimator is a faithful port of the Lounici (2012) unbiased
estimator used by the R package.
"""

import warnings

import numpy as np
import pandas as pd
from scipy import stats
from sklearn.base import BaseEstimator, TransformerMixin

# ---------------------------------------------------------------------------
# Module-level helpers (ported directly from the R source files)
# ---------------------------------------------------------------------------


def _covariance_with_missing(data):
    """
    Lounici (2012) unbiased covariance estimator for data with missing values.

    Faithful port of R's ``covariance_with_missing()``.

    Parameters
    ----------
    data : ndarray of shape (n_samples, n_features)
        May contain NaN.

    Returns
    -------
    cov : ndarray of shape (n_features, n_features)
    """
    delta = np.mean(~np.isnan(data))

    if delta == 1.0:
        # No missing data — use standard (unbiased) covariance.
        return np.cov(data, rowvar=False, ddof=1)

    # Transpose to Lounici notation: observations in columns, variables in rows.
    x = data.T  # shape (p, n)
    n = x.shape[1]  # number of observations
    # p = x.shape[0]  # number of variables

    # Per page 1 of the article: centre each variable by its observed mean,
    # then replace remaining NaN with 0.
    row_means = np.nanmean(x, axis=1, keepdims=True)
    y = x - row_means
    y = np.nan_to_num(y, nan=0.0)

    # Equation at the bottom of page 3.
    sigma_delta_n = (y @ y.T) / n

    # Equation 1.4.
    return ((delta - 1) * np.diag(np.diag(sigma_delta_n)) + sigma_delta_n) / delta**2


def _normalize_bounded_variable(x, constraints):
    """
    Transform a bounded variable to the real line.

    Faithful port of R's ``normalize_bounded_variable()``.

    Parameters
    ----------
    x : ndarray of shape (n,)  — may contain NaN.
    constraints : dict with optional keys ``'lower'`` and/or ``'upper'``.

    Returns
    -------
    out : ndarray of shape (n,)  — NaN values are preserved.
    """
    tol = stats.norm.cdf(-5)  # ≈ 2.867e-7, matches R's pnorm(-5)
    lower = constraints.get("lower", -np.inf)
    upper = constraints.get("upper", np.inf)

    if np.isfinite(lower) and np.isfinite(upper) and upper < lower:
        raise ValueError("'upper' must be greater than 'lower'")

    x = x.copy()
    nan_mask = np.isnan(x)

    # Count (and warn about) out-of-bounds values, then clip.
    n_clipped = int(np.sum((x[~nan_mask] < lower) | (x[~nan_mask] > upper)))
    if n_clipped > 0:
        warnings.warn(
            f"{n_clipped} value(s) were trimmed to the boundary.", stacklevel=2
        )

    # np.maximum / np.minimum propagate NaN, so NaN cells stay NaN.
    x = np.maximum(x, lower)
    x = np.minimum(x, upper)

    # Push values strictly inside the boundary.
    if np.isfinite(lower):
        x = np.maximum(lower + tol, x)
    if np.isfinite(upper):
        x = np.minimum(upper - tol, x)

    if np.isinf(lower) and np.isinf(upper):
        out = x
    elif np.isinf(lower):
        # Only bounded above: log(upper - x)
        out = np.log(upper - x)
    elif np.isinf(upper):
        # Only bounded below: log(x - lower)
        out = np.log(x - lower)
    else:
        # Bounded on both sides: qnorm((x - lower) / (upper - lower))
        out = stats.norm.ppf((x - lower) / (upper - lower))

    out[nan_mask] = np.nan
    return out


def _bound_normalized_variable(x, constraints):
    """
    Back-transform from the real line to the bounded domain.

    Faithful port of R's ``bound_normalized_variable()``.

    Parameters
    ----------
    x : ndarray of shape (n,)
    constraints : dict with optional keys ``'lower'`` and/or ``'upper'``.

    Returns
    -------
    out : ndarray of shape (n,)
    """
    lower = constraints.get("lower", -np.inf)
    upper = constraints.get("upper", np.inf)

    if np.isinf(lower) and np.isinf(upper):
        return x
    elif np.isinf(lower):
        return upper - np.exp(x)
    elif np.isinf(upper):
        return np.exp(x) + lower
    else:
        return stats.norm.cdf(x) * (upper - lower) + lower


# ---------------------------------------------------------------------------
# Main class
# ---------------------------------------------------------------------------


class FastImputer(BaseEstimator, TransformerMixin):
    """
    Fast imputation of missing values using the conditional multivariate
    normal distribution.

    Ports the R FastImputation package to Python with a scikit-learn
    compatible interface.

    Parameters
    ----------
    constraints : dict, optional
        Mapping from column name to a bound specification dict.
        Each spec may have keys ``'lower'`` and/or ``'upper'``.
        Example: ``{'age': {'lower': 0}, 'prob': {'lower': 0, 'upper': 1}}``.
    ignore_cols : list of str, optional
        Columns to carry through unchanged (e.g. ID columns).
    categorical : list of str, optional
        Columns containing discrete values; internally one-hot encoded.
    """

    def __init__(self, constraints=None, ignore_cols=None, categorical=None):
        self.constraints = constraints
        self.ignore_cols = ignore_cols
        self.categorical = categorical

    # ------------------------------------------------------------------
    # sklearn API
    # ------------------------------------------------------------------

    def fit(self, x, y=None):
        """
        Learn imputation parameters from training data.

        Parameters
        ----------
        x : pandas DataFrame of shape (n_samples, n_features)
        y : ignored

        Returns
        -------
        self
        """
        x = pd.DataFrame(x)
        constraints = self.constraints or {}
        ignore_cols = list(self.ignore_cols or [])
        categorical = list(self.categorical or [])

        self.var_names_ = list(x.columns)

        # Drop ignored columns from the active set.
        active_cols = [c for c in x.columns if c not in ignore_cols]
        w = x[active_cols].copy()

        # Capture sorted category levels (matches R's factor level ordering).
        self.categories_ = {}
        for col in categorical:
            if col in w.columns:
                self.categories_[col] = sorted(w[col].dropna().unique().tolist())

        # Normalize bounded variables in-place.
        for col, con in constraints.items():
            if col in w.columns:
                w[col] = _normalize_bounded_variable(w[col].values.astype(float), con)

        self.cols_categorical_ = [c for c in categorical if c in w.columns]
        self.cols_bound_ = [c for c in constraints if c in w.columns]

        # Build the model matrix z = [non-cat columns | one-hot dummies].
        self.non_cat_cols_ = [c for c in w.columns if c not in self.cols_categorical_]
        parts = []
        if self.non_cat_cols_:
            parts.append(w[self.non_cat_cols_].astype(float))
        for col in self.cols_categorical_:
            col_vals = w[col]
            for level in self.categories_[col]:
                dummy = np.where(
                    col_vals.isna(), np.nan, (col_vals == level).astype(float)
                )
                parts.append(pd.Series(dummy, index=w.index, name=f"{col}_{level}"))

        z = pd.concat(parts, axis=1) if parts else pd.DataFrame(index=w.index)
        self.z_columns_ = list(z.columns)

        z_arr = z.values.astype(float)

        self.means_ = np.nanmean(z_arr, axis=0)
        self.covariance_ = _covariance_with_missing(z_arr)

        # Regularise: ensure all eigenvalues >= 0.01.
        # Mirrors R's Matrix::nearPD call in train_fast_imputation().
        min_ev = 0.01
        eigenvalues, eigenvectors = np.linalg.eigh(self.covariance_)
        if np.min(eigenvalues) < min_ev:
            eigenvalues = np.maximum(eigenvalues, min_ev)
            cov = eigenvectors @ np.diag(eigenvalues) @ eigenvectors.T
            self.covariance_ = (cov + cov.T) / 2  # symmetrise

        return self

    def transform(self, x):
        """
        Impute missing values using the fitted conditional MVN model.

        Parameters
        ----------
        x : pandas DataFrame with the same columns as the training data.

        Returns
        -------
        out : pandas DataFrame — same shape/columns as x, NaN filled in.
        """
        x = pd.DataFrame(x)
        constraints = self.constraints or {}
        ignore_cols = list(self.ignore_cols or [])

        if list(x.columns) != self.var_names_:
            raise ValueError(
                f"Column mismatch. Expected {self.var_names_}, got {list(x.columns)}."
            )

        active_cols = [c for c in x.columns if c not in ignore_cols]
        w = x[active_cols].copy()

        # Normalize bounded variables.
        for col in self.cols_bound_:
            w[col] = _normalize_bounded_variable(
                w[col].values.astype(float), constraints[col]
            )

        # Build z with the same column layout as during fit.
        parts = []
        if self.non_cat_cols_:
            parts.append(w[self.non_cat_cols_].astype(float))
        for col in self.cols_categorical_:
            col_vals = w[col]
            for level in self.categories_[col]:
                dummy = np.where(
                    col_vals.isna(), np.nan, (col_vals == level).astype(float)
                )
                parts.append(pd.Series(dummy, index=w.index, name=f"{col}_{level}"))

        z = pd.concat(parts, axis=1) if parts else pd.DataFrame(index=w.index)
        z_arr = z.values.astype(float)

        means = self.means_
        cov = np.array(self.covariance_)
        n_rows, n_cols = z_arr.shape

        # Row-by-row conditional MVN imputation.
        # E[Y | X_obs] = μY + ΣYX ΣXX⁻¹ (X_obs - μX)
        for i in range(n_rows):
            row = z_arr[i]
            missing_mask = np.isnan(row)
            if not np.any(missing_mask):
                continue

            missing = np.where(missing_mask)[0]
            known = np.where(~missing_mask)[0]

            if len(known) == 0:
                z_arr[i, missing] = means[missing]
            else:
                cov_yx = cov[np.ix_(missing, known)]
                cov_xx = cov[np.ix_(known, known)]
                x_diff = row[known] - means[known]
                try:
                    z_arr[i, missing] = means[missing] + cov_yx @ np.linalg.solve(
                        cov_xx, x_diff
                    )
                except np.linalg.LinAlgError:
                    z_arr[i, missing] = means[missing]

        # Write imputed non-cat values back into w.
        for j, col in enumerate(self.non_cat_cols_):
            w[col] = z_arr[:, j]

        # Recover categorical columns via argmax over dummy block.
        dummy_offset = len(self.non_cat_cols_)
        for col in self.cols_categorical_:
            levels = self.categories_[col]
            n_levels = len(levels)
            block = z_arr[:, dummy_offset : dummy_offset + n_levels]
            best_idx = np.argmax(block, axis=1)
            w[col] = pd.Categorical(
                [levels[idx] for idx in best_idx],
                categories=levels,
            )
            dummy_offset += n_levels

        # Back-transform bounded variables.
        for col in self.cols_bound_:
            w[col] = _bound_normalized_variable(
                w[col].values.astype(float), constraints[col]
            )

        # Reconstruct full output (ignored columns kept from original x).
        out = x.copy()
        for col in active_cols:
            out[col] = w[col]

        return out
