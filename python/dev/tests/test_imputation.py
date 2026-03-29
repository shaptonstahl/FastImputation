"""Tests for FastImputer."""

import numpy as np
import pandas as pd
from fastimputation.imputation import (
    FastImputer,
    _bound_normalized_variable,
    _covariance_with_missing,
    _normalize_bounded_variable,
)
from sklearn.pipeline import Pipeline

# ---------------------------------------------------------------------------
# Helper
# ---------------------------------------------------------------------------


def _make_correlated(n=100, seed=0):
    """Return a DataFrame where b ≈ 2*a."""
    rng = np.random.default_rng(seed)
    a = rng.normal(5, 1, n)
    b = 2 * a + rng.normal(0, 0.1, n)
    return pd.DataFrame({"a": a, "b": b})


# ---------------------------------------------------------------------------
# _covariance_with_missing
# ---------------------------------------------------------------------------


class TestCovarianceWithMissing:
    def test_no_missing_matches_numpy_cov(self):
        rng = np.random.default_rng(1)
        x = rng.normal(size=(50, 3))
        np.testing.assert_allclose(
            _covariance_with_missing(x),
            np.cov(x, rowvar=False, ddof=1),
            rtol=1e-10,
        )

    def test_returns_symmetric_matrix(self):
        rng = np.random.default_rng(2)
        x = rng.normal(size=(30, 4))
        x[::5, 0] = np.nan
        cov = _covariance_with_missing(x)
        np.testing.assert_allclose(cov, cov.T, atol=1e-12)

    def test_returns_square_matrix_correct_size(self):
        rng = np.random.default_rng(3)
        x = rng.normal(size=(20, 5))
        x[0, 2] = np.nan
        cov = _covariance_with_missing(x)
        assert cov.shape == (5, 5)


# ---------------------------------------------------------------------------
# _normalize_bounded_variable / _bound_normalized_variable
# ---------------------------------------------------------------------------


class TestBoundedVariableTransforms:
    def test_lower_only_roundtrip(self):
        x = np.array([1.0, 2.0, 5.0])
        con = {"lower": 0.0}
        rt = _bound_normalized_variable(_normalize_bounded_variable(x, con), con)
        np.testing.assert_allclose(rt, x, rtol=1e-6)

    def test_upper_only_roundtrip(self):
        x = np.array([-1.0, 0.0, 3.0])
        con = {"upper": 5.0}
        rt = _bound_normalized_variable(_normalize_bounded_variable(x, con), con)
        np.testing.assert_allclose(rt, x, atol=1e-10)

    def test_two_sided_roundtrip(self):
        x = np.array([0.1, 0.5, 0.9])
        con = {"lower": 0.0, "upper": 1.0}
        rt = _bound_normalized_variable(_normalize_bounded_variable(x, con), con)
        np.testing.assert_allclose(rt, x, rtol=1e-6)

    def test_unbounded_is_identity(self):
        x = np.array([-3.0, 0.0, 3.0])
        con = {}
        np.testing.assert_array_equal(_normalize_bounded_variable(x, con), x)
        np.testing.assert_array_equal(_bound_normalized_variable(x, con), x)

    def test_normalize_preserves_nan(self):
        x = np.array([1.0, np.nan, 3.0])
        out = _normalize_bounded_variable(x, {"lower": 0.0})
        assert np.isnan(out[1])
        assert not np.isnan(out[0])
        assert not np.isnan(out[2])


# ---------------------------------------------------------------------------
# FastImputer.fit / transform — no missing values
# ---------------------------------------------------------------------------


class TestNoMissingValues:
    def test_continuous_unchanged(self):
        df = pd.DataFrame({"a": [1.0, 2.0, 3.0, 4.0], "b": [4.0, 5.0, 6.0, 7.0]})
        imp = FastImputer()
        result = imp.fit_transform(df)
        np.testing.assert_allclose(result[["a", "b"]].values, df[["a", "b"]].values)

    def test_categorical_unchanged(self):
        df = pd.DataFrame({"x": [1.0, 2.0, 3.0, 4.0], "cat": ["a", "b", "a", "b"]})
        imp = FastImputer(categorical=["cat"])
        result = imp.fit_transform(df)
        assert list(result["cat"]) == ["a", "b", "a", "b"]

    def test_columns_preserved(self):
        df = pd.DataFrame({"id": [1, 2, 3], "a": [1.0, 2.0, 3.0], "b": [4.0, 5.0, 6.0]})
        imp = FastImputer(ignore_cols=["id"])
        result = imp.fit_transform(df)
        assert list(result.columns) == list(df.columns)


# ---------------------------------------------------------------------------
# FastImputer — fills in missing values
# ---------------------------------------------------------------------------


class TestFillsMissingValues:
    def test_no_nan_after_imputation(self):
        df = _make_correlated(100)
        df_missing = df.copy()
        df_missing.loc[[10, 20, 30], "a"] = np.nan
        df_missing.loc[[15, 25], "b"] = np.nan

        imp = FastImputer()
        imp.fit(df)
        result = imp.transform(df_missing)

        assert not result.isnull().any().any()

    def test_imputed_values_are_reasonable(self):
        """When b ≈ 2*a, imputing b given a should give ≈ 2*a."""
        df = _make_correlated(200)
        imp = FastImputer()
        imp.fit(df)

        df_test = pd.DataFrame({"a": [3.0, 5.0, 7.0], "b": [np.nan, np.nan, np.nan]})
        result = imp.transform(df_test)

        for a_val, b_imputed in zip([3.0, 5.0, 7.0], result["b"], strict=False):
            assert abs(b_imputed - 2 * a_val) < 1.0

    def test_all_missing_row_gets_means(self):
        df = pd.DataFrame({"a": [1.0, 2.0, 3.0], "b": [4.0, 5.0, 6.0]})
        imp = FastImputer()
        imp.fit(df)

        df_test = pd.DataFrame({"a": [np.nan], "b": [np.nan]})
        result = imp.transform(df_test)

        assert not result.isnull().any().any()
        np.testing.assert_allclose(result["a"].iloc[0], imp.means_[0], rtol=1e-6)
        np.testing.assert_allclose(result["b"].iloc[0], imp.means_[1], rtol=1e-6)


# ---------------------------------------------------------------------------
# Constraints
# ---------------------------------------------------------------------------


class TestConstraints:
    def test_lower_bound_respected(self):
        rng = np.random.default_rng(42)
        df_train = pd.DataFrame(
            {"a": rng.uniform(1, 10, 100), "b": rng.normal(0, 1, 100)}
        )
        imp = FastImputer(constraints={"a": {"lower": 0}})
        imp.fit(df_train)

        df_test = pd.DataFrame({"a": [np.nan] * 20, "b": rng.normal(0, 1, 20)})
        result = imp.transform(df_test)

        assert (result["a"] >= 0).all()

    def test_upper_bound_respected(self):
        rng = np.random.default_rng(7)
        df_train = pd.DataFrame(
            {"a": rng.uniform(0, 9, 100), "b": rng.normal(0, 1, 100)}
        )
        imp = FastImputer(constraints={"a": {"upper": 10}})
        imp.fit(df_train)

        df_test = pd.DataFrame({"a": [np.nan] * 20, "b": rng.normal(0, 1, 20)})
        result = imp.transform(df_test)

        assert (result["a"] <= 10).all()

    def test_two_sided_bounds_respected(self):
        rng = np.random.default_rng(99)
        df_train = pd.DataFrame(
            {"p": rng.uniform(0.1, 0.9, 200), "x": rng.normal(0, 1, 200)}
        )
        imp = FastImputer(constraints={"p": {"lower": 0, "upper": 1}})
        imp.fit(df_train)

        df_test = pd.DataFrame({"p": [np.nan] * 50, "x": rng.normal(0, 1, 50)})
        result = imp.transform(df_test)

        assert (result["p"] >= 0).all()
        assert (result["p"] <= 1).all()


# ---------------------------------------------------------------------------
# ignore_cols
# ---------------------------------------------------------------------------


class TestIgnoreCols:
    def test_ignored_column_passes_through_unchanged(self):
        df = pd.DataFrame(
            {
                "id": ["r1", "r2", "r3", "r4"],
                "a": [1.0, np.nan, 3.0, 4.0],
                "b": [4.0, 5.0, np.nan, 7.0],
            }
        )
        imp = FastImputer(ignore_cols=["id"])
        result = imp.fit_transform(df)

        assert list(result["id"]) == ["r1", "r2", "r3", "r4"]

    def test_imputation_still_fills_active_columns(self):
        df = pd.DataFrame(
            {
                "id": [1, 2, 3, 4],
                "a": [1.0, np.nan, 3.0, 4.0],
                "b": [4.0, 5.0, np.nan, 7.0],
            }
        )
        imp = FastImputer(ignore_cols=["id"])
        result = imp.fit_transform(df)

        assert not result[["a", "b"]].isnull().any().any()

    def test_output_has_same_columns(self):
        df = pd.DataFrame({"id": [1, 2], "a": [1.0, 2.0], "b": [3.0, 4.0]})
        imp = FastImputer(ignore_cols=["id"])
        result = imp.fit_transform(df)

        assert list(result.columns) == ["id", "a", "b"]


# ---------------------------------------------------------------------------
# Categorical columns
# ---------------------------------------------------------------------------


class TestCategorical:
    def test_imputed_values_are_valid_categories(self):
        rng = np.random.default_rng(5)
        cats = ["low", "mid", "high"]
        n = 60
        df = pd.DataFrame(
            {
                "x": rng.normal(0, 1, n),
                "cat": rng.choice(cats, n),
            }
        )
        df_missing = df.copy()
        df_missing.loc[:4, "cat"] = np.nan

        imp = FastImputer(categorical=["cat"])
        imp.fit(df)
        result = imp.transform(df_missing)

        assert result["cat"].isin(cats).all()

    def test_known_categories_unchanged(self):
        df = pd.DataFrame({"x": [1.0, 2.0, 3.0, 4.0], "cat": ["a", "b", "a", "b"]})
        imp = FastImputer(categorical=["cat"])
        result = imp.fit_transform(df)

        assert list(result["cat"]) == ["a", "b", "a", "b"]

    def test_no_nan_after_categorical_imputation(self):
        rng = np.random.default_rng(8)
        n = 40
        df = pd.DataFrame(
            {
                "x": rng.normal(0, 1, n),
                "cat": rng.choice(["yes", "no"], n),
            }
        )
        df_missing = df.copy()
        df_missing.loc[::3, "x"] = np.nan
        df_missing.loc[1::3, "cat"] = np.nan

        imp = FastImputer(categorical=["cat"])
        imp.fit(df)
        result = imp.transform(df_missing)

        assert not result.isnull().any().any()


# ---------------------------------------------------------------------------
# sklearn Pipeline compatibility
# ---------------------------------------------------------------------------


class TestSklearnPipeline:
    def test_pipeline_fit_transform(self):
        df = pd.DataFrame(
            {"a": [1.0, 2.0, np.nan, 4.0, 5.0], "b": [2.0, np.nan, 6.0, 8.0, 10.0]}
        )
        pipe = Pipeline([("imputer", FastImputer())])
        result = pipe.fit_transform(df)

        assert not pd.DataFrame(result).isnull().any().any()

    def test_pipeline_fit_then_transform(self):
        df_train = _make_correlated(50)
        df_test = _make_correlated(10)
        df_test.loc[[0, 3, 7], "b"] = np.nan

        pipe = Pipeline([("imputer", FastImputer())])
        pipe.fit(df_train)
        result = pipe.transform(df_test)

        assert not pd.DataFrame(result).isnull().any().any()

    def test_pipeline_with_constraints(self):
        rng = np.random.default_rng(11)
        df = pd.DataFrame({"a": rng.uniform(1, 5, 60), "b": rng.normal(0, 1, 60)})
        df_missing = df.copy()
        df_missing.loc[::5, "a"] = np.nan

        pipe = Pipeline([("imputer", FastImputer(constraints={"a": {"lower": 0}}))])
        result = pipe.fit_transform(df_missing)

        assert (pd.DataFrame(result)["a"] >= 0).all()

    def test_get_params_set_params(self):
        """Verify sklearn's get_params / set_params contract."""
        imp = FastImputer(
            constraints={"a": {"lower": 0}}, ignore_cols=["id"], categorical=["cat"]
        )
        params = imp.get_params()
        assert params["constraints"] == {"a": {"lower": 0}}
        assert params["ignore_cols"] == ["id"]
        assert params["categorical"] == ["cat"]

        imp2 = FastImputer()
        imp2.set_params(**params)
        assert imp2.constraints == {"a": {"lower": 0}}
