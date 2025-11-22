import unittest
from fast_imputation.imputation import FastImputer

class TestFastImputer(unittest.TestCase):
    def test_init(self):
        imputer = FastImputer()
        self.assertIsInstance(imputer, FastImputer)

    def test_fit_transform(self):
        imputer = FastImputer()
        data = [[1, 2], [3, 4]]
        imputer.fit(data)
        result = imputer.transform(data)
        self.assertEqual(result, data)

if __name__ == '__main__':
    unittest.main()
