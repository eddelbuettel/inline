
library(inline)

## basic examples from manual page
fx <- cxxfunction(signature(x = "integer", y = "numeric"),
                  "return ScalarReal(INTEGER(x)[0] * REAL(y)[0]);")
expect_true(is(fx, "CFunc"))
expect_equal(fx(2L, 5), 10)

if (!requireNamespace("Rcpp", quietly=TRUE)) exit_file("Need Rcpp for remainder of tests")

fx <- cxxfunction(signature(x = "integer", y = "numeric"),
                  "return wrap(as<int>(x) * as<double>(y));",
                  plugin = "Rcpp")
expect_true(is(fx, "CFunc"))
expect_equal(fx(2L, 5), 10)

## equivalent shorter form using rcpp()
fx <- rcpp(signature(x = "integer", y = "numeric"),
           "return wrap(as<int>(x) * as<double>(y));")
expect_true(is(fx, "CFunc"))
expect_equal(fx(2L, 5), 10)
