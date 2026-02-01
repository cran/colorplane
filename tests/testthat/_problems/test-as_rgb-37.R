# Extracted from test-as_rgb.R:37

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "colorplane", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
cp <- ConstantColorPlane("#00FF00FF")
result <- as_rgb(cp)
expect_true(is.matrix(result))
expect_equal(result[1, 1], 0)
