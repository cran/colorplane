# Extracted from test-alpha_channel.R:65

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "colorplane", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
cp <- ConstantColorPlane("#FF0000FF")
result <- alpha_channel(cp, normalize = FALSE)
expect_equal(result, 255)
