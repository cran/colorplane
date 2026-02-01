# Extracted from test-alpha_channel.R:57

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "colorplane", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
cp <- ConstantColorPlane("#FF000080")
result <- alpha_channel(cp)
expect_equal(result, 128/255)
