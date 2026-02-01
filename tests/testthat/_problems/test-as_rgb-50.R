# Extracted from test-as_rgb.R:50

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "colorplane", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
clr <- c("#FF000080", "#00FF00FF")
cp <- HexColorPlane(clr)
result <- as_rgb(cp)
expect_equal(result[1, 4], 128)
expect_equal(result[2, 4], 255)
