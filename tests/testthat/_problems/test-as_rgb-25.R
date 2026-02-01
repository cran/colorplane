# Extracted from test-as_rgb.R:25

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "colorplane", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
clr <- c("#FF0000FF", "#00FF00FF", "#0000FFFF")
cp <- HexColorPlane(clr)
result <- as_rgb(cp)
expect_true(is.matrix(result))
expect_equal(nrow(result), 3)
expect_equal(ncol(result), 4)
expect_equal(result[1, 1], 255)
