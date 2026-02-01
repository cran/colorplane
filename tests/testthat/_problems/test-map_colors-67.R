# Extracted from test-map_colors.R:67

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "colorplane", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
intensity <- -5:5
cp <- IntensityColorPlane(intensity)
result <- map_colors(cp, threshold = c(-2, 2))
expect_true(all(result@clr[5:7, 4] == 0))
expect_true(result@clr[4, 4] == 0)
expect_true(result@clr[8, 4] == 0)
