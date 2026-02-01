# Extracted from test-map_colors.R:132

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "colorplane", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
lookup <- list(a = "#FF0000FF", b = "#00FF00FF")
cp <- DiscreteColorPlane(lookup)
result <- map_colors(cp, values = c("a", "x", "b"))
expect_length(result@clr, 3)
expect_equal(result@clr[2], "000000FF")
