# Extracted from test-map_colors.R:120

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "colorplane", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
lookup <- list(a = "#FF0000FF", b = "#00FF00FF", c = "#0000FFFF")
cp <- DiscreteColorPlane(lookup)
result <- map_colors(cp, values = c("a", "b", "c", "a"))
expect_s4_class(result, "HexColorPlane")
expect_length(result@clr, 4)
expect_equal(result@clr[1], "#FF0000FF")
