Sys.setenv("R_TESTS" = "")
library(testthat)
library(blblm)

test_check("blblm")
