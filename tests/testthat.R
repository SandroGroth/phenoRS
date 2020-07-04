library(zoo)

library(testthat)
library(phenoRS)

Sys.setenv("R_TEST" = "")

test_check("phenoRS")
