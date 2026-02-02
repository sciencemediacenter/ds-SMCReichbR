library(devtools)
library(roxygen2)
library(testthat)

if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}


devtools::document()
devtools::load_all()
devtools::test()
devtools::check()
devtools::build()


###################
## build install ##
###################
setwd("../..")
system("R CMD build ds-SMCReichbR --resave-data")
system("R CMD check SMCReichbR_0.0-1.tar.gz --as-cran")

###############################################
## Install locally to test for hidden errors ##
###############################################
devtools::install("ds-SMCReichbR")
