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
# devtools::build()
