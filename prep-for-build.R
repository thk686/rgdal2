library(Rcpp)
library(roxygen2)
roxygenize('.', roclets=c('rd', 'namespace'))
compileAttributes()
