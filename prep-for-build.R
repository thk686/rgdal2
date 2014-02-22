library(Rcpp)
compileAttributes()
library(roxygen2)
roxygenize('.', roclets=c('rd', 'namespace'))
