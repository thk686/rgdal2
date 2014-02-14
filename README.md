rgdal2
======

R bindings to GDAL/OGR

This is a rewrite of my original rgdal bindings found in the package rgdal on CRAN.

This repo is an alpha release / preview before I upload to CRAN. The package name will be "rgdal2".

To use this code, clone the repo, open the RStudio project file (rgdal2.Rproj) and choose build and install.
You have to install roxygen2 and Rcpp.

The commands necessary to generate the Rcpp and Rd files without
using RStudio are given in the file prep-for-build.R. You can run
this directly like R CMD BATCH prep-for-build.R, then
cd .. and install.


