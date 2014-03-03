rgdal2
======

R bindings to GDAL/OGR

This is a rewrite of my original rgdal bindings found in the package rgdal on CRAN.

You need to install GDAL for this package to work. See http://www.gdal.org/. Binaries can be
found easily using links on that site or using Google search. If typing 'gdal-config' on the
command line does not do anything, this package will not install.

I have modified the configure script to automatically build the Rcpp bindings and manual pages if
they are missing. If you have Rcpp and roxygen2 installed, it should just work. Otherwise see below.

To use this code, clone the repo, open the RStudio project file (rgdal2.Rproj) and choose build and install.
You have to install roxygen2 and Rcpp.

The commands necessary to generate the Rcpp and Rd files without
using RStudio are given in the file prep-for-build.R. You can run
this directly like R CMD BATCH prep-for-build.R, then
cd .. and install.


