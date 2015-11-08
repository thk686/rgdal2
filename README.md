rgdal2
======

[![Build Status](https://travis-ci.org/edzer/rgdal2.png?branch=master)](https://travis-ci.org/edzer/rgdal2)

R bindings to GDAL/OGR

**Please note: if you have tried previous versions, there was a serious bug in handling return values in Rcpp (whose behavior is largely undocumented upstream) that could crash the R session. Try this version.**

This is a rewrite of my original rgdal bindings found in the package rgdal on CRAN.

You need to install GDAL for this package to work. See http://www.gdal.org/. Binaries can be
found easily using links on that site or using Google search. If typing 'gdal-config' on the
command line does not do anything, this package will not install.

With the "devtools" package installed, the following R command builds the package on my platform: 

devtools::install_github("roxygen2", "yihui")

devtools::install_github("rgdal2", "thk686")

You might not need the line for roxygen2 if the CRAN version has been fixed.
