rgdal2
======

[![Build Status](https://travis-ci.org/thk686/rgdal2.png?branch=master)](https://travis-ci.org/thk686/rgdal2)

R bindings to GDAL/OGR

This is a rewrite of my original rgdal bindings found in the package rgdal on CRAN.

You need to install GDAL for this package to work. See http://www.gdal.org/. Binaries can be
found easily using links on that site or using Google search. If typing 'gdal-config' on the
command line does not do anything, this package will not install.

With the "devtools" package installed, the following R command builds the package on my platform: 

devtools::install_github("rgdal2", "thk686")
