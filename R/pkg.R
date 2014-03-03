#
# Copyright Timothy H. Keitt
#

#' @useDynLib rgdal2

#' @name rgdal2-package
#' @aliases rgdal2-package, rgdal2
#' @docType package
#' @title Support for geospatial data analysis in R
#' @description
#' Routines for reading, writing and analyzing data stored in common geospatial
#' data formats. Subsetting, querying, intersecting and reprojection of raster
#' and vector data.
#' @details
#' \tabular{ll}{
#' Package: \tab rgdal2\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2013-11-14\cr
#' License: \tab GPL \cr
#' }
#' Access to GDAL/OGR data sources is accomplished with \code{openGDAL} and 
#' \code{openOGR}. These return S4 classes holding pointers to the GDAL/OGR data
#' structures. Stanard R-like access function are provided. These S4 objects are 
#' convertable to grid graphics objects. A generic \code{draw} function is
#' provided for building plots.
#' @author
#' Timothy H. Keitt \cr \url{http://www.keittlab.org/} \cr \cr
#' Maintainer: Timothy H. Keitt \email{tkeitt@@gmail.com} \cr
#' @references \url{http://github.com/thk686/rgdal2}, \url{http://www.gdal.org/}
#' @keywords package
NULL

printPkgBanner = function()
{
  quiet = getOption("suppress.package.banners")
  if ( is.null(quiet) || (!quiet) )
  {
    msgfun = packageStartupMessage
    n = 42
    bling = function(n) msgfun(paste(rep("~", n), collapse = "."))
    bling(n);
    msgfun("\trgal2: a KeittLab production (http://www.keittlab.org/)")
    msgfun(paste0("\tCompiled against GDAL version: ", versionInfo()))
    msgfun("\tSet options(suppress.package.banners = TRUE) to load quietly")
    bling(n)
  }
}

.onAttach = function(libname, pkgname)
{
    GDALInit()
}

.onDetach = function(libpath)
{
  RGDAL_CleanupAll()
}

