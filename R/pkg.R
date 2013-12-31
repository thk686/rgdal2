#
# Copyright Timothy H. Keitt
#
.onAttach = function(libname, pkgname)
{
    quiet = getOption("suppress.package.banners")
    if ( is.null(quiet) || (!quiet) )
    {
        n = 42
        bling = function(n) cat(paste(rep("~", n), collapse = "."), "\n")
        bling(n);
        cat("\trgal2: a KeittLab production (http://www.keittlab.org/)\n")
        cat(paste0("\tCompiled against GDAL version: ", versionInfo(), "\n"))
        cat("\tSet options(suppress.package.banners = TRUE) to load quietly\n")
        bling(n)
    }
}

.onDetach = function(libpath)
{
  RGDAL_CleanupAll()
}

