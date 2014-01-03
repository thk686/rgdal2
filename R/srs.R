#
# Copyright Timothy H. Keitt
#

#
# Functions for spatial reference systems
#

newRGDAL2SpatialRef = function(handle)
{
    if ( isNullPtr(handle) ) return(NULL)
    attr(handle, "class") = NULL
    reg.finalizer(handle, OSRRelease)
    new("RGDAL2SpatialRef", handle = handle)
}

setMethod("show", "RGDAL2SpatialRef", function(object)
{
    if ( isEmptySRS(object) )
        cat("Empty SRS\n")
    else
        cat(RGDAL_GetPROJ4(object@handle), "\n")
    invisible(object)
})

#' @export
newSRS = function(defn = "WGS84")
{
    defn = getProj4FromAlias(defn)
    x = OSRNewSpatialReference("")
    if ( OSRSetFromUserInput(x, defn) )
        stop('Invalid SRS description')
    newRGDAL2SpatialRef(x)
}

# Probably should be a generic
#' @export
getWKT = function(x)
{
    assertClass(x, "RGDAL2SpatialRef")
    RGDAL_GetWKT(x@handle)
}

#' @export
getPROJ4 = function(x)
{
    stopifnot(inherits(x, "RGDAL2SpatialRef"))
    RGDAL_GetPROJ4(x@handle)
}

#' @export
setMethod('getSRS', 'RGDAL2Dataset',
function(object)
{
    wktdef = GDALGetProjectionRef(object@handle)
    newSRS(wktdef)
})

#' @export
setMethod('getSRS', 'RGDAL2RasterBand',
function(object)
{
    getSRS(object@dataset)
})

#' @export
setMethod("getSRS", "RGDAL2Geometry",
function(object)
{
    x = OGR_G_GetSpatialReference(object@handle)
    if ( isNullPtr(x) ) NULL
    else newRGDAL2SpatialRef(OSRClone(x))
})

#' @export
setMethod("getSRS", "RGDAL2LayerGeometry", function(object)
{
    getSRS(object@layer)
})

#' @export
setMethod("getSRS", "RGDAL2Layer", function(object)
{
    x = OGR_L_GetSpatialRef(object@handle)
    if ( isNullPtr(x) ) NULL
    else newRGDAL2SpatialRef(OSRClone(x))
})

#' @export
setMethod("setSRS",
          signature(object = "RGDAL2Geometry", SRS = "RGDAL2SpatialRef"),
          function(object, SRS)
{
    OGR_G_AssignSpatialReference(object@handle, SRS@handle)
    invisible(object)
})

#' @export
setMethod("setSRS",
          signature(object = "RGDAL2LayerGeometry", SRS = "RGDAL2SpatialRef"),
          function(object, SRS)
{
    warning("Cannot set SRS on geometry owned by layer")
    invisible(object)
})

#' @export
setMethod("setSRS",
          signature(object = "RGDAL2Geometry", SRS = "numeric"),
          function(object, SRS)
{
    setSRS(object, newSRS(paste0("EPSG", SRS, sep = ":")))
})

#' @export
setMethod("setSRS",
          signature(object = "RGDAL2Geometry", SRS = "character"),
          function(object, SRS)
{
    srs = newSRS(SRS)
    setSRS(object, srs)
})

#' @export
setMethod("setSRS",
          signature(object = "RGDAL2Dataset", SRS = "RGDAL2SpatialRef"),
          function(object, SRS)
{
    if ( GDALSetProjection(object@handle, getWKT(SRS)) )
        warning("Error setting projection")
    invisible(object)
})

#' @export
setMethod("setSRS",
          signature(object = "RGDAL2RasterBand", SRS = "RGDAL2SpatialRef"),
          function(object, SRS)
{
    if ( GDALSetProjection(object@dataset@handle, getWKT(SRS)) )
        warning("Error setting projection")
    invisible(object)
})

#' @export
setMethod("setSRS",
    signature(object = "ANY", SRS = "NULL"),
    function(object, SRS)
{
    warning("SRS not set; input SRS is NULL")
    object          
})

#' @export
setMethod("reproject",
    signature(object = "RGDAL2Geometry", SRS = "RGDAL2SpatialRef"),
    function(object, SRS)
{
    if ( isEmptySRS(SRS) ) return(object)
    if ( hasSRS(object) )
    {
        x = OGR_G_Clone(object@handle)
        if ( OGR_G_TransformTo(x, SRS@handle) )
            stop("Error reprojecting geometry")
        res = newRGDAL2Geometry(x)
        setSRS(res, SRS)
        res
    }
    else
    {
        setSRS(object, SRS)
        object
    }
})

#' @export
setMethod("reproject",
          signature(object = "RGDAL2Geometry", SRS = "numeric"),
          function(object, SRS)
{
    reproject(object, newSRS(paste0("EPSG", SRS, sep = ":")))
})

#' @export
setMethod("reproject",
          signature(object = "RGDAL2Geometry", SRS = "character"),
          function(object, SRS)
{
    reproject(object, newSRS(SRS))
})

#' @export
setMethod("reproject",
          signature(object = "ANY", SRS = "NULL"),
          function(object, SRS)
{
    warning("Object not reprojected; input SRS is NULL")
    object
})

isGeographic = function(x)
{
    if ( !inherits(x, "RGDAL2SpatialRef") ) x = getSRS(x)
    OSRIsGeographic(x@handle) == 1
}

hasSRS = function(x)
{
    !is.null(getSRS(x))
}

isEmptySRS = function(x)
{
    nchar(getWKT(x)) == 0
}

