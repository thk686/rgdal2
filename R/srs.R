#
# Copyright Timothy H. Keitt
#

#' @include defs.R
NULL

#
# Functions for spatial reference systems
#

newRGDAL2SpatialRef = function(handle)
{
    reg.finalizer(handle, RGDAL_OSRRelease)
    new("RGDAL2SpatialRef", handle = handle)
}

setMethod("show",
signature("RGDAL2SpatialRef"),
function(object)
{
    if ( isEmptySRS(object) )
        cat("Empty SRS\n")
    else
        cat(RGDAL_GetPROJ4(object@handle), "\n")
    invisible(object)
})

#' Construct a spatial reference system descriptor
#' 
#' Builds a spatial reference system descriptor based on a
#' provided string.
#' 
#' @param defn a string defining a spatial reference system
#' 
#' @details
#' The input definition can be in one of four forms:
#' 1) a PROJ4 string; 2) a Well-Known-Text string; 3) an EPSG code in the
#' form EPSG:####; or 4) an alias as listed below.
#'
#' Aliases: \cr
#' WGS84 = "+proj=longlat +datum=WGS84 +no_defs" \cr
#' NAD83 = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs" \cr
#' USNatAtl = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs" \cr
#' NALCC = "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" \cr
#' NAAEAC = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" \cr
#' Robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" \cr
#' Mollweide = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" \cr
#' GRS80 = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
#' 
#' @seealso \code{\link{getSRS}}, \code{\link{setSRS}}
#' 
#' @examples
#' x = newSRS("EPSG:3126")
#' show(x)
#' 
#' @export
newSRS = function(defn = "WGS84")
{
  defn = getProj4FromAlias(defn)
  x = RGDAL_OSRNewSpatialReference("")
  if ( nchar(defn) )
    if ( RGDAL_OSRSetFromUserInput(x, defn) )
      stop('Invalid SRS description')
  newRGDAL2SpatialRef(x)
}

getWKT = function(x)
{
    assertClass(x, "RGDAL2SpatialRef")
    RGDAL_GetWKT(x@handle)
}

getPROJ4 = function(x)
{
    assertClass(x, "RGDAL2SpatialRef")
    RGDAL_GetPROJ4(x@handle)
}

#' Gets or sets the spatial reference system object
#' 
#' Retrieves or sets the spatial reference system object
#' 
#' @param object the object containing or receiving the spatial reference system
#' 
#' @seealso \code{\link{newSRS}}
#' 
#' @examples
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openGDALBand(f)
#' a = getSRS(x)
#' show(a)
#' y = copyDataset(x)
#' setSRS(y, a)
#' 
#' @aliases getSRS setSRS
#' @rdname get-set-srs
#' @export
setMethod('getSRS',
signature('RGDAL2Dataset'),
function(object)
{
    newSRS(RGDAL_GetProjectionRef(object@handle))
})

#' @rdname get-set-srs
#' @export
setMethod('getSRS',
signature('RGDAL2RasterBand'),
function(object)
{
    getSRS(object@dataset)
})

#' @rdname get-set-srs
#' @export
setMethod("getSRS",
signature("RGDAL2Geometry"),
function(object)
{
    x = RGDAL_G_GetSpatialReference(object@handle)
    if ( is.null(x) ) NULL else newRGDAL2SpatialRef(x)
})

#' @rdname get-set-srs
#' @export
setMethod("getSRS",
signature("RGDAL2Layer"),
function(object)
{
    x = RGDAL_L_GetSpatialRef(object@handle)
    if ( is.null(x) ) NULL else newRGDAL2SpatialRef(x)
})

#' @aliases setSRS
#' @param SRS the spatial reference system as an object, string or number
#' @rdname get-set-srs
#' @export
setMethod("setSRS",
signature(object = "RGDAL2Geometry", SRS = "RGDAL2SpatialRef"),
function(object, SRS)
{
    RGDAL_G_AssignSpatialReference(object@handle, SRS@handle)
    invisible(object)
})

#' @rdname get-set-srs
#' @export
setMethod("setSRS",
signature(object = "RGDAL2Geometry", SRS = "numeric"),
function(object, SRS)
{
    setSRS(object, newSRS(paste0("EPSG", SRS, sep = ":")))
})

#' @rdname get-set-srs
#' @export
setMethod("setSRS",
signature(object = "RGDAL2Geometry", SRS = "character"),
function(object, SRS)
{
    srs = newSRS(SRS)
    setSRS(object, srs)
})

#' @rdname get-set-srs
#' @export
setMethod("setSRS",
signature(object = "RGDAL2Dataset", SRS = "RGDAL2SpatialRef"),
function(object, SRS)
{
    if ( RGDAL_SetProjection(object@handle, getWKT(SRS)) )
        warning("Error setting projection")
    invisible(object)
})

#' @rdname get-set-srs
#' @export
setMethod("setSRS",
signature(object = "RGDAL2RasterBand", SRS = "RGDAL2SpatialRef"),
function(object, SRS)
{
    if ( RGDAL_SetProjection(object@dataset@handle, getWKT(SRS)) )
        warning("Error setting projection")
    invisible(object)
})

#' @note
#' Calling \code{setSRS} with \code{SRS = NULL} is a no-op that simply
#' returns the object without effect.
#' 
#' @rdname get-set-srs
#' @export
setMethod("setSRS",
signature(object = "ANY", SRS = "NULL"),
function(object, SRS)
{
    # warning("SRS not set; input SRS is NULL")
    object          
})

#' Transform vector geometries
#' 
#' Project data to new reference system
#' 
#' @param object the object to reproject
#' @param SRS a spatial reference system descriptor
#' 
#' @details
#' The SRS can be given as an SRS object, a string or number. Numbers will be coverted to an EPSG code.
#' Strings can be SRS aliases (see \code{\link{newSRS}}) or anything GDAL understands including PROJ4
#' strings.
#' 
#' @examples
#' g1 = newGeometry("POINT", list(x = 1, y = 1), newSRS("WGS84"))
#' show(g1)
#' g2 = reproject(g1, newSRS("Moll"))
#' show(g2)
#' 
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGRLayer(f)
#' 
#' g = getGeometries(x)
#' draw(reproject(graticule(), getSRS(x)))
#' invisible(lapply(g, draw, overlay = TRUE, gp = gpar(fill = "lightblue")))
#' grid.text("WGS84 projection (lon-lat)", y = 1.1)
#'    
#' g = lapply(g, reproject, "Robinson")
#' draw(reproject(graticule(), "Robinson"))
#' invisible(lapply(g, draw, overlay = TRUE, gp = gpar(fill = "lightblue")))
#' grid.text("Robinson projection", y = 1.1)
#' 
#' @aliases reproject-geometry
#' @rdname reproject-vector
#' @export
setMethod("reproject",
signature(object = "RGDAL2Geometry", SRS = "RGDAL2SpatialRef"),
function(object, SRS)
{
    if ( isEmptySRS(SRS) ) return(object)
    if ( hasSRS(object) )
    {
        x = RGDAL_G_Clone(object@handle)
        if ( RGDAL_G_TransformTo(x, SRS@handle) )
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

#' @rdname reproject-vector
#' @export
setMethod("reproject",
signature(object = "RGDAL2Geometry", SRS = "numeric"),
function(object, SRS)
{
    reproject(object, newSRS(paste0("EPSG", SRS, sep = ":")))
})

#' @rdname reproject-vector
#' @export
setMethod("reproject",
signature(object = "RGDAL2Geometry", SRS = "character"),
function(object, SRS)
{
    reproject(object, newSRS(SRS))
})

#' @rdname reproject-vector
#' @export
setMethod("reproject",
signature(object = "ANY", SRS = "NULL"),
function(object, SRS)
{
    warning("Object not reprojected; input SRS is NULL")
    object
})

#' Simple raster warping
#' 
#' Reproject a raster object
#' 
#' @param object the raster object (dataset or band)
#' @param SRS the new spatial reference system
#' @param file the output filed (ignored if \code{driver = "MEM"})
#' @param driver the output driver
#' @param thresh resampling threshold
#' 
#' @details
#' This function calls a modified version of the GDAL warpsimple utility.
#' 
#' If \code{object} is a raster band, the entire dataset will be warped
#' and the first band of the dataset returned. See \code{\link{getDataset}}.
#' 
#' @examples
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openGDAL(f)
#' show(grid.layout(1, 2))
#' draw(x)
#' y = reproject(x, newSRS("WGS84"))
#' draw(y)
#' 
#' @aliases reproject-dataset
#' @rdname reproject-raster
#' @export
setMethod("reproject",
signature("RGDAL2Dataset"),
function(object, SRS, file = tempfile(), driver = "MEM", thresh = 0.125)
{
  srs.out = getWKT(SRS)
  res = RGDAL_RasterWarp(object@handle, file, srs.out, driver, thresh)
  newRGDAL2Dataset(res)
})

#' @aliases reproject-band
#' @rdname reproject-raster
#' @export
setMethod("reproject",
signature("RGDAL2RasterBand"),
function(object, SRS, file = tempfile(), driver = "MEM", thresh = 0.125)
{
  srs.out = getWKT(SRS)
  res = RGDAL_RasterWarp(object@dataset@handle, file, srs.out, driver, thresh)
  getBand(newRGDAL2Dataset(res))
})

isGeographic = function(x)
{
    if ( !inherits(x, "RGDAL2SpatialRef") ) x = getSRS(x)
    RGDAL_IsGeographic(x@handle) == 1
}

hasSRS = function(x)
{
    !is.null(getSRS(x))
}

isEmptySRS = function(x)
{
    nchar(getWKT(x)) == 0
}

