#
# Copyright Timothy H. Keitt
#

#' Open an OGR datasource
#'
#' Opens the datasource associated with the specified file.
#' 
#' @param fname the file name
#' @param readonly if true, disable write operations
#' 
#' @details
#' A data source can be treated as a named list of layers.
#' 
#' @return an object of class RGDAL2Datasource
#' 
#' @examples
#' f = system.file("example-data/tl_2013_us_state.shp", package = "rgdal2")
#' x = openOGR(f)
#' show(x)
#' 
#' @family ogr-io
#' @export
openOGR = function(fname, readonly = TRUE)
{
    x = RGDAL_OGROpen(fname, readonly)
    newRGDAL2Datasource(x)
}

#' Create an OGR datasource
#'
#' Creates a new OGR datasource
#' 
#' @param driver the file name
#' @param fname name of file to use for storage
#' 
#' @details
#' The \code{MEM} driver creates the datasource in memeory, in which case
#' the \code{fname} parameter is ignored.
#' 
#' @return an object of class RGDAL2Datasource
#' 
#' @family ogr-io
#' @export
newOGRDatasource = function(driver = "MEM", fname = tempfile())
{
    x = RGDAL_CreateDataSource(driver, fname)
    newRGDAL2Datasource(x)
}

setMethod('show', 'RGDAL2Datasource',
function(object)
{
    ogrinfo = Sys.which('ogrinfo')
    dsname = OGR_DS_GetName(object@handle)
    drname = RGDAL_GetDSDriverName(object@handle)
    if ( nchar(ogrinfo) > 0 && drname != "Memory" )
    {
        info = pipe(paste(ogrinfo, '-so', dsname), 'rt')
        res = readLines(info); close(info)
        catLines(res)
        return(invisible(res))
    }
    else
    {
        cat('OGR datasource at address ')
        print(object@handle)
        return(invisible(object))    
    }
})

#' Methods for data sources
#' 
#' List-like access to layers in a data source
#' 
#' @param x the data source
#' 
#' @examples
#' f = system.file("example-data/tl_2013_us_state.shp", package = "rgdal2")
#' x = openOGR(f)
#' length(x)
#' names(x)
#' class(x[[1]])
#' class(x[1])
#' class(x$"tl_2013_us_state")
#' 
#' @aliases vector-data-source
#' @rdname data-source-methods
#' @export
setMethod('length', "RGDAL2Datasource",
function(x)
{
    OGR_DS_GetLayerCount(x@handle)
})

#' @rdname data-source-methods
#' @export
setMethod("names", "RGDAL2Datasource", function(x)
{
    res = character(length(x))
    for ( i in 1:length(res) )
        res[i] = getLayerName(getLayer(x, i))
    res
})

#' @param i the layer number
#' @rdname data-source-methods
#' @export
setMethod("[","RGDAL2Datasource",
function(x, i)
{
    res = NULL
    for ( ii in i )
        res = append(res, getLayer(x, ii))
    res
})

#' @rdname data-source-methods
#' @export
setMethod("[[", "RGDAL2Datasource",
function(x, i)
{
    getLayer(x, i)    
})

#' @param name the name of the layer
#' @rdname data-source-methods
#' @export
setMethod("$", "RGDAL2Datasource",
function(x, name)
{
    getLayer(x, name)
})

setMethod("properties", "RGDAL2Datasource",
function(object)
{
    list(create.layer = testCapability(object, "create.layer"),
         delete.layer = testCapability(object, "delete.layer"),
         create.geom.field = testCapability(object, "create.geom.field"))
})

#' Open an OGR layer
#' 
#' Opens an OGR data source and returns a layer
#' 
#' @param layer the layer number
#' @param readonly if true, dissable writes
#' 
#' @examples
#' f = system.file("example-data/tl_2013_us_state.shp", package = "rgdal2")
#' x = openOGRLayer(f)
#' show(x)
#' 
#' @family ogr-io
#' @export
openOGRLayer = function(fname, layer = 1L, readonly = TRUE)
{
    getLayer(openOGR(fname, readonly), layer)
}

#' Fetch layer name
#' 
#' @param x a layer object
#' @return the name of the layer
#' 
#' @examples
#' f = system.file("example-data/tl_2013_us_state.shp", package = "rgdal2")
#' x = openOGRLayer(f)
#' getLayerName(x)
#' 
#' @family layer-io
#' @export
getLayerName = function(x)
{
    assertClass(x, "RGDAL2Layer")
    OGR_L_GetName(x@handle)
}

setMethod('show', 'RGDAL2Layer',
function(object)
{
    ogrinfo = Sys.which('ogrinfo')
    dsname = OGR_DS_GetName(object@datasource@handle)
    lyrname = getLayerName(object)
    if ( nchar(ogrinfo) > 0 )
    {
        info = pipe(paste(ogrinfo, '-so', dsname, lyrname), 'rt')
        res = readLines(info); close(info)
        catLines(res)
        return(invisible(res))
    }
    else
    {
        cat('OGR layer at address ')
        print(object@handle)
        return(invisible(object))    
    }
})

#' Methods for data layers
#' 
#' Quasi-dataframe-like access to layers in a data layer (not fully implemented).
#' 
#' @param x the data source
#' 
#' @examples
#' f = system.file("example-data/tl_2013_us_state.shp", package = "rgdal2")
#' x = openOGRLayer(f)
#' dim(x)
#' length(x)
#' names(x)
#' 
#' @family layer-io
#' @aliases vector-layer
#' @rdname layer-methods
#' @export
setMethod('dim', 'RGDAL2Layer',
function(x)
{
  rows = RGDAL_OGR_L_GetFeatureCount(x@handle)
  cols = GetLayerFieldCount(x@handle)
  c(rows, cols)
})

#' @rdname layer-methods
#' @export
setMethod('length', 'RGDAL2Layer',
function(x)
{
  GetLayerFieldCount(x@handle)
})

#' @rdname layer-methods
#' @export
setMethod('names', 'RGDAL2Layer',
function(x)
{
  GetFieldNames(x@handle)
})

setMethod("properties", "RGDAL2Layer",
function(object)
{
    list(random.read = testCapability(object, "random.read"),
         sequential.write = testCapability(object, "sequential.write"),
         random.write = testCapability(object, "random.write"),
         fast.spatial.filter = testCapability(object, "fast.spatial.filter"),
         fast.feature.count = testCapability(object, "fast.feature.count"),
         fast.get.extent = testCapability(object, "fast.get.extent"),
         fast.set.next.by.index = testCapability(object, "fast.set.next.by.index"),
         create.field = testCapability(object, "create.field"),
         create.geom.field = testCapability(object, "create.geom.field"),
         delete.field = testCapability(object, "delete.field"),
         reorder.fields = testCapability(object, "reorder.fields"),
         alter.field.defn = testCapability(object, "alter.field.defn"),
         delete.feature = testCapability(object, "delete.feature"),
         transactions = testCapability(object, "transactions"))
})

#' Query data source and return a layer
#' 
#' Execute SQL query and return results as a layer
#' 
#' @param x a data source
#' @param sql an SQL query string
#' 
#' @details
#' \code{getSQLLayer} executes the SQL query and returns the results as a layer,
#' whereas \code{execSQL}, as a convenience, simply executes the SQL and discards
#' the result. Note that a layer could be simply a table of values without any
#' associated geometries.
#' 
#' @examples
#' f = system.file("example-data/tl_2013_us_state.shp", package = "rgdal2")
#' x = openOGR(f)
#' y = getSQLLayer(x, "select * from tl_2013_us_state where \'NAME\' = \"Texas\"")
#' show(y)
#' 
#' @family ogr-io
#' @rdname sql-layer
#' @export
getSQLLayer = function(x, sql)
{
    assertClass(x, "RGDAL2Datasource")
    lyr = RGDAL_ExecSQL(x@handle, sql)
    newRGDAL2SQLLayer(lyr, x, sql)
}

#' @rdname sql-layer
#' @export
execSQL = function(x, sql)
{
    assertClass(x, "RGDAL2Datasource")
    lyr = RGDAL_ExecSQL(x@handle, sql)
    OGR_DS_ReleaseResultSet(x@handle, lyr)
    invisible()
}

setMethod('show', 'RGDAL2SQLLayer',
function(object)
{
    ogrinfo = Sys.which('ogrinfo')
    dsname = OGR_DS_GetName(object@datasource@handle)
    if ( nchar(ogrinfo) > 0 )
    {
        info = pipe(paste(ogrinfo, '-so', dsname, '-sql \'', object@sql, '\''), 'rt')
        res = readLines(info); close(info)
        catLines(res)
        return(invisible(res))
    }
    else
    {
        cat('OGR layer at address ')
        print(object@handle)
        return(invisible(object))    
    }
})

#' Rewind OGR layer
#' 
#' Reset the geometry cursor to the beginning
#' 
#' @param x an OGR layer
#' 
#' @details
#' The function \code{\link{getNextGeometry}} will increment forward until
#' no geometries remain. To start with the first geometry, this function must
#' be called if the cursor is not already at the begining.
#' 
#' @family layer-io
#' @export
rewind = function(x)
{
    assertClass(x, "RGDAL2Layer")
    RGDAL_OGR_L_ResetReading(x@handle)
    invisible(x)
}

#' Fetch a feature using its identifier
#' 
#' @param x an OGR layer
#' @param fid the feature identifier
#' 
#' @details
#' Each "row" of a data layer is called a feature. Every feature is assigned a unique
#' identifier or "FID". This function uses the FID as a key into the layer. This function
#' does not return a geometry. Geometries must be extracted from a feature using
#' \code{\link{getGeometry}}.
#' 
#' @family layer-io
#' @export
getFeature = function(x, fid)
{
    assertClass(x, "RGDAL2Layer")
    feat = RGDAL_GetFeature(x@handle, fid)
    newRGDAL2Feature(feat, x)
}

#' Fetch the next feature
#' 
#' Returns the next feature in a layer
#' 
#' @param x an OGR layer
#' 
#' @details
#' Each layer holds an internal cursor. This function fetches the next
#' feature in a sequence and increments the cursor. It can be called
#' repeatedly to get all the features in a layer.
#' 
#' @examples
#' f = system.file("example-data/tl_2013_us_state.shp", package = "rgdal2")
#' x = openOGRLayer(f)
#' g = getNextGeometry(x)
#' show(g)
#' 
#' @family layer-io
#' @export
getNextFeature = function(x)
{
    assertClass(x, "RGDAL2Layer")
    feat = RGDAL_OGR_L_GetNextFeature(x@handle)
    newRGDAL2Feature(feat, x)
}

#' Fetch the next geometry
#' 
#' Returns the next geometry in a layer
#' 
#' @param x an OGR layer
#' 
#' @details
#' Each layer holds an internal cursor. This function fetches the next
#' geometry in a sequence and increments the cursor. It can be called
#' repeatedly to get all the geometries in a layer.
#' 
#' @examples
#' f = system.file("example-data/tl_2013_us_state.shp", package = "rgdal2")
#' x = openOGRLayer(f)
#' g = getNextGeometry(x)
#' show(g)
#' 
#' @family layer-io
#' @export
getNextGeometry = function(x)
{
    feat = getNextFeature(x)
    if ( is.null(feat) )
        stop("No more features; rewind layer?")
    getGeometry(feat)
}

getIDs = function(x)
{
    assertClass(x, "RGDAL2Layer")
    unclass(RGDAL_GetFIDs(x@handle))
}

lapplyFeatures = function(x, applyFun, ...)
{
    applyFun = match.fun(applyFun)
    ids = getIDs(x)
    res = lapply(ids, function(fid) applyFun(getFeature(x, fid), ...))
    names(res) = ids
    res
}

lapplyGeometries = function(x, applyFun, ...)
{
    applyFun = match.fun(applyFun)
    ids = getIDs(x)
    res = lapply(ids, function(fid) applyFun(getGeometry(getFeature(x, fid)), ...))
    names(res) = ids
    res
}

getGeometries = function(x)
{
    rewind(x)
    geomvec = RGDAL_GetGeometries(x@handle)
    res = lapply(1:length(geomvec), function(i)
    {
        newRGDAL2LayerGeometry(geomvec[[i]], x)
    })
    unlist(res)
}

featureIter = function(x, reset = TRUE)
{
    assertClass(x, "RGDAL2Layer")
    if ( reset ) rewind(x)
    f = function()
    {
        res = getNextFeature(x)
        if ( is.null(res) ) stop('StopIteration')
        res
    }
    structure(list(nextElem = f), class = c('rgdal2featureIter', 'abstractiter', 'iter'))
}

geometryIter = function(x, reset = TRUE)
{
    assertClass(x, "RGDAL2Layer")
    if ( reset ) rewind(x)
    f = function()
    {
        res = getNextFeature(x)
        if ( is.null(res) ) stop('StopIteration')
        getGeometry(res)
    }
    structure(list(nextElem = f), class = c('rgdal2geometryIter', 'abstractiter', 'iter'))
}

#' Dump a layer as a dataframe
#' 
#' Converts a layer to a dataframe.
#' 
#' @param from a data layer object
#' 
#' @details
#' A dataframe is constructed containing all the fields of the data layer.
#' A geometries are returned in the last column and FIDs in the first column.
#' The returned dataframe has a special print function to avoid printing all
#' of the geometries, which could be very large.
#'
#' @examples 
#' f = system.file("example-data/tl_2013_us_state.shp", package = "rgdal2")
#' x = openOGRLayer(f)
#' y = as(x, 'data.frame')
#' head(y)
#' 
#' @name as
#' @aliases as-data-frame
#' @family layer-io
#' @export
setAs('RGDAL2Layer', 'data.frame',
function(from)
{
    res = foreach(i = featureIter(from),
                  .init = data.frame(),
                  .combine = rbind) %do%
    {
        getFields(i)
    }
    res = cbind(getIDs(from), res)
    fidcolnam = RGDAL_OGR_L_GetFIDColumn(from@handle)
    names(res)[1] = ifelse(nchar(fidcolnam), fidcolname, "FID")
    geomfname = RGDAL_OGR_L_GetGeometryColumn(from@handle)
    if ( nchar(geomfname) == 0 ) geomfname = "GEOM"
    class(res) = c('RGDAL2LayerDF', class(res))
    res[[geomfname]] = getGeometries(from)
    rownames(res) = NULL
    res
})

#' @export
print.RGDAL2LayerDF = function(x, ...)
{
    i = which(sapply(x, function(a)
    {
        inherits(a[[1]], "RGDAL2Geometry")
    }))
    for ( ii in i )
    {
        x[[ii]] = sapply(x[[ii]], function(a)
        {
            RGDAL_OGR_G_GetGeometryType(a@handle)
        })
    }
    class(x) = class(x)[-1]
    print(x, ...)
    invisible(x)
}

#' Get the spatial extent of an object
#' 
#' Compute the spatial extent of an object and return it as a geometry
#' 
#' @param object a spatial object
#' 
#' @details
#' If the object has a spatial filter set, then the extent of the spatial
#' filter is returned. Otherwise, the extent of the object is returned.
#' 
#' @value a geometry object
#' 
#' @examples
#' f = system.file("example-data/tl_2013_us_state.shp", package = "rgdal2")
#' x = openOGRLayer(f)
#' extent(x)
#' 
#' @aliases extent
#' @rdname extent
#' @export
setMethod("extent", "RGDAL2Layer",
function(object)
{
    sf = getSpatialFilter(object)
    if ( !is.null(sf) ) return(extent(sf))
    res = RGDAL_GetLayerEnv(object@handle)
    res = newRGDAL2Geometry(res)
    setSRS(res, getSRS(object))
    res
})

setSpatialFilter = function(layer, geom)
{
    assertClass(layer, "RGDAL2Layer")
    assertClass(geom, "RGDAL2Geometry")
    if ( hasSRS(geom) )
        filt = reproject(geom, getSRS(layer))
    else
    {
        warning("Region geometry has null projection")
        filt = geom
    }
    OGR_L_SetSpatialFilter(layer@handle, filt@handle)
    invisible(layer)
}

getSpatialFilter = function(layer)
{
    assertClass(layer, "RGDAL2Layer")
    res = RGDAL_OGR_L_GetSpatialFilter(layer@handle)
    newRGDAL2Geometry(res)
}

setSelectWhere = function(layer, where)
{
    assertClass(layer, "RGDAL2Layer")
    if ( OGR_L_SetAttributeFilter(layer@handle, where) )
        stop("Error setting attribute filter")
    invisible(layer)
}

#
# Functions for features
#
setMethod("show", "RGDAL2Feature", function(object)
{
  file = tempfile()
  on.exit(unlink(file))
  RGDAL_PrintFeature(object@handle, file)
  res = readLines(file)
  catLines(res)
  return(invisible(res))
})

#' Get the fields of a feature
#' 
#' Return fields as R-native objects
#' 
#' @param x the feature
#' 
#' @return a list of field values
#' 
#' @examples
#' f = system.file("example-data/tl_2013_us_state.shp", package = "rgdal2")
#' x = openOGRLayer(f)
#' y = getNextFeature(x)
#' unlist(getFields(y))
#' 
#' @export
getFields = function(x)
{
    assertClass(x, "RGDAL2Feature")
    RGDAL_GetFields(x@handle)
}

getID = function(x)
{
    assertClass(x, "RGDAL2Feature")
    RGDAL_GetFID(x@handle)
}

#' Fetch a geometry from a feature
#' 
#' @param x the feature object
#' 
#' @return a geometry object
#' 
#' @export
getGeometry = function(x)
{
    stopifnot(inherits(x, "RGDAL2Feature"))
    geom = OGR_F_GetGeometryRef(x@handle)
    newRGDAL2LayerGeometry(geom, x@layer)
}

setMethod("getSRS", "RGDAL2Feature",
function(object)
{
    getSRS(object@layer)
})

#
# Functions for geometries
#
newGeometry = function(geomType, points = NULL, SRS = NULL)
{
    res = OGR_G_CreateGeometry(geomType)
    res = newRGDAL2Geometry(res)
    if ( ! is.null(points) )
        addPoints(res, points)
    if ( ! is.null(SRS) )
        setSRS(res, SRS)
    res
}

setMethod("show", "RGDAL2Geometry",
function(object)
{
  file = tempfile()
  on.exit(unlink(file))
  RGDAL_DumpGeometry(object@handle, file)
  res = readLines(file)
  catLines(res)
  return(invisible(res))
})

#Fixme: this should return count of geometries
setMethod('length', "RGDAL2Geometry", function(x)
{
    cdim = OGR_G_GetCoordinateDimension(x@handle)
    if ( cdim == 0 ) return(0L)
    length(unlist(getPoints(x))) / cdim
})

getPoints = function(x, collapse = FALSE)
{
    assertClass(x, "RGDAL2Geometry")
    res = RGDAL_GetPoints(x@handle)
    if ( collapse )
        collapsePointList(res)
    else res
}

addPoints = function(x, points)
{
    assertClass(x, "RGDAL2Geometry")
    switch(OGR_G_GetGeometryType(x@handle),
           wkbPoint = addPointsFromList(x, points),
           wkbLinearRing = addPointsFromList(x, points),
           wkbLineString = addPointsFromList(x, points),
           wkbPolygon = addRingsFromList(x, points),
           wkbPoint25D = addPointsFromList(x, points),
           wkbLineString25D = addPointsFromList(x, points),
           wkbPolygon25D = addRingsFromList(x, points),
           wkbMultiPoint = accumPointsFromList(x, points),
           wkbMultiPoint25D = accumPointsFromList(x, points),
           stop("Cannot add points to this geometry type"))
    invisible(x)
}

addGeometry = function(g1, g2)
{
    stopifnot(inherits(g1, "RGDAL2Geometry"))
    stopifnot(inherits(g2, "RGDAL2Geometry"))
    stopifnot(length(g2) != 0)
    if ( OGR_G_AddGeometry(g1@handle, g2@handle) )
        stop("Cannot add geometry")
    invisible(g1)
}

setMethod("[", "RGDAL2Geometry",
function(x, i)
{
    handle = OGR_G_GetGeometryRef(x@handle, i)
    res = newRGDAL2Geometry(handle) #Who owns handle?
    if ( !is.null(res) )
        setSRS(res, getSRS(x))
    res
})

#' @rdname extent
#' @export
setMethod("extent", "RGDAL2Geometry",
function(object)
{
    res = RGDAL_GetGeomEnv(object@handle)
    res = newRGDAL2Geometry(res)
    setSRS(res, getSRS(object))
    res

})

setMethod("properties", "RGDAL2Geometry",
function(object)
{
    list(is.empty = OGR_G_IsEmpty(x@handle) == 1,
         is.valid = OGR_G_IsValid(x@handle) == 1,
         is.simple = OGR_G_IsSimple(x@handle) == 1,
         is.ring = OGR_G_IsRing(x@handle) == 1)
})

#
# Spatial predicates and geometry functions
#

centroid = function(x)
{
    assertClass(x, "RGDAL2Geometry")
    res = newGeometry("wkbPoint")
    if ( OGR_G_Centroid(x@handle, res@handle) )
        stop("Error computing centroid")
    res
}

'%intersects%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    OGR_G_Intersects(lhs@handle, rhs@handle) == 1;
}

'%equals%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    OGR_G_Equals(lhs@handle, rhs@handle) == 1
}

'%disjoint%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    OGR_G_Disjoint(lhs@handle, rhs@handle) == 1
}

'%touches%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    OGR_G_Touches(lhs@handle, rhs@handle) == 1
}

'%crosses%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    OGR_G_Crosses(lhs@handle, rhs@handle) == 1
}

'%within%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    OGR_G_Within(lhs@handle, rhs@handle) == 1
}

'%contains%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    OGR_G_Contains(lhs@handle, rhs@handle) == 1
}

'%overlaps%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    OGR_G_Overlaps(lhs@handle, rhs@handle) == 1
}

'%intersection%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    x = OGR_G_Intersection(lhs@handle, rhs@handle)
    newRGDAL2Geometry(handle = x)
}

'%union%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    x = OGR_G_Union(lhs@handle, rhs@handle)
    newRGDAL2Geometry(x)
}

'%difference%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    x = OGR_G_Difference(lhs@handle, rhs@handle)
    newRGDAL2Geometry(x)
}

'%symdiff%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    x = OGR_G_SymDifference(lhs@handle, rhs@handle)
    newRGDAL2Geometry(x)
}

'%distance%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    OGR_G_Distance(lhs@handle, rhs@handle)
}

boundary = function(x)
{
    assertClass(x, "RGDAL2Geometry")
    res = OGR_G_Boundary(x@handle)
    newRGDAL2Geometry(res)
}

convexHull = function(x)
{
    assertClass(x, "RGDAL2Geometry")
    res = OGR_G_ConvexHull(x@handle)
    newRGDAL2Geometry(res)
}

unionCascaded = function(x)
{
    assertClass(x, "RGDAL2Geometry")
    res = OGR_G_UnionCascaded(x@handle)
    newRGDAL2Geometry(res)
}

lineLength = function(x)
{
    assertClass(x, "RGDAL2Geometry")
    OGR_G_Length(x@handle)
}

area = function(x)
{
    assertClass(x, "RGDAL2Geometry")
    OGR_G_Area(x@handle)
}

#
# Misc functions
#
setMethod("extent", "list",
function(object)
{
    if ( isPointList(object) )
    {
      xmin = min(object$x)
      xmax = max(object$x)
      ymin = min(object$x)
      ymax = max(object$x)
      res = newGeometry('wkbPolygon')
      addPoints(res, list(x = c(xmin, xmin, xmax, xmax),
                          y = c(ymin, ymax, ymax, ymin)))
      res = newRGDAL2Geometry(res)
      return(res)
    }
    else
      lapply(object, function(el) extent(el))
})

makeExtent = function(xmin = 0, xmax = 1, ymin = 0, ymax = 1, SRS = NULL)
{
    res = RGDAL_MakeExtent(xmin, xmax, ymin, ymax)
    res = newRGDAL2Geometry(res)
    if ( !is.null(SRS) ) setSRS(res, SRS)
    res
}

simplify = function(object, tolerance, preserve.topology = TRUE)
{
    assertClass(object, "RGDAL2Geometry")
    x = if ( preserve.topology )
            OGR_G_SimplifyPreserveTopology(object@handle, tolerance)
        else
            OGR_G_Simplify(object@handle, tolerance)
   res = newRGDAL2Geometry(x)
   setSRS(res, getSRS(object))
   res
}

polygonize = function(object)
{
    assertClass(object, "RGDAL2Geometry")
    x = OGR_G_Polygonize(object@handle)
    res = newRGDAL2Geometry(x)
    if ( !is.null(res) )
        setSRS(res, getSRS(object))
    res
}

hexGrid = function(object)
{
    e = extent(object)
    pts = getPoints(e)    
}








