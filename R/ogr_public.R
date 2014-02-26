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
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGR(f)
#' show(x)
#' 
#' @export
openOGR = function(fname, readonly = TRUE)
{
    x = RGDAL_OGROpen(fname, readonly)
    if ( is.null(x) ) stop("Unable to open file")
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
#' @examples
#' x = newOGRDatasource()
#' show(x)
#' 
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
    dsname = RGDAL_DS_GetName(object@handle)
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
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGR(f)
#' length(x)
#' names(x)
#' class(x[[1]])
#' class(x[1])
#' class(x$continent)
#' 
#' @aliases datasource
#' @rdname data-source-methods
#' @export
setMethod('length', "RGDAL2Datasource",
function(x)
{
    RGDAL_DS_GetLayerCount(x@handle)
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

setMethod("properties",
signature("RGDAL2Datasource"),
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
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGRLayer(f)
#' show(x)
#' draw(x, gp = gpar(fill = "lightblue"))
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
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGRLayer(f)
#' getLayerName(x)
#' 
#' @family layer-io
#' @export
getLayerName = function(x)
{
    assertClass(x, "RGDAL2Layer")
    RGDAL_L_GetName(x@handle)
}

setMethod('show',
signature('RGDAL2Layer'),
function(object)
{
    ogrinfo = Sys.which('ogrinfo')
    dsname = RGDAL_DS_GetName(object@datasource@handle)
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
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGRLayer(f)
#' dim(x)
#' length(x)
#' names(x)
#' 
#' @family layer-io
#' @aliases vector-layer
#' @rdname layer-methods
#' @export
setMethod('dim',
signature('RGDAL2Layer'),
function(x)
{
  rows = RGDAL_L_GetFeatureCount(x@handle)
  cols = GetLayerFieldCount(x@handle)
  c(rows, cols)
})

#' @rdname layer-methods
#' @export
setMethod('length',
signature('RGDAL2Layer'),
function(x)
{
  GetLayerFieldCount(x@handle)
})

#' @rdname layer-methods
#' @export
setMethod('names',
signature('RGDAL2Layer'),
function(x)
{
  GetFieldNames(x@handle)
})

#' Fetch properties of an object
#' 
#' Interrogate properties of objects
#' 
#' @param object the object
#' 
#' @return a list of properties
#' 
#' @examples
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGRLayer(f)
#' properties(x)
#' 
#' @aliases properties
#' @rdname properties
#' @export
setMethod("properties",
signature("RGDAL2Layer"),
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
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGR(f)
#' y = getSQLLayer(x, "select * from continent where \'CONTINENT\' = \"Australia\"")
#' show(y)
#' draw(y, region = getNextGeometry(y))
#' 
#' execSQL(x, "select * from continent")  #only for side-effect
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
    RGDAL_DS_ReleaseResultSet(x@handle, lyr)
    invisible(x)
}

setMethod('show',
signature('RGDAL2SQLLayer'),
function(object)
{
    ogrinfo = Sys.which('ogrinfo')
    dsname = RGDAL_DS_GetName(object@datasource@handle)
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
    RGDAL_L_ResetReading(x@handle)
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
#' @examples
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGRLayer(f)
#' ids = getIDs(x)
#' f = getFeature(x, ids[[1]])
#' show(f)
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
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGRLayer(f)
#' g = getNextGeometry(x)
#' show(g)
#' 
#' @family layer-io
#' @export
getNextFeature = function(x)
{
    assertClass(x, "RGDAL2Layer")
    feat = RGDAL_L_GetNextFeature(x@handle)
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
#' f = system.file("example-data/continents", package = "rgdal2")
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

#' Fetch feature IDs
#' 
#' Return a list of feature IDs
#' 
#' @param x a layer object
#' 
#' @details
#' Every row of a layer has a unique feature id. This function returns them.
#' Usually this will be a sequence of integers. There is no requirement that
#' they be contiguous.
#' 
#' @examples
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGRLayer(f)
#' getIDs(x)
#' 
#' @export
getIDs = function(x)
{
    assertClass(x, "RGDAL2Layer")
    unclass(RGDAL_GetFIDs(x@handle))
}

#' OGR apply-like functions
#' 
#' Apply a function to all features  or geometries in a layer
#' 
#' @parm x a data layer
#' @parm applyFun a function or string
#' 
#' @details
#' The call signature for is \code{function(feature, ...)} or
#' \code{function(geometry, ...)}.
#' 
#' @examples
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGRLayer(f)
#' invisible(lapplyFeatures(x, function(a) show(a)))
#' invisible(lapplyGeometries(x, function(a) show(a)))
#' 
#' @rdname applyfuncs
#' @export
lapplyFeatures = function(x, applyFun, ...)
{
    applyFun = match.fun(applyFun)
    ids = getIDs(x)
    res = lapply(ids, function(fid) applyFun(getFeature(x, fid), ...))
    names(res) = ids
    res
}

#' @rdname applyfuncs
#' @export
lapplyGeometries = function(x, applyFun, ...)
{
    applyFun = match.fun(applyFun)
    ids = getIDs(x)
    res = lapply(ids, function(fid) applyFun(getGeometry(getFeature(x, fid)), ...))
    names(res) = ids
    res
}

#' Fetch all geometries from a layer
#' 
#' Return a vector of geometries
#' 
#' @param x a layer object
#' 
#' @details
#' The layer will be \code{\link{rewound}} and all geometries
#' extracted. After this function is called, the internal feature
#' cursor will be past the end of the feature list. You must call
#' \code{\link{rewind}} again before fetching features from the
#' layer.
#' 
#' @examples
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGRLayer(f)
#' y = getGeometries(x)
#' draw(extent(x))
#' invisible(lapply(y,
#' function(g)
#' {
#'  draw(extent(g), overlay = TRUE)
#' }))
#' 
#' @export
getGeometries = function(x)
{
    rewind(x)
    geomvec = RGDAL_GetGeometries(x@handle)
    res = lapply(1:length(geomvec), function(i)
    {
        newRGDAL2Geometry(geomvec[[i]], x)
    })
    unlist(res)
}

#' Create feature iterator
#' 
#' Generate a feature iterator for use with \code{\link{foreach}}.
#' 
#' @param x a data layer
#' @param reset if true, \code{\link{rewind}} the layer
#' 
#' @details
#' Combined with \code{\link{foreach}} and \code{\link{do}} the
#' returned object can be used to iterate over all features in a
#' data layer.
#' 
#' @export
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

#' Create geometry iterator
#' 
#' Generate a geometry iterator for use with \code{\link{foreach}}.
#' 
#' @param x a data layer
#' @param reset if true, \code{\link{rewind}} the layer
#' 
#' @details
#' Combined with \code{\link{foreach}} and \code{\link{do}} the
#' returned object can be used to iterate over all geometries in a
#' data layer.
#' 
#' @export
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
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGRLayer(f)
#' y = as(x, 'data.frame')
#' head(y)
#'
#' @name as
#' @aliases as-data-frame
#' @family layer-io
#' @export
setAs('RGDAL2Layer',
signature('data.frame'),
function(from)
{
    res = foreach(i = featureIter(from),
                  .init = data.frame(),
                  .combine = rbind) %do%
    {
        getFields(i)
    }
    res = cbind(getIDs(from), res)
    fidcolnam = RGDAL_L_GetFIDColumn(from@handle)
    names(res)[1] = ifelse(nchar(fidcolnam), fidcolname, "FID")
    geomfname = RGDAL_L_GetGeometryColumn(from@handle)
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
            RGDAL_G_GetGeometryType(a@handle)
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
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGRLayer(f)
#' extent(x)
#' 
#' @aliases extent
#' @rdname extent
#' @export
setMethod("extent", 
signature("RGDAL2Layer"),
function(object)
{
    sf = getSpatialFilter(object)
    if ( !is.null(sf) ) return(extent(sf))
    res = RGDAL_GetLayerEnv(object@handle)
    res = newRGDAL2Geometry(res)
    setSRS(res, getSRS(object))
    res
})

#' Set a spatial filter on a data layer
#' 
#' Constrains the data layer by the geometry
#' 
#' @param layer a data layer
#' @param geom a geometry
#' 
#' @details
#' This should in principle cause \code{\link{getGeometries}} to only
#' return those that overlap the filter geometry. Its not clear to me
#' that this actually works.
#' 
#' @rdname spatial-filter
#' @export
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
    RGDAL_L_SetSpatialFilter(layer@handle, filt@handle)
    invisible(layer)
}

#' @rdname spatial-filter
#' @export
getSpatialFilter = function(layer)
{
    assertClass(layer, "RGDAL2Layer")
    res = RGDAL_L_GetSpatialFilter(layer@handle)
    newRGDAL2Geometry(res)
}

#' @param where an SQL where statement
#' @rdname spatial-filter
#' @export
setSelectWhere = function(layer, where)
{
    assertClass(layer, "RGDAL2Layer")
    if ( RGDAL_L_SetAttributeFilter(layer@handle, where) )
        stop("Error setting attribute filter")
    invisible(layer)
}

#
# Functions for features
#
setMethod("show",
signature("RGDAL2Feature"),
function(object)
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
#' f = system.file("example-data/continents", package = "rgdal2")
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

#' Get a feture ID
#' 
#' Returns the ID of a feature
#' 
#' @param x the feature object
#' 
#' @export
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
    assertClass(x, "RGDAL2Feature")
    geom = RGDAL_F_GetGeometryRef(x@handle)
    newRGDAL2Geometry(geom, x@layer)
}

#' Get the spatial reference system from a feature
#' 
#' @param object the object holding the SRS
#' 
#' @rdname srs
#' @export
setMethod("getSRS",
signature("RGDAL2Feature"),
function(object)
{
    getSRS(object@layer)
})

#
# Functions for geometries
#

#' Create a geometry
#' 
#' Creates a geometry object optionally setting the spatial reference system
#' 
#' @param geomType the name of the geometry type
#' @param points an x, y point list
#' @param SRS a spatial reference system object
#' 
#' @examples
#' newGeometry("POINT", list(x = rnorm(3), y = rnorm(3)))
#' 
#' @export
newGeometry = function(geomType, points = NULL, SRS = NULL)
{
    res = RGDAL_G_CreateGeometry(geomType)
    res = newRGDAL2Geometry(res)
    if ( ! is.null(points) )
        addPoints(res, points)
    if ( ! is.null(SRS) )
        setSRS(res, SRS)
    res
}

setMethod("show",
signature("RGDAL2Geometry"),
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
    cdim = RGDAL_G_GetCoordinateDimension(x@handle)
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
    switch(RGDAL_G_GetGeometryType(x@handle),
           POINT = addPointsFromList(x, points),
           LINEARRING = addPointsFromList(x, points),
           LINESTRING = addPointsFromList(x, points),
           POLYGON = addRingsFromList(x, points),
           POINT25D = addPointsFromList(x, points),
           LINESTRING25D = addPointsFromList(x, points),
           POLYGON25D = addRingsFromList(x, points),
           MULTIPOINT = accumPointsFromList(x, points),
           MULTIPOINT25D = accumPointsFromList(x, points),
           stop("Cannot add points to this geometry type"))
    invisible(x)
}

addGeometry = function(g1, g2)
{
    stopifnot(inherits(g1, "RGDAL2Geometry"))
    stopifnot(inherits(g2, "RGDAL2Geometry"))
    stopifnot(length(g2) != 0)
    if ( RGDAL_G_AddGeometry(g1@handle, g2@handle) )
        stop("Cannot add geometry")
    invisible(g1)
}

setMethod("[", "RGDAL2Geometry",
function(x, i)
{
    handle = RGDAL_G_GetGeometryRef(x@handle, i)
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
    list(is.empty = RGDAL_G_IsEmpty(x@handle) == 1,
         is.valid = RGDAL_G_IsValid(x@handle) == 1,
         is.simple = RGDAL_G_IsSimple(x@handle) == 1,
         is.ring = RGDAL_G_IsRing(x@handle) == 1)
})

#
# Spatial predicates and geometry functions
#

#' Binary geometry functions
#' 
#' Infix operators for binary geometry functions
#' 
#' @details
#' These mirror the equivalent functions in OGR. See
#' \url{http://www.gdal.org/} for more information.
#' 
#' @param lhs a geometry object
#' @param rhs a geometry object
#' 
#' @examples
#' x = c(0, 0, 1, 1)
#' y = c(0, 1, 1, 0)
#' x1 = newGeometry("POLYGON", list(x = x, y = y))
#' x2 = newGeometry("POLYGON", list(x = x + 0.5, y = y + 0.5))
#' 
#' x1 %intersects% x2
#' x1 %equals% x2
#' x1 %disjoint% x2
#' x1 %touches% x2
#' x1 %crosses% x2
#' x1 %within% x2
#' x1 %contains% x2
#' x1 %overlaps% x2
#' 
#' x1 %distance% x2 
#' 
#' gp1 = gpar(lwd = 6, lty = 2, col = rgb(1, 0, 0, 0.5), fill = NA)
#' gp2 = gpar(lwd = 6, lty = 2, col = rgb(0, 1, 0, 0.5), fill = NA)
#' gp3 = gpar(lwd = 6, col = rgb(0, 0, 1, 0.5), fill = NA)
#' roi = extent(x1 %union% x2)
#' 
#' x3 = x1 %union% x2
#' draw(x1, region = roi, gp = gp1)
#' draw(x2, gp = gp2, overlay = TRUE)
#' draw(x3,  gp = gp3, overlay = TRUE)
#' grid.text("union")
#' 
#' x3 = x1 %difference% x2
#' draw(x1, region = roi, gp = gp1)
#' draw(x2, gp = gp2, overlay = TRUE)
#' draw(x3,  gp = gp3, overlay = TRUE)
#' grid.text("difference")
#' 
#' x3 = x1 %symdiff% x2
#' draw(x1, region = roi, gp = gp1)
#' draw(x2, gp = gp2, overlay = TRUE)
#' draw(x3,  gp = gp3, overlay = TRUE)
#' grid.text("symmetric difference")
#' 
#' @aliases geometry-binary-ops
#' @rdname geometry-binary
#' @export
'%intersects%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    RGDAL_G_Intersects(lhs@handle, rhs@handle) == 1;
}

#' @rdname geometry-binary
#' @export
'%equals%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    RGDAL_G_Equals(lhs@handle, rhs@handle) == 1
}

#' @rdname geometry-binary
#' @export
'%disjoint%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    RGDAL_G_Disjoint(lhs@handle, rhs@handle) == 1
}

#' @rdname geometry-binary
#' @export
'%touches%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    RGDAL_G_Touches(lhs@handle, rhs@handle) == 1
}

#' @rdname geometry-binary
#' @export
'%crosses%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    RGDAL_G_Crosses(lhs@handle, rhs@handle) == 1
}

#' @rdname geometry-binary
#' @export
'%within%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    RGDAL_G_Within(lhs@handle, rhs@handle) == 1
}

#' @rdname geometry-binary
#' @export
'%contains%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    RGDAL_G_Contains(lhs@handle, rhs@handle) == 1
}

#' @rdname geometry-binary
#' @export
'%overlaps%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    RGDAL_G_Overlaps(lhs@handle, rhs@handle) == 1
}

#' @rdname geometry-binary
#' @export
'%intersection%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    x = RGDAL_G_Intersection(lhs@handle, rhs@handle)
    newRGDAL2Geometry(handle = x)
}

#' @rdname geometry-binary
#' @export
'%union%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    x = RGDAL_G_Union(lhs@handle, rhs@handle)
    newRGDAL2Geometry(x)
}

#' @rdname geometry-binary
#' @export
'%difference%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    x = RGDAL_G_Difference(lhs@handle, rhs@handle)
    newRGDAL2Geometry(x)
}

#' @rdname geometry-binary
#' @export
'%symdiff%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    x = RGDAL_G_SymDifference(lhs@handle, rhs@handle)
    newRGDAL2Geometry(x)
}

#' @rdname geometry-binary
#' @export
'%distance%' = function(lhs, rhs)
{
    assertClass(lhs, 'RGDAL2Geometry')
    assertClass(rhs, 'RGDAL2Geometry')
    RGDAL_G_Distance(lhs@handle, rhs@handle)
}

#' Unary geometry functions
#' 
#' These function operate on geometries.
#' 
#' @param object a geometry object
#' 
#' @details
#' These mirror the equivalent functions in OGR. See
#' \url{http://www.gdal.org/} for more information.
#' 
#' @examples
#' x = newGeometry("MULTIPOINT", list(x = rnorm(100), y = rnorm(100)))
#' y = convexHull(x)
#' draw(y); draw(x, overlay = T)
#' centroid(y) 
#' boundary(y)
#' unionCascaded(y)
#' lineLength(newGeometry("LINESTRING", list(x = c(0, 1), y = c(0, 1))))
#' area(y)
#' simplify(y, 0.25)
#' polygonize(x)
#' 
#' @aliases geometry-unary-ops
#' @rdname geometry-unary
#' @export
centroid = function(object)
{
  assertClass(object, "RGDAL2Geometry")
  res = newGeometry("POINT", SRS = getSRS(object))
  if ( RGDAL_G_Centroid(object@handle, res@handle) )
    stop("Error computing centroid")
  res
}

#' @rdname geometry-unary
#' @export
boundary = function(object)
{
    assertClass(object, "RGDAL2Geometry")
    h = RGDAL_G_Boundary(object@handle)
    res = newRGDAL2Geometry(h)
    setSRS(res, getSRS(object))
    res
}

#' @rdname geometry-unary
#' @export
convexHull = function(object)
{
    assertClass(object, "RGDAL2Geometry")
    h = RGDAL_G_ConvexHull(object@handle)
    res = newRGDAL2Geometry(h)
    setSRS(res, getSRS(object))
    res
}

#' @rdname geometry-unary
#' @export
unionCascaded = function(object)
{
    assertClass(object, "RGDAL2Geometry")
    mp = RGDAL_G_ForceToMultiPolygon(object@handle)
    h = RGDAL_G_UnionCascaded(mp)
    res = newRGDAL2Geometry(h)
    setSRS(res, getSRS(object))
    RGDAL_G_DestroyGeometry(mp)
    res
}

#' @rdname geometry-unary
#' @export
lineLength = function(object)
{
    assertClass(object, "RGDAL2Geometry")
    RGDAL_G_Length(object@handle)
}

#' @rdname geometry-unary
#' @export
area = function(object)
{
    assertClass(object, "RGDAL2Geometry")
    RGDAL_G_Area(object@handle)
}

#' @param tolerance larger values increase simplification
#' @param perserve.topology if true, attempt to not invalidate the geometry
#' @rdname geometry-unary
#' @export
simplify = function(object, tolerance, preserve.topology = TRUE)
{
  assertClass(object, "RGDAL2Geometry")
  h = if ( preserve.topology )
    RGDAL_G_SimplifyPreserveTopology(object@handle, tolerance)
  else
    RGDAL_G_Simplify(object@handle, tolerance)
   x = newRGDAL2Geometry(h)
   setSRS(x, getSRS(object))
   x
}

#' @rdname geometry-unary
#' @export
polygonize = function(object)
{
    assertClass(object, "RGDAL2Geometry")
    h = RGDAL_G_Polygonize(object@handle)
    x = newRGDAL2Geometry(h)
    setSRS(x, getSRS(object))
    x
}

#' Generate an extent object
#' 
#' Create an extent based on input boundaries
#' 
#' @param xmin left side
#' @param xmas right side
#' @param ymin bottom
#' @param ymax top
#' @param SRS a spatial reference system
#' 
#' @return a rectanglular polygon geometry
#' 
#' @examples
#' show(makeExtent())
#' 
#' @export
makeExtent = function(xmin = 0, xmax = 1, ymin = 0, ymax = 1, SRS = NULL)
{
  env = RGDAL_MakeExtent(xmin, xmax, ymin, ymax)
  res = newRGDAL2Geometry(env)
  if ( !is.null(SRS) ) setSRS(res, SRS)
  res
}

#' @export
setMethod("extent",
signature("list"),
function(object)
{
  if ( isPointList(object) )
  {
    xmin = min(object$x)
    xmax = max(object$x)
    ymin = min(object$y)
    ymax = max(object$y)
    res = newGeometry('POLYGON')
    addPoints(res, list(x = c(xmin, xmin, xmax, xmax),
                        y = c(ymin, ymax, ymax, ymin)))
    res = newRGDAL2Geometry(res)
    setSRS(res, getSRS(object))
    return(res)
  }
  else
    lapply(object, function(el) extent(el))
})

hexGrid = function(object)
{
    e = extent(object)
    pts = getPoints(e)    
}








