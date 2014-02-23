#
# Copyright Timothy H. Keitt
#

newRGDAL2Datasource = function(handle)
{
    reg.finalizer(handle, RGDAL_OGRReleaseDataSource, TRUE)
    new("RGDAL2Datasource", handle = handle)
}

newRGDAL2Layer = function(handle, datasource)
{
    new("RGDAL2Layer", handle = handle, datasource = datasource)
}

newRGDAL2SQLLayer = function(handle, datasource, sql)
{
    if ( is.null(handle) ) return(NULL)
    f = function(lyrExtPtr)
      RGDAL_DS_ReleaseResultSet(datasource@handle, lyrExtPtr)
    reg.finalizer(handle, f, TRUE)
    new("RGDAL2SQLLayer", handle = handle, datasource = datasource, sql = sql)
}

newRGDAL2Feature = function(handle, layer)
{
    if ( is.null(handle) )
      NULL
    else
      new("RGDAL2Feature", handle = handle, layer = layer)
}

newRGDAL2Geometry = function(handle)
{
    if ( is.null(handle) ) return(NULL)
    reg.finalizer(handle, function(x) RGDAL_G_DestroyGeometry(x))
    new("RGDAL2Geometry", handle = handle)
}

newRGDAL2LayerGeometry = function(handle, layer)
{
    new("RGDAL2LayerGeometry", handle = handle, layer = layer)
}

getLayer = function(x, layer = 1L)
{
    assertClass(x, "RGDAL2Datasource")
    lyr = if ( is.character(layer) )
            RGDAL_DS_GetLayerByName(x@handle, layer)
          else
            RGDAL_DS_GetLayer(x@handle, layer - 1)
    newRGDAL2Layer(lyr, x)
}

setGeneric("testCapability", function(object, capability)
{
    standardGeneric("testCapability")
})

setMethod("testCapability", "RGDAL2Datasource",
          function(object, capability = c('create.layer',
                                          'delete.layer',
                                          'create.geom.field'))
{
    capability = switch(match.arg(capability),
                        create.layer = 'ODsCCreateLayer',
                        delete.layer = 'ODsCDeleteLayer',
                        create.geom.field = 'ODsCCreateGeomFieldAfterCreateLayer')
    RGDAL_DS_TestCapability(object@handle, capability) == 1;
})

setMethod("testCapability", "RGDAL2Layer",
          function(object,
                   capability = c('random.read',
                                  'sequential.write',
                                  'random.write',
                                  'fast.spatial.filter',
                                  'fast.feature.count',
                                  'fast.get.extent',
                                  'fast.set.next.by.index',
                                  'create.field',
                                  'create.geom.field',
                                  'delete.field',
                                  'reorder.fields',
                                  'alter.field.defn',
                                  'delete.feature',
                                  'transactions'))
{
    capability = switch(match.arg(capability),
                        random.read = "RandomRead",
                        sequential.write = "SequentialWrite",
                        random.write = "RandomWrite",
                        fast.spatial.filter = "FastSpatialFilter",
                        fast.feature.count = "FastFeatureCount",
                        fast.get.extent = "FastGetExtent",
                        fast.set.next.by.index = "FastSetNextByIndex",
                        create.field = "CreateField",
                        create.geom.field = "CreateGeomField",
                        delete.field = "DeleteField",
                        reorder.fields = "ReorderFields",
                        alter.field.defn = "AlterFieldDefn",
                        delete.feature = "DeleteFeature",
                        transactions = "Transactions")
    RGDAL_L_TestCapability(object@handle, capability) == 1;
})

drawPolygonGeom = function(x, ...)
{
    coords = getPoints(x)
    if ( is.null(coords$x) )
        lapply(coords, function(y)
        {
            polygon(y, ...)    
        })
    else
        polygon(coords, ...)
    invisible(coords)
}

get.plot.fun = function(geom)
{
    switch(RGDAL_G_GetGeometryType(geom@handle),
           POINT = points,
           MULTIPOINT = points,
           LINESTRING = lines,
           MULTILINESTRING = lines,
           POLYGON = drawPolygonGeom,
           MULTIPOLYGON = drawPolygonGeom,
           lines)
}

plotGeom = function(x, ...)
{
    coords = getPoints(x)
    if ( is.null(coords$x) )
        lapply(coords, function(y)
        {
            plot(y, ...)    
        })
    else
        plot(coords, ...)
}

initPlot = function(x)
{
    plotGeom(x, asp = 1, type = 'n', axes = FALSE,
             xlab = NA, ylab = NA, xaxs = "i", yaxs = "i")
}

getProj4FromAlias = function(alias)
{
    aliases = c("WGS84", "NAD83", "USNatAtl", "NALCC",
                "NAAEAC", "Robinson", "Mollweide", "GRS80")
    i = pmatch(alias, aliases)
    if ( is.na(i) ) return(alias)
    switch(aliases[i],
           WGS84 = "+proj=longlat +datum=WGS84 +no_defs",
           NAD83 = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs",
           USNatAtl = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
           NALCC = "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
           NAAEAC = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
           Robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
           Mollweide = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
           GRS80 = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs",
           alias)
}

isPointList = function(x)
    is2DPointList(x) || is3DPointList(x)

is2DPointList = function(x)
{
    all(c("x", "y") %in% names(x)) && (!("z" %in% names(x)))
}

is3DPointList = function(x)
{
    all(c("x", "y", "z") %in% names(x))
}

addPointsFromList = function(x, points)
{
    if ( is2DPointList(points) )
    {
        xx = as.double(points$x)
        yy = as.double(points$y)
        lapply(seq(along = xx), function(i)
        {
            RGDAL_G_AddPoint_2D(x@handle, xx[i], yy[i])
        })
        return(invisible(x))
    }
    if ( is3DPointList(points) )
    {
        xx = as.double(points$x)
        yy = as.double(points$y)
        zz = as.double(points$z)
        lapply(seq(along = xx), function(i)
        {
            OGR_G_AddPoint(x@handle, xx[i], yy[i], zz[i])
        })
        return(invisible(x))
    }
    stop("Invalid points list")
}

addRingToPolygon = function(x, points)
{
    ring = newGeometry('LINEARRING')
    addPointsFromList(ring, points)
    if ( RGDAL_G_AddGeometry(x@handle, ring@handle) )
        stop("Error adding points")
    RGDAL_G_CloseRings(x@handle)
    invisible(x)
}

addRingsFromList = function(x, points)
{
    if ( isPointList(points) )
    {
        addRingToPolygon(x, points)
    }
    else
    {
        if ( is.list(points) )
            lapply(points, function(pts) addRingFromList(x, pts))
        else
            stop("Invalid points list")
    }
    invisible(x)
}

accumPointsFromList = function(x, points)
{
    if ( isPointList(points) )
    {
        addPointToMultiPoint(x, points)
    }
    else
    {
        if ( is.list(points) )
            lapply(points, function(pts) accumPointsFromList(x, pts))
        else
            stop("Invalid points list")
    }
    invisible(x)
}

addPointToMultiPoint = function(x, points)
{
    pt = newGeometry("wkbPoint")
    if ( is3DPointList(points) )
        for ( i in 1:length(points$x) )
        {
            addPoints(pt, list(x = points$x[i],
                               y = points$y[i],
                               z = points$z[i]))
            addGeometry(x, pt)
        }
    else
        for ( i in 1:length(points$x) )
        {
            addPoints(pt, list(x = points$x[i],
                               y = points$y[i]))
            addGeometry(x, pt)
        }
    OGR_G_DestroyGeometry(pt@handle)
    invisible(x)
}
    
multiPolygonGrob = function(x, ...)
{
    if ( isPointList(x) )
    {
        polygonGrob(x$x, x$y, ...)
    }
    else
    {
        id.lens = getPointListLengths(x)
        x = collapsePointList(x)
        polygonGrob(x$x, x$y, id.lengths = id.lens, ...)
    }
}

multiLineGrob = function(x, ...)
{
    if ( isPointList(x) )
    {
        linesGrob(x$x, x$y, ...)
    }
    else
    {
        id.lens = getPointListLengths(x)
        x = collapsePointList(x)
        polylineGrob(x$x, x$y, id.lengths = id.lens, ...)
    }
}

getPointListLengths = function(x)
{
    unlist(lapply(1:length(x),
           function(i)
           {
                length(x[[i]][[1]])
           }))
}

collapsePointList = function(x)
{
    if ( isPointList(x) ) return(x)
    list(x = unlist(lapply(x, function(a) a$x)),   
         y = unlist(lapply(x, function(a) a$y)))
}

getPointsRange = function(x)
{
    list(x.min = min(x$x), x.max = max(x$x),
         y.min = min(x$y), y.max = max(x$y))
}






