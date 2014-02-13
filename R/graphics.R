#
# Copyright Timothy H. Keitt
#

#
# Grid stuff
#

#' Convert a geometry to a grob
#' 
#' Converts the geometry to a grid graphics object
#' 
#' @param object the geometry
#' @param units grid units to use
#' @param ... passed to grid functions
#' 
#' @details
#' Grid functions operate on grobs. This function generates grobs you can
#' use in creating grid plots. It is more convenient to use \code{\link{draw}}
#' as that will do the conversion and build the plot.
#' 
#' @examples
#' g = graticule()
#' gg = geometryGrob(g)
#' show(gg)
#' 
#' @export
geometryGrob = function(object, ..., units = "native")
{
    points = getPoints(object)
    if ( length(unlist(points)) == 0 ) return(grob())
    switch(RGDAL_OGR_G_GetGeometryType(object@handle),
           POINT = pointsGrob(points$x, points$y, default.units = units, ...),
           LINEARRING = polylineGrob(points$x, points$y, default.units = units, ...),
           LINESTRING = linesGrob(points$x, points$y, default.units = units, ...),
           MULTILINESTRING = multiLineGrob(points, default.units = units, ...),
           POLYGON = multiPolygonGrob(points, default.units = units, ...),
           MULTIPOLYGON = multiPolygonGrob(points, default.units = units, ...),
           POINT25D = pointsGrob(points$x, points$y, default.units = units, ...),
           LINESTRING25D = linesGrob(points$x, points$y, default.units = units, ...),
           MULTILINESTRING25D = multiLineGrob(points, default.units = units, ...),
           POLYGON25D = multiPolygonGrob(points, default.units = units, ...),
           MULTIPOLYGON25D = multiPolygonGrob(points, default.units = units, ...),
           MULTIPOINT = pointsGrob(points$x, points$y, default.units = units, ...),
           MULTIPOINT25D = pointsGrob(points$x, points$y, default.units = units, ...),
           stop("Conversion to grid object not implemented"))
}

#' Draw a spatial data object
#' 
#' Graphically displays the object using the \code{\link{grid}} package
#' 
#' @param object the object to draw
#' @param region a geometry setting the drawing region
#' @param overlay if false, clear the graphics device before plotting
#' @param recording if false, do not record on the graphics stack
#' 
#' @return the grid graphics object invisibly
#' 
#' @examples
#' f = system.file("example-data/bee.jpg", package = "rgdal2")
#' x = openGDAL(f)
#' draw(x)
#'   
#' @rdname draw
#' @export
setGeneric("draw",
function(object, ..., region = extent(object), overlay = FALSE, recording = TRUE)
{
    standardGeneric("draw")
})

#' @rdname draw
setMethod("draw", "RGDAL2Geometry",
function(object, ..., region = extent(object), overlay = FALSE, recording = TRUE)
{
    if ( ! overlay ) 
    {
        grid.newpage()
        pushViewport(viewport(width = 0.8, height = 0.8))
        setViewport(region)
    }
    grid.draw(geometryGrob(object, ...), recording = recording)
    invisible(object)
})

#' @rdname draw
setMethod("draw", "RGDAL2Layer",
function(object, ..., region = extent(object), overlay = FALSE, recording = TRUE)
{
    if ( ! overlay ) 
    {
        grid.newpage()
        pushViewport(viewport(width = 0.8, height = 0.8))
        setViewport(region)
    }
    draw.fun = function(x) draw(x, ..., overlay = TRUE, recording = recording)
    lapplyGeometries(object, draw.fun)
    invisible(object)
})

#' @param dpi raster image resolution
#' @rdname draw
setMethod("draw", "RGDAL2RasterBand",
function(object, ..., dpi = 100, region = extent(object), overlay = FALSE, recording = TRUE)
{
    if ( ! overlay ) 
    {
        grid.newpage()
        pushViewport(viewport(width = 0.8, height = 0.8))
        setViewport(region)
    }
    grid.draw(rasterBandGrob(object, dpi = dpi, region = region, ...), recording = recording)
    invisible(object)
})

#' @rdname draw
setMethod("draw", "RGDAL2Dataset",
function(object, ..., dpi = 100, region = extent(object), overlay = FALSE, recording = TRUE)
{
    if ( ! overlay ) 
    {
        grid.newpage()
        pushViewport(viewport(width = 0.8, height = 0.8))
        setViewport(region)
    }
    grid.draw(rasterDatasetGrob(object, dpi = dpi, region = region, ...), recording = recording)
    invisible(object)
})

#' Set the grid viewport
#' 
#' Uses the \code{\link{extent}} of a spatial object to
#' set the viewport.
#' 
#' @param object
#' @param recording if true, grid will record a copy of the grob
#' 
#' @export
setViewport = function(object, ..., recording = TRUE)
{
    pushViewport(extentViewport(object, ...), recording = recording)   
}

extentViewport = function(object, ...)
{
    points = getPoints(extent(object), collapse = TRUE)
    xscale = range(points$x); yscale = range(points$y)
    obj.asp = diff(yscale) / diff(xscale)
    vp.asp = current.viewport.aspect()
    width = 1; height = obj.asp / vp.asp
    width = width / max(width, height)
    height = height / max(width, height)
    viewport(width = unit(width, "npc"), height = unit(height, "npc"), 
             xscale = unit(xscale, "native"), yscale = unit(yscale, "native"), ...)
}

rasterBandGrob = function(object,
                          dpi = 100,
                          region = extent(object),
                          col = defaultRGDALPalette(),
                          by.rank = TRUE,
                          units = "native",
                          ...)
{
    object = checkBand(object)
    pts = getPoints(extent(region), collapse = TRUE)
    width = diff(range(pts$x)); height = diff(range(pts$y))
    if ( is.finite(dpi) ) 
    {
        dim.in = dim(object)
        dim.out = round(dpi * rev(current.viewport.size()))
        if ( any(dim.out > dim.in) ) dim.out = dim.in
        ii = 1L:dim.out[1]; jj = 1L:dim.out[2]
        rast = if ( by.rank ) scale01(asRanked(object[region, ii = ii, jj = jj]))
               else scale01(object[region, ii = ii, jj = jj])
    }
    else
    {
        rast = if ( by.rank ) scale01(asRanked(object[region]))
               else scale01(object[region])
    }
    if ( length(col) ) rast[] = col[1 + rast * (length(col) - 1)]
    rasterGrob(rast, width = width, height = height, default.units = units, ...)
}

rasterDatasetGrob = function(object,
                             dpi = 100,
                             bands = 1L:nband(object),
                             region = extent(object),
                             units = "native",
                             ...)
{
    object = checkDataset(object)
    if ( length(bands) != 3L )
        return(rasterBandGrob(getBand(object), ...))
    pts = getPoints(extent(region), collapse = TRUE)
    width = diff(range(pts$x)); height = diff(range(pts$y))
    has.alpha = getMaskFlags(getMask(object))$is.alpha
    nbands.out = ifelse(has.alpha, 4L, 3L)
    if ( is.finite(dpi) ) 
    {
        dim.in = dim(object)[1:2]
        dim.out = round(dpi * rev(current.viewport.size()))
        if ( any(dim.out > dim.in) ) dim.out = dim.in
        rast = array(dim = c(dim.out, nbands.out))
        ii = 1L:dim.out[1]; jj = 1L:dim.out[2]
        for ( i in seq(along = bands) )
        {
            b = getBand(object, bands[i])
            rast[,,i] = scale01(b[region, ii = ii, jj = jj])
        }
        if ( has.alpha )
        {
            rast[,, nbands.out] = scale01(getMask(object)[region, ii = ii, jj = jj])
        }
    }
    else
    {
        dim.out = dim(object)[1:2]
        rast = array(dim = c(dim.out, nbands.out))
        for ( i in seq(along = bands) )
        {
            b = getBand(object, bands[i])
            rast[,,i] = scale01(b[region])
        }
        if ( has.alpha )
        {
            rast[,, nbands.out] = scale01(getMask(object)[region])
        }
    }    
    rasterGrob(rast, width = width, height = height, default.units = units, ...)
}

#' Pick an extent object
#' 
#' Create an extent geometry by clicking on the plot area
#' 
#' @param object if not null, assing the output the object's spatial reference system
#' @param plot.it if true, overlay the extent on the current plot
#' 
#' @export
pickExtent = function(object = NULL, plot.it = TRUE)
{
    p1 = unlist(grid.locator())
    grid.points(p1[1], p1[2])
    p2 = unlist(grid.locator())
    grid.points(p2[1], p2[2])
    res = makeExtent(min(p1[1], p2[1]), max(p1[1], p2[1]),
                     min(p1[2], p2[2]), max(p1[2], p2[2]))
    if ( !is.null(object) ) setSRS(res, getSRS(object))
    if ( plot.it ) draw(res, overlay = T, gp = gpar(lty = 2, lwd = 2))
    res
}

current.viewport.size = function(units = "inches")
{
    vp = current.viewport()
    vi = convertUnit(vp$height, units)
    wi = convertUnit(vp$width, units)
    c(width = wi, height = vi)
}

current.viewport.aspect = function()
{
    sz = current.viewport.size()
    unclass(sz[2]) / unclass(sz[1])
}

#' Create a graticule
#' 
#' Returns a geometry object containing a graticule
#' 
#' @param dlat the latitude interval in degrees
#' @param dlon the longitude interval in degrees
#' @param linc the line-segment increment in degrees
#' 
#' @examples
#' f = system.file("example-data/continents", package = "rgdal2")
#' x = openOGRLayer(f)
#' draw(graticule())
#' draw(x, gp = gpar(fill = "lightblue"), overlay = TRUE)
#' 
#' @export
graticule = function(dlat = 10, dlon = 10, linc = 1)
{
    latlon = newSRS("EPSG:4326")
    g = newGeometry('MULTILINESTRING', SRS = latlon)
    for ( lon in seq(-180, 180, by = dlon) )
    {
        pts = list(x = rep(lon, 2), y = c(-90, 90))
        gg = newGeometry('LINESTRING', pts, latlon)
        RGDAL_OGR_G_Segmentize(gg@handle, linc)
        addGeometry(g, gg)
    }
    for ( lat in seq(-90, 90, by = dlat) )
    {
        pts = list(x = c(-180, 180), y = rep(lat, 2))
        gg = newGeometry('LINESTRING', pts, latlon)
        RGDAL_OGR_G_Segmentize(gg@handle, linc)
        addGeometry(g, gg)
    }
    return(g)
}



