#
# Copyright Timothy H. Keitt
#

openGDAL = function(fname, readonly = TRUE, shared = TRUE)
{
  x = GDALOpen(fname, readonly, shared)
  newRGDAL2Dataset(x)
}

openGDALBand = function(fname, band = 1L, readonly = TRUE)
{
  getBand(openGDAL(fname, readonly), band)
}

newGDALDataset = function(nrow, ncol, nbands = 1,
                          dataType = 'GDT_Float64', driver = 'MEM',
                          file = tempfile())
{
  x = RGDALCreateDataset(driver, file, nrow, ncol, nbands, dataType)
  if ( driver != 'MEM' )
  {
    GDALClose(x) #This seems to improve reliability
    x = GDALOpen(file, 'GA_Update')
  }
  else unlink(file)
  newRGDAL2Dataset(x)
}

copyDataset = function(x, file = tempfile(), driver = "MEM")
{
    assertClass(x, "RGDAL2Dataset")
    handle = RGDAL_CreateCopy(x@handle, file, driver)
    newRGDAL2Dataset(handle)
}

getTransform = function(object)
{
    object = checkDataset(object)
    res = RGDAL_GetGeoTransform(object@handle)
    list(transl = c(res[c(1, 4)]),
         affmat = matrix(res[c(2, 3, 5, 6)], 2))
}

setTransform = function(object, transform)
{
    object = checkDataset(object)
    with(transform,
    {
        gt = (c(transl[1], affmat[,1],
                transl[2], affmat[,2]))
        if ( RGDAL_SetGeoTransform(object@handle, gt) )
            error("Unable to set geo transform")
    })
    invisible(object)
}

copyTransform = function(obj1, obj2)
{
    obj1 = checkDataset(obj1)
    obj2 = checkDataset(obj2)
    setTransform(obj2, getTransform(obj1))
    invisible(obj2)
}

newGDALBand = function(nrow, ncol, dataType = 'GDT_Float64',
                       driver = 'MEM', file = tempfile())
{
    getBand(newGDALDataset(nrow, ncol, 1L, dataType, driver, file))
}

makeTestDataset = function()
{
    x = RGDALMakeTestDataset();
    newRGDAL2Dataset(x)
}

getBand = function(x, band = 1L)
{
  assertClass(x, 'RGDAL2Dataset')
  b = GDALGetRasterBand(x@handle, band)
  newRGDAL2RasterBand(b, x)
}

getDataset = function(x)
{
    assertClass(x, 'RGDAL2RasterBand')
    x@dataset
}

getColorTable = function(x)
{
  x = checkBand(x)
  x = RGDALGetRasterColorTable(x@handle)
  attr(x, 'class') = NULL
  as.matrix(x)
}

getMask = function(x)
{
  x = checkBand(x)
  m = GDALGetMaskBand(x@handle)
  f = GDALGetMaskFlags(x@handle)
  newRGDAL2RasterMask(m, x@dataset, f)
}

getMaskFlags = function(x)
{
  list(no.mask = bitwAnd(x@flag, 1L) != 0L,
       is.shared = bitwAnd(x@flag, 2L) != 0L,
       is.alpha = bitwAnd(x@flag, 4L) != 0L,
       is.nodata = bitwAnd(x@flag, 8L) != 0L)
}

setMethod('show', 'RGDAL2Dataset', function(object)
{
    gdalinfo = Sys.which('gdalinfo')
    fname = GDALGetDescription(object@handle)
    if ( nchar(gdalinfo) > 0 && file.access(fname, 4) == 0 )
    {
        info = pipe(paste(gdalinfo, '-nogcp -nomd -noct -nofl', fname), 'rt')
        res = readLines(info); close(info)
        catLines(res)
        return(invisible(res))
    }
    else
    {
        cat('GDAL Dataset at address ')
        print(object@handle)
        return(invisible(object))    
    }
})

setMethod('show', 'RGDAL2RasterBand', function(object)
{
    if ( max(dim(object)) < 20 )
        show(as(object, 'matrix'))
    else
    {
        cat(paste0('GDAL Raster Band (', nrow(object), ', ', ncol(object), ') at address '))
        print(object@handle)
    }
})

setMethod('dim', 'RGDAL2Dataset', function(x)
{
    num_rows = GDALGetRasterYSize(x@handle)
    num_cols = GDALGetRasterXSize(x@handle)
    num_bands = GDALGetRasterCount(x@handle)
    c(num_rows, num_cols, num_bands)
})

setMethod('dim', 'RGDAL2RasterBand', function(x)
{
    num_rows = GDALGetRasterBandYSize(x@handle)
    num_cols = GDALGetRasterBandXSize(x@handle)
    c(num_rows, num_cols)
})

nband = function(x)
{
    dim(x)[3L]
}

getBlockSize = function(x)
{
    x = checkBand(x)
    res = RGDALGetBlockSize(x@handle)
    attr(res, 'class') = NULL
    res
}

setMethod('[', 
          signature('RGDAL2RasterBand', 'missing', 'missing'),
          function(x, i, j, ..., drop = TRUE)
{
  readRasterBand(x, 1L:nrow(x), 1L:ncol(x), ..., drop = drop)
})

setMethod('[', 
          signature('RGDAL2RasterBand', 'numeric', 'missing'),
          function(x, i, j, ..., drop = TRUE)
{
  readRasterBand(x, i, 1L:ncol(x), ..., drop = drop)
})

setMethod('[', 
          signature('RGDAL2RasterBand', 'missing', 'numeric'),
          function(x, i, j, ..., drop = TRUE)
{
  readRasterBand(x, 1L:nrow(x), j, ..., drop = drop)
})

setMethod('[', 
          signature('RGDAL2RasterBand', 'numeric', 'numeric'),
          function(x, i, j, ..., drop = TRUE)
{
  readRasterBand(x, i, j, ..., drop = drop)
})

setMethod('[',
          signature('RGDAL2RasterBand', 'RGDAL2Geometry', 'missing'),
          function(x, i, j, ..., drop = TRUE)
{
    ij = region2Indices(x, i)
    readRasterBand(x, ij$i, ij$j, ..., drop = drop)
})

setMethod('[[', 
          signature('RGDAL2RasterBand', 'missing', 'missing'),
          function(x, i, j, ...)
{
    i = 1L:nrow(x); j = 1L:ncol(x)
    res = RGDAL_CopySubset(x@handle, min(j) - 1, min(i) - 1, length(j), length(i))
    getBand(newRGDAL2Dataset(res))
})

setMethod('[[', 
          signature('RGDAL2RasterBand', 'numeric', 'missing'),
          function(x, i, j, ...)
{
    j = 1L:ncol(x)
    res = RGDAL_CopySubset(x@handle, min(j) - 1, min(i) - 1, length(j), length(i))
    getBand(newRGDAL2Dataset(res))
})

setMethod('[[', 
          signature('RGDAL2RasterBand', 'missing', 'numeric'),
          function(x, i, j, ..., drop = TRUE)
{
    i = 1L:nrow(x)
    res = RGDAL_CopySubset(x@handle, min(j) - 1, min(i) - 1, length(j), length(i))
    getBand(newRGDAL2Dataset(res))
})

setMethod('[[', 
          signature('RGDAL2RasterBand', 'numeric', 'numeric'),
          function(x, i, j, ..., drop = TRUE)
{
    res = RGDAL_CopySubset(x@handle, min(j) - 1, min(i) - 1, length(j), length(i))
    getBand(newRGDAL2Dataset(res))
})

setMethod('[[',
          signature('RGDAL2RasterBand', 'RGDAL2Geometry'),
          function(x, i, j, ...)
{
    i = reproject(i, getSRS(x))
    pts = getPoints(extent(i))
    tpts = RGDAL_ApplyGeoTransform(x@dataset@handle, pts, 1L)
    xoff = floor(min(tpts$x) - 1); yoff = floor(min(tpts$y) - 1)
    xsz = ceiling(diff(range(tpts$x)) + 1); ysz = ceiling(diff(range(tpts$y)) + 1)
    res = RGDAL_CopySubset(x@handle,xoff, yoff, xsz, ysz)
    getBand(newRGDAL2Dataset(res))
})

setMethod('[<-',
          signature('RGDAL2RasterBand', 'missing', 'missing'),
          function(x, i, j, ..., value)
{
  writeRasterBand(x, 1L:nrow(x), 1L:ncol(x), ..., value = value)
})

setMethod('[<-',
          signature('RGDAL2RasterBand', 'numeric', 'missing'),
          function(x, i, j, ..., value)
{
  writeRasterBand(x, i, 1L:ncol(x), ..., value = value)
})

setMethod('[<-',
          signature('RGDAL2RasterBand', 'missing', 'numeric'),
          function(x, i, j, ..., value)
{
  writeRasterBand(x, 1L:nrow(x), j, ..., value = value)
})

setMethod('[<-',
          signature('RGDAL2RasterBand', 'numeric', 'numeric'),
          function(x, i, j, ..., value)
{
  writeRasterBand(x, i, j, ..., value = value)
})

setAs('RGDAL2Dataset', 'array', function(from)
{
    dims = dim(from)
    from[1L:dims[1L], 1L:dims[2L], 1L:dims[3L]]
})

setAs('RGDAL2Dataset', 'vector', function(from)
{
    as(as(from, 'array'), 'vector')
})

setAs('RGDAL2RasterBand', 'matrix', function(from)
{
    from[1L:nrow(from), 1L:ncol(from)]
})

setAs('RGDAL2RasterBand', 'vector', function(from)
{
    as(as(from, 'matrix'), 'vector')
})

readBlock = function(object, i, j)
{
    object = checkBand(object)
    res = RGDAL_ReadBlock(object@handle, i, j)
    attr(res, "class") = NULL
    res
}

writeBlock = function(object, i, j, x)
{
    object = checkBand(object)
    if ( RGDAL_WriteBlock(object@handle, i, j, x) )
        stop("Error writing block")
    invisible(object)
}

blockCoordIter = function(b, block.size = getBlockSize(b))
{
    x = 1L
    y = 1L
    xx = ncol(b)
    yy = nrow(b)
    block.size = rep(block.size, length = 2)
    xby = block.size[2L]
    yby = block.size[1L]
    f = function()
    {
        if ( y > yy ) stop('StopIteration')
        yrange = y:min(c(yy, y + yby - 1L))
        xrange = x:min(c(xx, x + xby - 1L))
        x <<- x + xby
        if ( x > xx )
        {
            x <<- 1L
            y <<- y + yby
        }
        list(x = xrange, y = yrange)
    }
    structure(list(nextElem = f), class = c('rgdal2BlockIter', 'abstractiter', 'iter'))
}

blockIter = function(b, block.size = getBlockSize(b), native.indexing = FALSE)
{
    x = 1L
    y = 1L
    xx = ncol(b)
    yy = nrow(b)
    block.size = rep(block.size, length = 2)
    xby = block.size[2L]
    yby = block.size[1L]
    f = function()
    {
        if ( y > yy ) stop('StopIteration')
        yrange = y:min(c(yy, y + yby - 1L))
        xrange = x:min(c(xx, x + xby - 1L))
        res = b[yrange, xrange, drop = FALSE]
        x <<- x + xby
        if ( x > xx )
        {
            x <<- 1L
            y <<- y + yby
        }
        list(x = xrange, y = yrange, z = res, native.indexing = native.indexing)
    }
    structure(list(nextElem = f), class = c('rgdal2BlockIter', 'abstractiter', 'iter'))
}

foreach.block = function(x,
                         out = newGDALDataset(nrow(x), ncol(x)),
                         block.size = getBlockSize(x),
                         init = getBand(out),
                         combine = function(out, i)
                                   {
                                       out[i$y, i$x, native.indexing = i$native.indexing] = i$z
                                       return(out)
                                   },
                         final = function(x) out,
                         inorder = FALSE,
                         native.indexing = FALSE)
{
  args = list()
  if ( inherits(x, 'RGDAL2RasterBand') )
  {
    args[['i']] = blockIter(x, block.size, native.indexing) 
  }
  else
  {
    for ( i in 1:nband(x) )
    {
      args[[paste0('i', i)]] = blockIter(getBand(x, i), block.size, native.indexing)
    }
  }
  args[['.init']] = init
  args[['.combine']] = combine
  args[['.final']] = final
  args[['.inorder']] = inorder
  do.call('foreach', args)
}

setMethod("extent", "RGDAL2Dataset",
function(object)
{
    x = RGDAL_GetRasterExtent(object@handle)
    res = newRGDAL2Geometry(x)
    setSRS(res, getSRS(object))
    res
})

setMethod("extent", "RGDAL2RasterBand",
function(object)
{
    extent(object@dataset)
})

setMethod("reproject", "RGDAL2Dataset",
function(object, SRS, file = tempfile(), driver = "MEM", thresh = 0.125)
{
    srs.out = getWKT(SRS)
    res = RGDAL_RasterWarp(object@handle, file, srs.out, driver, thresh)
    newRGDAL2Dataset(res)
})

setMethod("reproject", "RGDAL2RasterBand",
function(object, SRS, file = tempfile(), driver = "MEM", thresh = 0.125)
{
    srs.out = getWKT(SRS)
    res = RGDAL_RasterWarp(object@dataset@handle, file, srs.out, driver, thresh)
    getBand(newRGDAL2Dataset(res))
})






