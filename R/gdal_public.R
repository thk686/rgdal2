#
# Copyright Timothy H. Keitt
#

#' Open a GDAL dataset
#'
#' Opens the dataset associated with the specified file
#' 
#' @param fname the file name
#' @param readonly if true, prohibit write operations
#' @param shared if true, share the internal GDAL handle
#' 
#' @return an object of class RGDAL2Dataset
#' 
#' @author Timothy H. Keitt
#' 
#' @examples
#' f = system.file("example-data/butterfly.jpg", package = "rgdal2")
#' x = openGDAL(f)
#' show(x)
#'   
#' @export
openGDAL = function(fname, readonly = TRUE, shared = TRUE)
{
  x = GDALOpen(fname, readonly, shared)
  newRGDAL2Dataset(x)
}

#' Open a GDAL dataset and return a band
#'
#' Opens the dataset associated with the specified file and returns a band
#' 
#' @param fname the file name
#' @oaran babd the band number (1-based)
#' @param readonly if true, prohibit write operations
#' 
#' @return an object of class RGDAL2RasterBand
#' 
#' @author Timothy H. Keitt
#'
#' @examples
#' f = system.file("example-data/butterfly.jpg", package = "rgdal2")
#' x = openGDALBand(f)
#' show(x)
#'   
#' @export
openGDALBand = function(fname, band = 1L, readonly = TRUE)
{
  getBand(openGDAL(fname, readonly), band)
}

#' Create a new GDAL dataset
#'
#' @param nrow number of rows (scan lines)
#' @param ncol number of columns (pixels)
#' @param nbands number of bands
#' @param dataType the storage data type (see details)
#' @param driver the name of the dataset driver
#' @param file the name of the file to create
#' @param nosave unlink the file after creation
#' 
#' @details
#' For most purposed, only the "GTiff" and "MEM" drivers are needed. "GTiff"
#' creates a geotiff file-based dataset. The "MEM" driver creates the dataset
#' in memory and the data will not be saved unless the dataset is copied to a
#' file-based dataset. Similarly, if \code{nosave} is true, then a dataset will
#' be created on disk, but the underlying file will be unlinked. This may be useful
#' for dealing with huge temporary datasets beyond memory capacity. Once a dataset
#' is closed (this will happen automatically if the dataset object goes out of scope),
#'  all data will be lost unless the dataset is copied to another file-based dataset. 
#' 
#' The data type is given as a string, and can be one of Byte, Int16, Int32,
#' Float32, Float64. These strings are the same as the GDALDataType enum in the GDAL
#' distribution, but with the prefix "GDT_" removed. Other data types have limited
#' support. See \url{http://www.gdal.org} for more information.
#' 
#' @return an object of class RGDAL2Dataset
#' 
#' @author Timothy H. Keitt
#' 
#' @seealso \code{\link{copyDataset}}
#' 
#' @examples
#' x = newGDALDataset(100, 100, 3, driver = "GTiff", nosave = TRUE)
#' show(x); dim(x)
#'   
#' @export
newGDALDataset = function(nrow, ncol, nbands = 1,
                          dataType = 'Int32', driver = 'MEM',
                          file = tempfile(), nosave = FALSE)
{
  x = RGDALCreateDataset(driver, file, nrow, ncol, nbands, dataType)
  if ( driver == 'MEM' || nosave ) unlink(file)
  newRGDAL2Dataset(x)
}

#' Copy a GDAL dataset
#'
#' Copies a source dataset to a new dataset
#' 
#' @param x the source dataset
#' @param file the file for the new dataset
#' @param driver the driver for the new dataset
#' 
#' @details
#' The \code{file} parameter is ignored if the \code{driver} parameter
#' is set to "MEM".
#' 
#' @return an object of class RGDAL2Dataset
#' 
#' @author Timothy H. Keitt
#' 
#' @examples
#' x = newGDALDataset(10, 10, 3)
#' y = copyDataset(x)
#' show(x); show(y)
#'   
#' @export
copyDataset = function(x, file = tempfile(), driver = "MEM")
{
    assertClass(x, "RGDAL2Dataset")
    handle = RGDAL_CreateCopy(x@handle, file, driver)
    newRGDAL2Dataset(handle)
}

#' Retrieve the affine transformation
#'
#' Retrieves an offset vector and a rotation matrix that allows
#' projection from pixel, scan-line (row, column) indices to
#' geospatial coordinates.
#' 
#' @param object a dataset or raster band
#' 
#' @return
#' a 2-element list:
#' \item{transl}{a 2-element vector giving the x and y offsets}
#' \item{affmat}{a 2x2 scale-rotation matrix}
#' 
#' @author Timothy H. Keitt
#' 
#' @seealso \code{\link{setTransform}}, \code{\link{copyTransform}}
#' 
#' @export
getTransform = function(object)
{
    object = checkDataset(object)
    res = RGDAL_GetGeoTransform(object@handle)
    list(transl = c(res[c(1, 4)]),
         affmat = matrix(res[c(2, 3, 5, 6)], 2))
}

#' Sets the affine transformation
#'
#' Sets the affine transform coefficients on a dataset. These
#' allow projection from pixel, scan-line (row, column) indices to
#' geospatial coordinates.
#' 
#' @param object a dataset or raster band
#' @param transform a list a returned by \code{\link{getTransform}}
#' 
#' @return the dataset invisibly
#' 
#' @author Timothy H. Keitt
#' 
#' @seealso \code{\link{getTransform}}, \code{\link{copyTransform}}
#' 
#' @export
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

#' Copies affine transform coefficients
#'
#' The affine transform coefficients are copied from one dataset
#' to another.
#' 
#' @param obj1 the source dataset or raster band
#' @param obj2 the target dataset or raster band
#' 
#' @return the target dataset invisibly
#' 
#' @author Timothy H. Keitt
#' 
#' @seealso \code{\link{getTransform}}, \code{\link{setTransform}}
#' 
#' @export
copyTransform = function(obj1, obj2)
{
    obj1 = checkDataset(obj1)
    obj2 = checkDataset(obj2)
    setTransform(obj2, getTransform(obj1))
    invisible(obj2)
}

#' Create a new GDAL raster band
#' 
#' This is a convenience wrapper around \code{\link{newGDALDataset}}. It
#' calls \code{\link{newGDALDataset}} and then returns the first band.
#'
#' @param nrow number of rows (scan lines)
#' @param ncol number of columns (pixels)
#' @param nbands number of bands
#' @param dataType the storage data type
#' @param driver the name of the dataset driver
#' @param file the name of the file to create
#' 
#' @return an object of class RGDAL2RasterBand
#' 
#' @author Timothy H. Keitt
#' 
#' @seealso \code{\link{newGDALDataset}}
#' 
#' @examples
#' x = newGDALBand(100, 100, 3)
#' show(x); dim(x)
#' y = getDataset(x)
#' show(y)
#'   
#' @export
newGDALBand = function(nrow, ncol, dataType = 'Int32',
                       driver = 'MEM', file = tempfile())
{
    getBand(newGDALDataset(nrow, ncol, 1L, dataType, driver, file))
}

#' Fetch a raster band object from a dataset
#' 
#' @param x a dataset object
#' @param band the band number
#' 
#' @details
#' Band indices start at 1.
#' 
#' @return an object of class RGDAL2RasterBand
#' 
#' @author Timothy H. Keitt
#' 
#' @seealso \code{\link{newGDALBand}}, \code{\link{nband}}
#' 
#' @examples
#' x = newGDALDataset(100, 100, 3)
#' show(x); dim(x)
#' y = getBand(x, 2)
#' show(y)
#'   
#' @export
getBand = function(x, band = 1L)
{
  assertClass(x, 'RGDAL2Dataset')
  b = GDALGetRasterBand(x@handle, band)
  newRGDAL2RasterBand(b, x)
}

#' Fetch the dataset owning a raster band
#' 
#' @param x a raster band object
#' 
#' @details
#' Raster bands are always associated with a dataset and cannot
#' be deleted. This function fetches the dataset to which the
#' raster band belongs.
#' 
#' @return an object of class RGDAL2Dataset
#' 
#' @author Timothy H. Keitt
#' 
#' @seealso \code{\link{getBand}}
#' 
#' @examples
#' x = newGDALBand(100, 100)
#' show(x)
#' y = getDataset(x)
#' show(y)
#'   
#' @export
getDataset = function(x)
{
    assertClass(x, 'RGDAL2RasterBand')
    x@dataset
}

getColorTable = function(x)
{
  x = checkBand(x)
  x = RGDALGetRasterColorTable(x@handle)
  as.matrix(x)
}

#' Fetch the mask associated with a raster band
#' 
#' @param x a raster band or dataset
#' 
#' @details
#' The primary purpose of a mask band is to indicate no-data regions
#' that should be clipped when drawing and analyzing. A mask band can
#' also contain alpha values in a RGBA dataset. The meaning of the band
#' can be ascertained using \code{\link{getMaskFlags}}.
#' 
#' Note that if a dataset does not contain a mask, this function will
#' still construct and return a mask of all true (non-zero) values.
#' 
#' As a convenience when working with single band datasets, this function
#' will automatically extract the first band if a dataset is passed. Other
#' bands are ignored.
#' 
#' @return an object of class RGDAL2RasterMask
#' 
#' @author Timothy H. Keitt
#' 
#' @seealso \code{\link{getMaskFlags}}
#' 
#' @examples
#' f = system.file("example-data/gtopo30_vandg.tif", package = "rgdal2")
#' x = openGDALBand(f)
#' show(x)
#' getMaskFlags(x)
#' y = getMask(x)
#' show(y)
#' 
#' @export
getMask = function(x)
{
  x = checkBand(x)
  m = GDALGetMaskBand(x@handle)
  f = GDALGetMaskFlags(x@handle)
  newRGDAL2RasterMask(m, x@dataset, f)
}

#' Fetch flags indicating the mask interpretation
#' 
#' @param x a raster band, dataset or mask
#' 
#' @note
#' As a convenience when working with single band datasets, this function
#' will automatically extract the first band if a dataset is passed. Other
#' bands are ignored.
#' 
#' @return
#' \item{no.mask}{true if there is no stored mask}
#' \item{is.shared}{true if the mask applies to all bands}
#' \item{is.alpha}{true if the mask contains alpha transparency values}
#' \item{is.nodata}{true if the maks indicates valid data}
#' 
#' @author Timothy H. Keitt
#' 
#' @seealso \code{\link{getMask}}
#' 
#' @examples
#' f = system.file("example-data/gtopo30_vandg.tif", package = "rgdal2")
#' x = openGDALBand(f)
#' show(x)
#' getMaskFlags(x)
#' 
#' @export
getMaskFlags = function(x)
{
  x = checkBand(x)
  if ( inherits(x, "RGDAL2RasterMask") ) flag = x@flag
  else flag = GDALGetMaskFlags(x@handle)
  list(no.mask = bitwAnd(flag, 1L) != 0L,
       is.shared = bitwAnd(flag, 2L) != 0L,
       is.alpha = bitwAnd(flag, 4L) != 0L,
       is.nodata = bitwAnd(flag, 8L) != 0L)
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






