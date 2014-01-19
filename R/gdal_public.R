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
#' @examples
#' f = system.file("example-data/butterfly.jpg", package = "rgdal2")
#' x = openGDAL(f)
#' show(x)
#'
#' @family gdal-io   
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
#' @param band the band number (1-based)
#' @param readonly if true, prohibit write operations
#' 
#' @return an object of class RGDAL2RasterBand
#' 
#' @examples
#' f = system.file("example-data/butterfly.jpg", package = "rgdal2")
#' x = openGDALBand(f)
#' show(x)
#'
#' @family gdal-io
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
#' For most purposes, only the "GTiff" and "MEM" drivers are needed. "GTiff"
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
#' @seealso \code{\link{copyDataset}}
#' 
#' @examples
#' x = newGDALDataset(100, 100, 3, driver = "GTiff", nosave = TRUE)
#' show(x); dim(x)
#'
#' @family gdal-io   
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
#' @examples
#' x = newGDALDataset(10, 10, 3)
#' y = copyDataset(x)
#' show(x); show(y)
#'
#' @family gdal-io   
#' @export
copyDataset = function(x, file = tempfile(), driver = "MEM")
{
    x = checkDataset(x)
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

#' Display information about a dataset
#' 
#' @param object a dataset
#' 
#' @details
#' If possible, this function will call and return the output of
#' the \code{gdalinfo} command. An attempt is made to discover the
#' number of console lines and truncate the output to fit on the
#' current console. Otherwise, a simple message is printed along
#' with the memmory address of the handle.
#' 
#' @seealso \code{\link{show}}
#' 
#' @examples
#' f = system.file("example-data/gtopo30_vandg.tif", package = "rgdal2")
#' x = openGDAL(f)
#' show(x)
#'
#' @export
setMethod('show',
signature('RGDAL2Dataset'),
function(object)
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

#' Display information about a raster band
#' 
#' @param object a raster band
#' 
#' @seealso \code{\link{show}}
#' 
#' @examples
#' f = system.file("example-data/gtopo30_vandg.tif", package = "rgdal2")
#' x = openGDALBand(f)
#' show(x)
#'
#' @export
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

#' Return dimensions of a dataset
#' 
#' @param x a dataset
#' 
#' @seealso \code{\link{dim}}
#' 
#' @examples
#' f = system.file("example-data/gtopo30_vandg.tif", package = "rgdal2")
#' x = openGDAL(f)
#' dim(x)
#'
#' @export
setMethod('dim', 'RGDAL2Dataset', function(x)
{
    num_rows = GDALGetRasterYSize(x@handle)
    num_cols = GDALGetRasterXSize(x@handle)
    num_bands = GDALGetRasterCount(x@handle)
    c(num_rows, num_cols, num_bands)
})

#' Return dimensions of a band
#' 
#' @param x a dataset
#' 
#' @seealso \code{\link{dim}}
#' 
#' @examples
#' f = system.file("example-data/gtopo30_vandg.tif", package = "rgdal2")
#' x = openGDAL(f)
#' dim(x)
#'
#' @export
setMethod('dim', 'RGDAL2RasterBand', function(x)
{
    num_rows = GDALGetRasterBandYSize(x@handle)
    num_cols = GDALGetRasterBandXSize(x@handle)
    c(num_rows, num_cols)
})

#' Return number of raster bands in a dataset
#' 
#' @param x a dataset
#' 
#' @seealso \code{\link{dim}}
#' 
#' @examples
#' f = system.file("example-data/butterfly.jpg", package = "rgdal2")
#' x = openGDAL(f)
#' nband(x)
#'
#' @export
nband = function(x)
{
    dim(x)[3L]
}

#' Return the internal blocksize of a dataset
#' 
#' @param x a dataset or raster band
#' 
#' @details
#' GDAL datasets can have internal data organized in different ways. Most
#' datasets are in scanline, pixel (sequential row) arrangement. Other
#' datasets may be tiled. This allows efficient local access to blocks of
#' raster data. The GDAL commandline utilities can be used to reblock a
#' dataset. This function returns the blocksize of the dataset. Block-
#' aligned access is much more efficient than other data chunking
#' as GDAL caches whole blocks internally.
#' 
#' @note
#' GDAL uses and x, y or longitude, latitude convention. The \code{rgdal2}
#' package will always return values as y, x (row, column) or latitude,
#' longitude.
#' 
#' @examples
#' f = system.file("example-data/gtopo30_vandg.tif", package = "rgdal2")
#' x = openGDAL(f)
#' getBlockSize(x)
#'
#' @export
getBlockSize = function(x)
{
    x = checkBand(x)
    res = GDALGetBlockSize(x@handle)
    res
}

#' Extract data from a raster band
#' 
#' Use R-style semantics to read data from a raster band
#' 
#' @param x a raster band
#' @param i the row indices
#' @param j the column indicies
#' @param ii subsampling row indices (see details)
#' @param jj subsampling column indices (see details)
#' @param drop if true (default), drop singleton dimensions
#' @param use.mask if true (default), set invalid data to \code{NA}
#' 
#' @details
#' A raster band emulates an R array. Indexing should operate similarly
#' to native R objects. GDAL internal access functions only allow fetching
#' of square blocks of data, so \code{x[c(1, 100), c(1, 100)]} will have the
#' same overhead as \code{x[1:100, 1:100]}. The first however will return a
#' 2x2 matrix whereas the second will return a 100x100 matrix. If either \code{i}
#' or \code{j} are missing, they assume the values \code{1:nrow(x)} and
#' \code{1:ncol(x)} respectively.
#' 
#' The subsampling indices \code{ii} and \code{jj} can be used to subsample
#' the extracted data. The output matrix will have \code{length(ii)} rows and
#' \code{length(jj)} columns. If the number of elements of \code{ii} (\code{jj})
#' is smaller than the number of elements in \code{i} (\code{j}) then the
#' image will be subsampled by skipping pixel values. If the opposite is true,
#' pixel values will be duplicated to fill out the returned matrix. Note that
#' whenever the subsampling indices are applied, the minimum value is subtracted
#' so that \code{ii = 1:3} is equivalent to \code{ii = 100:103}.
#' 
#' As a special case, \code{i} may be given as a geometry. The extent of the
#' geometry will be extracted and used to subset the raster data. The geometry
#' will be reprojected to match the spatial reference system  of the dataset.
#' This can be combined with the \code{ii} and \code{jj} parameters.
#' 
#' Data are always returned as R type \code{numeric}. The functions
#' \code{\link{readBlock}} and \code{\link{writeBlock}} are faster and will
#' return raw bytes, integer or numeric values depending on the type
#' of data stored in the band. 
#' 
#' @seealso \code{\link{readBlock}}
#' 
#' @name [,RGDAL2RasterBand
#' @aliases [,band
#' @examples
#' x = newGDALBand(5, 5)
#' x[]
#' x[] = 1:25
#' x[]
#' x[1:2]
#' x[c(1, 3), 4:2]
#' x[ii = 1:3, jj = c(4, 2)]
#' x[i = 1:2, j = 3:2, ii = 1:5, jj = 1:7]
#'
#' @aliases [,RGDAL2RasterBand
#' @rdname sub-band
#' @export
setMethod('[', 
          signature('RGDAL2RasterBand', 'numeric', 'numeric'),
          function(x, i, j, ..., drop = TRUE)
{
  readRasterBand(x, i, j, ..., drop = drop)
})

#' @rdname sub-band
#' @export
setMethod('[', 
          signature('RGDAL2RasterBand', 'missing', 'missing'),
          function(x, i, j, ..., drop = TRUE)
          {
            readRasterBand(x, 1L:nrow(x), 1L:ncol(x), ..., drop = drop)
          })

#' @rdname sub-band
#' @export
setMethod('[', 
          signature('RGDAL2RasterBand', 'numeric', 'missing'),
          function(x, i, j, ..., drop = TRUE)
          {
            readRasterBand(x, i, 1L:ncol(x), ..., drop = drop)
          })

#' @rdname sub-band
#' @export
setMethod('[', 
          signature('RGDAL2RasterBand', 'missing', 'numeric'),
          function(x, i, j, ..., drop = TRUE)
          {
            readRasterBand(x, 1L:nrow(x), j, ..., drop = drop)
          })

#' @rdname sub-band
#' @export
setMethod('[',
          signature('RGDAL2RasterBand', 'RGDAL2Geometry', 'missing'),
          function(x, i, j, ..., drop = TRUE)
{
    ij = region2Indices(x, i)
    readRasterBand(x, ij$i, ij$j, ..., drop = drop)
})

setMethod('[[', 
          signature('RGDAL2RasterBand', 'numeric', 'numeric'),
          function(x, i, j, ..., drop = TRUE)
          {
            res = RGDAL_CopySubset(x@handle, min(j) - 1, min(i) - 1, length(j), length(i))
            getBand(newRGDAL2Dataset(res))
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

#' Get a block-matrix object
#' 
#' Construct an object that acts as a matrix of raster blocks
#' 
#' @param b the band or dataset
#' 
#' @details
#' GDAL raster bands have an internal blocking strcuture. This is usually
#' a simple scanline, pixel arrangement where each image row is a single
#' block of data. Other datasets may have internal storage arranged as
#' tiled blocks of data. Block access is much faster than random IO as
#' these blocks are cached by the GDAL IO layer. A strategy for efficient
#' update of large files is to read a block of data, modify it, and then
#' write the block either into a new dataset or into the original dataset
#' overwriting the original data.
#' 
#' The returned object acts as a matrix in so far as it
#' has methods for the \code{[[]]} operator and you can retrieve
#' its dimensions. Indexing the block-matrix object will return
#' the corresponding block of raster data.
#' 
#' Note that especially for tiled data, the blocks will not perfectly
#' subdivide the raster. Portions of marginal blocks on the right and
#' bottom will often extend beyond the raster extent. Out-of-bound block pixel
#' values will usually be set to \code{NA} in that case. (Raw byte data does
#' not have an \code{NA} value defined, so in that case the out-of-bounds
#' pixels will be set to zero.) However the behavior is driver dependend
#' and therefore may vary by file type. The returned blocks are not truncated
#' to fit within the raster.
#' 
#' Whem writing blocks, the \code{\link{storage.mode}} of the value parameter
#' must match that of the raster band. The dimensions of the object do not
#' matter; however its length must be equal to the number of elements in a
#' block. All integral types other than raw are handled as \code{integer} type.
#' 
#' @return
#' a \code{RGDAL2BlockMatrix} object, a raster block or the block dimensions
#'
#' @examples
#' f = system.file("example-data/butterfly.jpg", package = "rgdal2")
#' x = openGDAL(f)
#' y = copyDataset(x)
#' m = getBlockMatrix(y)
#' dim(m)
#' dim(m[[1, 1]])
#' z = m[[1, 1]]
#' z[] = as.raw(42)
#' m[[1, 1]] = z
#' m[[1, 1]][1, 1]
#' 
#' @rdname block-matrix
#' @export
getBlockMatrix = function(b)
{
  b = checkBand(b)
  new("RGDAL2BlockMatrix", band = b)
}

#' @rdname block-matrix
#' @param i the block row
#' @param j the block column
#' @param drop if true, drop singleton dimensions
#' @export
setMethod('[[', 'RGDAL2BlockMatrix',
function(x, i, j, ...)
{
  RGDAL_ReadBlock(x@band@handle, i, j)
})

#' @rdname block-matrix
#' @param value an r object of appropriate type and length
#' @export
setMethod('[[<-', 'RGDAL2BlockMatrix',
function(x, i, j, ..., value)
{
  if ( RGDAL_WriteBlock(x@band@handle, i, j, value) )
    stop("Error writing block")
  invisible(x)
})

#' @rdname block-matrix
#' @export
setMethod('dim', 'RGDAL2BlockMatrix',
function(x)
{
  ceiling(dim(x@band) / getBlockSize(x@band))
})

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


