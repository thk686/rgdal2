#
# Copyright Timothy H. Keitt
#

#' @include defs.R
#' @include gdal_private.R
NULL

#' Open a GDAL dataset
#'
#' Opens a dataset and returns the dataset or a band from the dataset
#' 
#' @param fname the file name
#' @param readonly if true, prohibit write operations
#' @param shared if true, share the internal GDAL handle
#' 
#' @details
#' A dataset holds raster data organized into multiple layers or bands.
#' The \code{openRasterBand} function simply opens a dataset and returns
#' the indicated band. This is a convenience in the common situation of
#' working with single-band datasets.
#' 
#' @return \code{openDataset}: a dataset object
#' 
#' @examples
#' f = system.file("example-data/bee.jpg", package = "rgdal2")
#' x = openDataset(f)
#' show(x)
#' draw(x)
#'
#' @seealso \code{\link{openRasterBand}}, \code{\link{openOGR}}
#' @rdname gdalopen
#' @export
openDataset = function(fname, readonly = TRUE, shared = TRUE)
{
  x = RGDAL_Open(fname, readonly, shared)
  newRGDAL2Dataset(x)
}

#' @param band the band number (1-based)
#' 
#' @return \code{openRasterBand}: a raster band
#' 
#' @examples
#' f = system.file("example-data/bee.jpg", package = "rgdal2")
#' x = openRasterBand(f)
#' show(x)
#' draw(x)
#'
#' @rdname gdalopen
#' @export
openRasterBand = function(fname, band = 1L, readonly = TRUE)
{
  getBand(openDataset(fname, readonly), band)
}

#' Create a new GDAL dataset
#'
#' @param nrow number of rows (scan lines)
#' @param ncol number of columns (pixels)
#' @param nbands number of bands
#' @param dataType the storage data type (see details)
#' @param driver the name of the dataset driver
#' @param file the name of the file to create
#' @param opts dataset creation options (see \url{http://www.gdal.org/formats_list.html})
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
#' all data will be lost unless the dataset is copied to another file-based dataset. 
#' 
#' The data type is given as a string, and can be one of Byte, Int16, Int32,
#' Float32, Float64. These strings are the same as the GDALDataType enum in the GDAL
#' distribution, but with the prefix "GDT_" removed. Other data types have limited
#' support. See \url{http://www.gdal.org} for more information.
#' 
#' @return an object of class RGDAL2Dataset
#' 
#' @note Be careful not to unlink an existing file.
#' 
#' @seealso \code{\link{copyDataset}}
#' 
#' @examples
#' createOpts = c("COMPRESS=LZW", "TILED=YES", "BLOCKXSIZE=16", "BLOCKYSIZE=16")
#' x = newDataset(64, 64, 5, driver = "GTiff", opts = createOpts)
#' show(x)
#' x = newDataset(100, 100, 3, driver = "GTiff", nosave = TRUE)
#' show(x); dim(x)
#'
#' @seealso \code{\link{openRasterBand}}, \code{\link{openOGR}} 
#' 
#' @export
newDataset = function(nrow, ncol, nbands = 1L,
                      dataType = 'Int32', driver = 'MEM',
                      file = tempfile(), opts = character(),
                      nosave = FALSE)
{
  x = RGDAL_CreateDataset(driver, file, nrow, ncol, nbands, dataType, opts)
  if ( nosave ) unlink(file)
  res = newRGDAL2Dataset(x)
  if ( length(paste0(opts)) > 0 )
    setMetadata(res, opts, "rgdal2_create_opts")
  return(res)
}

#' Copy a GDAL dataset
#'
#' Copies a source dataset to a new dataset
#' 
#' @param x the source dataset
#' @param file the file for the new dataset
#' @param driver the driver for the new dataset
#' @param opts create options; vector of key=value pairs
#' 
#' @details
#' This function may be convenient for working with a dataset in memory. Note that
#' not all atrributes are copied at this time. If the driver specified is the same
#' as the driver of the source dataset and the source dataset was created in
#' \code{rgdal2}, then the creation options will be passed from the source to the
#' copy. You can suppress this behavior by setting \code{opts} explicitely, either
#' to valid options or an invalid option like \code{""}. Invalid options will
#' generate a warning message, but the dataset will still be copied. The invalid
#' option strings will propagate to subsequent copies without harm.
#' 
#' The \code{file} parameter is ignored if the \code{driver} parameter
#' is set to "MEM".
#' 
#' @return an GDAL dataset object
#' 
#' @examples
#' x = newDataset(10, 10, 3)
#' y = copyDataset(x)
#' show(x); show(y)
#'
#' @seealso \code{\link{openRasterBand}}, \code{\link{openOGR}} 
#' 
#' @export
copyDataset = function(x, file = tempfile(), driver = "MEM", opts = character())
{
    x = checkDataset(x)
    if ( length(paste0(opts)) < 1 &&
         identical(driver, getDriverName(x)) )
      opts = getMetadata(x, "rgdal2_create_opts")
    handle = RGDAL_CreateCopy(x@handle, file, driver, opts)
    res = newRGDAL2Dataset(handle)
    if ( length(paste0(opts)) > 0 )
      setMetadata(res, opts, "rgdal2_create_opts")
    return(res)
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
#' @examples
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openDataset(f)
#' getTransform(x)
#' 
#' @rdname get-set-transform
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
#' @param transform a list a returned by \code{\link{getTransform}}
#' 
#' @return the dataset invisibly
#' 
#' @examples
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openDataset(f)
#' y = copyDataset(x)
#' setTransform(y, getTransform(x))
#' getTransform(y)
#' 
#' @rdname get-set-transform
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
#' @examples
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openDataset(f)
#' y = copyDataset(x)
#' copyTransform(x, y)
#' getTransform(y)
#' 
#' @rdname get-set-transform
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
#' This is a convenience wrapper around \code{\link{newDataset}}. It
#' calls \code{\link{newDataset}} and then returns the first band.
#'
#' @param nrow number of rows (scan lines)
#' @param ncol number of columns (pixels)
#' @param dataType the storage data type
#' @param driver the name of the dataset driver
#' @param file the name of the file to create
#' 
#' @details
#' This function does not add a new band to an existing dataset. Very few drivers
#' support adding bands. Currently that capability is not yet implemented in \code{rgdal2}.
#' 
#' @return a raster band object
#' 
#' @seealso \code{\link{newDataset}}
#' 
#' @examples
#' x = newRasterBand(100, 100)
#' show(x); dim(x)
#' y = getDataset(x)
#' show(y)
#'   
#' @export
newRasterBand = function(nrow, ncol, dataType = 'Int32', driver = 'MEM', file = tempfile())
{
  getBand(newDataset(nrow, ncol, 1L, dataType, driver, file))
}

#' Fetch a raster band object from a dataset
#' 
#' @param x a dataset object
#' @param band the band number
#' 
#' @details
#' Band indices start at 1.
#' 
#' @return a raster band object
#' 
#' @seealso \code{\link{newRasterBand}}, \code{\link{nband}}
#' 
#' @examples
#' x = newDataset(100, 100, 3)
#' show(x); dim(x)
#' y = getBand(x, 2)
#' show(y)
#'
#' @rdname get-raster-band
#' @export
getBand = function(x, band = 1L)
{
  assertClass(x, 'RGDAL2Dataset')
  b = RGDAL_GetRasterBand(x@handle, band)
  newRGDAL2RasterBand(b, x)
}

#' Fetch the dataset owning a raster band
#' 
#' @details
#' Raster bands are always associated with a dataset and cannot
#' be deleted. This function fetches the dataset to which the
#' raster band belongs.
#' 
#' @return a dataset object
#' 
#' @seealso \code{\link{getBand}}
#' 
#' @examples
#' x = newRasterBand(100, 100)
#' show(x)
#' y = getDataset(x)
#' show(y)
#'
#' @rdname get-raster-band
#' @export
getDataset = function(x)
{
    assertClass(x, 'RGDAL2RasterBand')
    x@dataset
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
#' @return a raster mask object
#' 
#' @seealso \code{\link{getMaskFlags}}
#' 
#' @examples
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openRasterBand(f)
#' show(x)
#' getMaskFlags(x)
#' y = getMask(x)
#' show(y)
#' draw(y)
#' 
#' @export
getMask = function(x)
{
  x = checkBand(x)
  m = RGDAL_GetMaskBand(x@handle)
  newRGDAL2RasterMask(m, x@dataset)
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
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openRasterBand(f)
#' show(x)
#' getMaskFlags(x)
#' 
#' @export
getMaskFlags = function(x)
{
  x = checkBand(x)
  flag = RGDAL_GetMaskFlags(x@handle)
  list(no.mask = bitwAnd(flag, 1L) != 0L,
       is.shared = bitwAnd(flag, 2L) != 0L,
       is.alpha = bitwAnd(flag, 4L) != 0L,
       is.nodata = bitwAnd(flag, 8L) != 0L)
}

setMethod('show',
signature('RGDAL2Dataset'),
function(object)
{
    gdalinfo = Sys.which('gdalinfo')
    fname = RGDAL_GetDescription(object@handle)
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

setMethod('show',
signature('RGDAL2RasterBand'),
function(object)
{
    if ( max(dim(object)) < 20 )
        show(object[])
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
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openDataset(f)
#' dim(x)
#'
#' @aliases dim-dataset
#' @rdname dim-band-dataset
#' @export
setMethod('dim',
signature('RGDAL2Dataset'),
function(x)
{
    num_rows = RGDAL_GetRasterYSize(x@handle)
    num_cols = RGDAL_GetRasterXSize(x@handle)
    num_bands = RGDAL_GetRasterCount(x@handle)
    c(num_rows, num_cols, num_bands)
})

#' Return dimensions of a raster band
#' 
#' @seealso \code{\link{dim}}
#' 
#' @examples
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openRasterBand(f)
#' dim(x)
#'
#' @aliases dim-band
#' @rdname dim-band-dataset
#' @export
setMethod('dim',
signature('RGDAL2RasterBand'),
function(x)
{
    num_rows = RGDAL_GetRasterBandYSize(x@handle)
    num_cols = RGDAL_GetRasterBandXSize(x@handle)
    c(num_rows, num_cols)
})

#' Return number of raster bands in a dataset
#' 
#' @seealso \code{\link{dim}}
#' 
#' @examples
#' f = system.file("example-data/bee.jpg", package = "rgdal2")
#' x = openDataset(f)
#' nband(x)
#'
#' @rdname dim-band-dataset
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
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openDataset(f)
#' getBlockSize(x)
#'
#' @export
getBlockSize = function(x)
{
    x = checkBand(x)
    RGDAL_GetBlockSize(x@handle)
}

#' Extract data from a raster band
#' 
#' Use R-style syntax to read data from a raster band
#' 
#' @param x a raster band
#' @param i the row indices
#' @param j the column indicies
#' @param ... additional arguments (see details)
#' @param drop if true (default), drop singleton dimensions
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
#' so that \code{ii = 1:3} is equivalent to \code{ii = 101:103}.
#' 
#' If \code{use.mask} is set to \code{FALSE}, then all data will be read. Otherwise
#' data values where the mask is false will be returned as \code{NA}.
#' 
#' As a special case, \code{i} may be given as a geometry. The extent of the
#' geometry will be extracted and used to subset the raster data. The geometry
#' will be reprojected to match the spatial reference system  of the dataset.
#' This can be combined with the \code{ii} and \code{jj} parameters.
#' 
#' Data are always returned as R type \code{numeric}. The functions
#' \code{\link{readBlock}} and \code{\link{writeBlock}} are faster
#' and will return raw bytes, integer or numeric values depending on the type
#' of data stored in the band. 
#' 
#' @seealso \code{\link{readBlock}}
#' 
#' @examples
#' x = newRasterBand(5, 5)
#' x[]
#' x[] = 1:25
#' x[]
#' x[1:2]
#' x[c(1, 3), 4:2]
#' x[ii = 1:3, jj = c(4, 2)]
#' x[i = 1:2, j = 3:2, ii = 1:5, jj = 1:7]
#' e = makeExtent(2, 4, 2, 4)
#' show(e)
#' x[e]
#'
#' @aliases [,band
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
  readRasterBand(x, 1L:nrow(x), ..., drop = drop)
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

#' Subset a raster band
#' 
#' Extract a subset of a raster band and return a new raster band holding the result.
#' 
#' @param x a raster band object
#' @param i the row indices
#' @param j the column indices
#' 
#' @details
#' Rather than copying data into R memory, these indexing functions
#' copy the data to a new dataset. Only the minimum value of \code{i} and
#' \code{j} and their lengths determine the subset region. If for example
#' you scramble the values of \code{i} using \code{\link{sample}} (without
#' replacement) you will get the same result. This is a limitation of the
#' way that GDAL indexes into raster bands.
#' 
#' @examples
#' x = newRasterBand(5, 5)
#' x[[]]
#' x[] = 1:25
#' x[[]]
#' class(x[[]])
#' x[[1:2]]
#' x[[c(1, 3), 4:2]]
#' x[[i = 1:2, j = 3:2]]
#' e = makeExtent(2, 4, 2, 4)
#' show(e)
#' x[[e]]
#' 
#' @aliases [[,band
#' @rdname copy-band-subset
#' @export
setMethod('[[', 
signature('RGDAL2RasterBand', 'numeric', 'numeric'),
function(x, i, j)
{
  res = RGDAL_CopySubset(x@handle, min(j) - 1, min(i) - 1, length(j), length(i))
  getBand(newRGDAL2Dataset(res))
})

#' @rdname copy-band-subset
#' @export
setMethod('[[', 
signature('RGDAL2RasterBand', 'missing', 'missing'),
function(x, i, j)
{
    i = 1L:nrow(x); j = 1L:ncol(x)
    res = RGDAL_CopySubset(x@handle, min(j) - 1, min(i) - 1, length(j), length(i))
    getBand(newRGDAL2Dataset(res))
})

#' @rdname copy-band-subset
#' @export
setMethod('[[', 
signature('RGDAL2RasterBand', 'numeric', 'missing'),
function(x, i, j)
{
    j = 1L:ncol(x)
    res = RGDAL_CopySubset(x@handle, min(j) - 1, min(i) - 1, length(j), length(i))
    getBand(newRGDAL2Dataset(res))
})

#' @rdname copy-band-subset
#' @export
setMethod('[[', 
signature('RGDAL2RasterBand', 'missing', 'numeric'),
function(x, i, j)
{
    i = 1L:nrow(x)
    res = RGDAL_CopySubset(x@handle, min(j) - 1, min(i) - 1, length(j), length(i))
    getBand(newRGDAL2Dataset(res))
})

#' @rdname copy-band-subset
#' @export
setMethod('[[',
signature('RGDAL2RasterBand', 'RGDAL2Geometry'),
function(x, i)
{
    ij = region2Indices(x, i)
    xoff = floor(min(ij$j) - 1); yoff = floor(min(ij$i) - 1)
    xsz = ceiling(diff(range(ij$j)) + 1); ysz = ceiling(diff(range(ij$i)) + 1)
    res = RGDAL_CopySubset(x@handle, xoff, yoff, xsz, ysz)
    getBand(newRGDAL2Dataset(res))
})

#' Write data to a raster band
#' 
#' Use R-style semantics to write data to a raster band
#' 
#' @param x a raster band
#' @param i the row indices
#' @param j the column indicies
#' @param ... optional arguments (see details)
#' @param value the data to write
#' 
#' @details
#' In GDAL, the rows and columns of the data written to a raster band can be
#' different that the rows and columns touched by the write. The input will
#' be exanded or subsampled to fit the area written. Hence the range of \code{i}
#' and \code{j} need not be exactly the size of the data written to the band.
#' 
#' If \code{native.indexing} is true, then a region of raster band data will be
#' read from the band first. The assignment will then take place on this R matrix
#' and the matrix written back to the band. This will be slower, but allows one
#' to write to non-contiguous indexes as in R. There is a performance penalty
#' as the data block needs to be read and written back to the band.
#' 
#' If the subsetting indices \code{ii} or \code{jj} are set and \code{native.indexing}
#' is false, then their range of values will be interpreted as the matrix dimensions of
#' \code{value}. When \code{native.indexing} is true, these arguments are ignored.
#' 
#' This method of writting data is much slower than \code{\link{writeBlock}}
#' but will automatically cast the data to the correct type unlike \code{\link{writeBlock}}
#' which requires the input to be of the correct \code{\link{storage.mode}}.
#' 
#' @seealso \code{\link{writeBlock}}
#' 
#' @examples
#' x = newRasterBand(5, 5); x[]
#' x[] = 1:25; x[]
#' x[2:3, 2:3] = matrix(101:125, 5, 5); x[]
#' x[2:4, 2:4] = matrix(101:125, 5, 5); x[]
#' x[] = 1; x[]
#' x[c(2, 4), c(2, 4)] = 2:10; x[]; x[] = 1
#' x[c(2, 4), c(2, 4), native.indexing = TRUE] = 2:5; x[]
#' x[ii = 1:3, jj = 1:3] = 1:9; x[]
#'
#' @aliases [<-,band
#' @rdname write-band
#' @export
setMethod('[<-',
signature('RGDAL2RasterBand', 'missing', 'missing'),
function(x, i, j, ..., value)
{
  writeRasterBand(x, 1L:nrow(x), 1L:ncol(x), ..., value = value)
})

#' @rdname write-band
#' @export
setMethod('[<-',
signature('RGDAL2RasterBand', 'numeric', 'missing'),
function(x, i, j, ..., value)
{
  writeRasterBand(x, i, 1L:ncol(x), ..., value = value)
})

#' @rdname write-band
#' @export
setMethod('[<-',
signature('RGDAL2RasterBand', 'missing', 'numeric'),
function(x, i, j, ..., value)
{
  writeRasterBand(x, 1L:nrow(x), j, ..., value = value)
})

#' @rdname write-band
#' @export
setMethod('[<-',
signature('RGDAL2RasterBand', 'numeric', 'numeric'),
function(x, i, j, ..., value)
{
  writeRasterBand(x, i, j, ..., value = value)
})

#' @examples
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openDataset(f)
#' extent(x)
#' 
#' @rdname extent
#' @export
setMethod("extent",
signature("RGDAL2Dataset"),
function(object)
{
    x = RGDAL_GetRasterExtent(object@handle)
    res = newRGDAL2Geometry(x)
    setSRS(res, getSRS(object))
    res
})

#' @examples
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openRasterBand(f)
#' extent(x)
#' 
#' @rdname extent
#' @export
setMethod("extent",
signature("RGDAL2RasterBand"),
function(object)
{
    extent(object@dataset)
})

#' Raster blocks
#' 
#' Read and write raster blocks
#' 
#' @param x a raster band or dataset
#' @param i the block row
#' @param j the block column
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
#' \code{readBlock}: a matrix of raster data
#' 
#' @examples
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openRasterBand(f)
#' y = getBand(newDataset(nrow(x), ncol(x), dataType = "Int16",
#'                        driver = "GTiff", opts = c("TILED=YES"),
#'                        nosave = TRUE))
#'                        
#' for ( i in 1:nBlockRows(x) )
#'   for ( j in 1:nBlockCols(x) )
#'     writeBlock(y, i, j, readBlock(x, i, j))
#'     
#'  draw(y)
#' 
#' @rdname read-write-block
#' @export
readBlock = function(x, i, j)
{
  x = checkBand(x)
  RGDAL_ReadBlock(x@handle, i, j)
}

#' @param data an array or vector of data
#' @return \code{writeBlock}: the raster band invisibly
#' @rdname read-write-block
#' @export
writeBlock = function(x, i, j, data)
{
  x = checkBand(x)
  if ( RGDAL_WriteBlock(x@handle, i, j, data) )
    stop("Error writing band")
  invisible(x)
}

#' @return \code{nBlockRows}: number of blocks in y-direction
#' @rdname read-write-block
#' @export
nBlockRows = function(x)
{
  x = checkBand(x)
  nBlockDim(x)[1]
}

#' @return \code{nBlockCols}: number of blocks in x-direction
#' @rdname read-write-block
#' @export
nBlockCols = function(x)
{
  x = checkBand(x)
  nBlockDim(x)[2]
}

#' @return \code{nBlockDim}: number of blocks in x- and y- directions
#' @rdname read-write-block
#' @export
nBlockDim = function(x)
{
  x = checkBand(x)
  ceiling(dim(x) / getBlockSize(x))
}

#' Iterate over coordinate regions
#' 
#' An iterator over tiled coordinate ranges
#' 
#' @param b any object for which \code{\link{nrow}} and \code{\link{ncol}} are defined
#' @param tile.size the x, y dimensions of the tiles
#' @param native.indexing if true, use slower native indexing (see \code{\link{[<-,band}})
#' 
#' @details
#' When used with \code{\link{foreach}}, this function allows one
#' to iterator over the coordinates defining a set of tiles. The default
#' behavior is to call \code{\link{getBlockSize}}, but this can be
#' overriden, for example, when using an ordinary matrix. The parameter
#' \code{tile.size} is forced to length 2 so that a scalar can be
#' passed.
#' 
#' @return a list with elements \code{x} and \code{y}
#' 
#' @examples
#' a = matrix(rep(1:5, each = 5), 5, 5)
#' b = matrix(NA, 5, 5)
#' 
#' if (require(foreach)) {
#' invisible(
#' foreach(i = tileCoordIter(a, 2)) %do%
#' {
#'  b[i$y, i$x] = t(a[i$y, i$x])
#' })
#' 
#' show(a)
#' show(b)
#' 
#' x = newRasterBand(9, 9)
#' x[] = 1:81; x[]
#' y = foreach.tile(x, tile.size = c(3, 3)) %do%
#' {
#'  i$z = t(i$z)
#'  i # default combine requires we return an iterator
#' }
#' y[]
#' }
#' 
#' @rdname tile
#' @export
tileCoordIter = function(b, tile.size = getBlockSize(b))
{
  x = 1L
  y = 1L
  xx = ncol(b)
  yy = nrow(b)
  tile.size = rep(tile.size, length = 2)
  xby = tile.size[2L]
  yby = tile.size[1L]
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
  structure(list(nextElem = f), class = c('rgdal2TileIter', 'abstractiter', 'iter'))
}

#' @rdname tile
#' @export
tileIter = function(b, tile.size = getBlockSize(b), native.indexing = FALSE)
{
  x = 1L
  y = 1L
  xx = ncol(b)
  yy = nrow(b)
  tile.size = rep(tile.size, length = 2)
  xby = tile.size[2L]
  yby = tile.size[1L]
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

#' @details
#' \code{foreach.tile} creates a \code{\link{foreach}} object that can be used
#' to apply an expression of tiles of data. It uses \code{tileIter} internally.
#' The default \code{combine} function writes the output into the band returned
#' by the \code{init} function. Note that the block iterator (\code{i}) has both the raster
#' values and their x and y coordinates. You can therefore change where the block
#' of data is written to the output band by manipulating \code{i$x} and \code{i$y}.
#' 
#' @param out write output to this dataset
#' @param init initial input to combine
#' @param combine a function to combine result of expression with \code{init}
#' @param final a function that returns the final result
#' @param inorder process tiles in order?
#' 
#' @rdname tile
#' @export
foreach.tile = function(b,
                        out = newDataset(nrow(b), ncol(b)),
                        tile.size = getBlockSize(b),
                        init = getBand(out),
                        combine = NULL,
                        final = function(x) x,
                        inorder = FALSE,
                        native.indexing = FALSE)
{
  if ( is.null(combine) )
    combine = function(out, i)
    {
      out[i$y, i$x, native.indexing = i$native.indexing] = i$z
      return(out)
    }
  args = list()
  if ( inherits(b, 'RGDAL2RasterBand') )
  {
    args[['i']] = tileIter(b, tile.size, native.indexing) 
  }
  else
  {
    for ( i in 1:nband(b) )
    {
      args[[paste0('i', i)]] = tileIter(getBand(b, i), tile.size, native.indexing)
    }
  }
  args[['.init']] = init
  args[['.combine']] = combine
  args[['.final']] = final
  args[['.inorder']] = inorder
  do.call('foreach', args)
}

#' Get or set no data value
#' 
#' @param object a raster band
#' @param no.data.value the no data value
#' 
#' @details
#' If you pass a dataset object, the first band will be used.
#' \code{setNoDataValue} will return a non-zero value on error.
#' \code{getNoDataValue} will return NULL if the no data value is
#' not set.
#' 
#' @examples
#' f = system.file("example-data/gtopo30_gall.tif", package = "rgdal2")
#' x = openRasterBand(f)
#' getNoDataValue(x)
#' y = copyDataset(x)
#' getNoDataValue(y)
#' setNoDataValue(y, -1)
#' getNoDataValue(y)
#' f = system.file("example-data/bee.jpg", package = "rgdal2")
#' x = openDataset(f)
#' getNoDataValue(x)
#' 
#' @rdname get-set-nodatavalue
#' @export
getNoDataValue = function(object)
{
  object = checkBand(object)
  RGDAL_GetRasterNoDataValue(object@handle)
}

#' @rdname get-set-nodatavalue
#' @export
setNoDataValue = function(object, no.data.value)
{
  object = checkBand(object)
  RGDAL_SetRasterNoDataValue(object@handle, no.data.value)
}

#' Get and set metadata
#' 
#' @param object a raster object
#' @param metadata a vector of key=value strings
#' @param domain the metadata domain
#' 
#' @examples
#' x = newDataset(10, 10)
#' getMetadata(x)
#' setMetadata(x, c("key1=value1", "key2=value2"))
#' getMetadata(x)
#' getMetadata(x, "private")
#' setMetadata(x, c("key1=value1", "key2=value2"), "private")
#' getMetadata(x, "private")
#' 
#' @rdname get-set-metadata
#' @export
getMetadata = function(object, domain = "")
{
  object = checkDataset(object)
  RGDAL_GetMetadata(object@handle, domain)
}

#' @rdname get-set-metadata
#' @export
setMetadata = function(object, metadata, domain = "")
{
  object = checkDataset(object)
  RGDAL_SetMetadata(object@handle, metadata, domain)
}
