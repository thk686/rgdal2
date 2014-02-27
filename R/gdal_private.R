#
# Copyright Timothy H. Keitt
#

#' @include defs.R
NULL

newRGDAL2Dataset = function(handle)
{
    reg.finalizer(handle, RGDAL_Close, TRUE)
    new("RGDAL2Dataset", handle = handle)
}

newRGDAL2RasterBand = function(handle, dataset)
{
    new("RGDAL2RasterBand", handle = handle, dataset = dataset)
}

newRGDAL2RasterMask = function(handle, dataset, flag)
{
    new("RGDAL2RasterMask", handle = handle, dataset = dataset, flag = flag)
}

readDataset = function(x, i, j, k = 1L:nband(x), ii = i, jj = j, drop = TRUE)
{
    nRowsIn = diff(range(i)) + 1L
    nColsIn = diff(range(j)) + 1L
    nRowsOut = diff(range(ii)) + 1L
    nColsOut = diff(range(jj)) + 1L
    res = RGDAL_ReadDataset(x@handle, min(j) - 1L, min(i) - 1L,
                            nColsIn, nRowsIn, nColsOut, nRowsOut, as.integer(k))
    res = res[ii - min(ii) + 1L, jj - min(jj) + 1L,, drop = FALSE]
    if ( drop ) drop(res) else res
}

readRasterBand = function(x, i, j, ii = i, jj = j, drop = TRUE, use.mask = TRUE)
{
    nRowsIn = diff(range(i)) + 1L
    nColsIn = diff(range(j)) + 1L
    nRowsOut = diff(range(ii)) + 1L
    nColsOut = diff(range(jj)) + 1L
    res = RGDAL_ReadRasterData(x@handle, min(j) - 1L, min(i) - 1L,
                               nColsIn, nRowsIn, nColsOut, nRowsOut)
    res = res[ii - min(ii) + 1L, jj - min(jj) + 1L, drop = FALSE]
    if ( use.mask && bitwAnd(RGDAL_GetMaskFlags(x@handle), 8L) )
    {
        mh = RGDAL_GetMaskBand(x@handle)
        mv = RGDAL_ReadRasterData(mh, min(j) - 1L, min(i) - 1L,
                                  nColsIn, nRowsIn, nColsOut, nRowsOut)
        mv = mv[ii - min(ii) + 1L, jj - min(jj) + 1L, drop = FALSE]
        res[mv == 0] = NA
    }
    attr(res, "class") = NULL
    if ( drop ) drop(res) else res
}

writeRasterBand = function(x, i, j, ii = i, jj = j, native.indexing = FALSE, value)
{
    nRowsOut = diff(range(i)) + 1L
    nColsOut = diff(range(j)) + 1L
    if ( native.indexing )
    {
        buf = readRasterBand(x, min(i):max(i), min(j):max(j), drop = FALSE, use.mask = FALSE)
        buf[i - min(i) + 1L, j - min(j) + 1L] = value
        RGDALWriteRasterBand(x@handle, buf, min(j) - 1L, min(i) - 1L, nColsOut, nRowsOut)
    }
    else
    {
        if ( ! is.matrix(value) )
        {
            value = matrix(value, diff(range(ii)) + 1L, diff(range(jj)) + 1L)
        }
        RGDAL_writeRasterData(x@handle, value, min(j) - 1L, min(i) - 1L, nColsOut, nRowsOut)
    }
    invisible(x)      
}

rotateImageData = function(x)
{
  t(x[rev(1L:nrow(x)),])
}

asRanked = function(x, ties.method = "first")
{
  x[is.finite(x)] = rank(x[is.finite(x)], ties.method)
  return(x)
}

checkBand = function(x)
{
  if ( inherits(x, "RGDAL2RasterBand") )
    x
  else
    getBand(x)
}

checkDataset = function(x)
{
    if ( inherits(x, "RGDAL2Dataset") )
        x
    else
        getDataset(x)
}

defaultRGDALPalette = function(n = 1024)
{
    colorRampPalette(rev(brewer.pal(11, 'Spectral')))(n)
}

scale01 = function(x, na.rm = TRUE)
{
    mm = range(x, na.rm = na.rm)
    (x - mm[1]) / diff(mm)
}

region2Indices = function(x, i)
{
    dims = dim(x)
    i = reproject(i, getSRS(x))
    pts = getPoints(extent(i))
    tpts = RGDAL_ApplyGeoTransform(x@dataset@handle, pts, 1L)
    tpts = lapply(tpts, abs)
    i = floor(min(tpts$y)):floor(max(tpts$y))
    j = floor(min(tpts$x)):floor(max(tpts$x))
    i = i[i > 0 & i <= dims[1]]
    j = j[j > 0 & j <= dims[2]]
    list(i = i, j = j)
}



