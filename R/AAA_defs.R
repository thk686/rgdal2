#
# Copyright Timothy H. Keitt
#

#
# GDAL raster classes
#
setClass('RGDAL2Dataset', slots = c(handle = 'externalptr'))
setClass('RGDAL2RasterBand', slots = c(handle = 'externalptr', dataset = 'RGDAL2Dataset'))
setClass('RGDAL2RasterMask', contains = 'RGDAL2RasterBand', slots = c(flag = 'integer'))

#
# OGR classes
#
setClass('RGDAL2Datasource', slots = c(handle = 'externalptr'))
setClass('RGDAL2Layer', slots = c(handle = 'externalptr', datasource = 'RGDAL2Datasource'))
setClass('RGDAL2SQLLayer', contains = 'RGDAL2Layer', slots = c(sql = 'character'))
setClass('RGDAL2Feature', slots = c(handle = 'externalptr', layer = 'RGDAL2Layer'))
setClass('RGDAL2Geometry', slots = c(handle = 'externalptr'))
setClass('RGDAL2LayerGeometry', contains = "RGDAL2Geometry", slots = c(layer = 'RGDAL2Layer'))

#
# SRS Classes
#
setClass('RGDAL2SpatialRef', slots = c(handle = 'externalptr'))

#
# OGR Generics
#
setGeneric("extent",
function(object)
{
    standardGeneric("extent")
})

setGeneric("properties",
function(object)
{
    standardGeneric("properties")
})


#
# SRS Generics
#
setGeneric("getSRS",
function(object)
{
    standardGeneric("getSRS")
})

setGeneric("setSRS",
function(object, SRS)
{
    standardGeneric("setSRS")
})

setGeneric("reproject",
function(object, SRS, ...)
{
    standardGeneric("reproject")
})
