#include "pkg_config.h"
#include <gdal.h>
#include <gdal_alg.h>
#include <ogr_api.h>
#include <ogr_srs_api.h>
#include <cpl_conv.h>
#include <Rcpp.h>

using namespace Rcpp;

//
// I am trying to stick to the GDAL public interface,
// so these need to be defined solely to have a type
// for the XPtr template, and for readability.
//
class GDALDataset;
class GDALRasterBand;
class OGRDataSource;
class OGRLayer;
class OGRGeometry;
class OGRSpatialReference;
class OGRFeature;

//
// Kludge until I come up with something better
//
#if SIZEOF_INT * CHAR_BIT != 32
#error "Integers must be 32bit"
#endif

#if SIZEOF_DOUBLE * CHAR_BIT != 64
#error "Double type must be 64bit"
#endif

//
// Stop on error (error handler will print message)
//
#define _(a) if ((a) != 0) stop(#a)

//
// Returns errors to R
// Note only case 4 actually returns immediately
// Lower error codes are recoverable
//
static void __err_handler(CPLErr eErrClass, int err_no, const char *msg)
{
    switch ( eErrClass )
    {
        case 0:
            break;
        case 1:
        case 2:
            Rf_warning("GDAL Message %d: %s\n", err_no, msg);
            break;
        case 3:
            Rf_warning("GDAL Error %d: %s\n", err_no, msg);
            break;
        case 4:
            Rf_warning("GDAL Error %d: %s\n", err_no, msg);
            stop("Unrecoverable error\n");
            break;        
        default:
            Rf_warning("Received invalid error class %d (errno %d: %s)\n", eErrClass, err_no, msg);
            break;
    }
    return;
}

template<class T>
SEXP wrapHandle(void* x)
{
  if ( !x ) stop("Function returned null handle\n");
  return XPtr<T>((T*) x, false);
}

template<class T>
void* unwrapHandle(SEXP x)
{
  XPtr<T> y(x);
  void* z = (void*) &*y;
  if ( !z ) stop("Null handle passed to function\n");
  return z;
}

// [[Rcpp::export]]
void GDALInit()
{
    CPLSetErrorHandler((CPLErrorHandler)__err_handler);
    GDALAllRegister();
    OGRRegisterAll();
}

// [[Rcpp::export]]
SEXP GDALOpen(const char* file,
              bool readonly = true,
              bool shared = true)
{
  GDALAccess access = readonly ? GA_ReadOnly : GA_Update;
  GDALDatasetH h = shared ? GDALOpenShared(file, access) : GDALOpen(file, access);
  return wrapHandle<GDALDataset>(h);
}

// [[Rcpp::export]]
void GDALClose(SEXP x)
{
  GDALDatasetH h = unwrapHandle<GDALDataset>(x);
  GDALClose(h);
}

// [[Rcpp::export]]
bool isNullPtr(SEXP x)
{
  Rf_warning("Checked for null pointer\n"); // trying to deprecate this
  return EXTPTR_PTR(x) == NULL;
}

// [[Rcpp::export]]
const char* RGDAL_GDALGetDescription(SEXP x)
{
  GDALDatasetH h = unwrapHandle<GDALDataset>(x);
  return GDALGetDescription(h);  
}

// [[Rcpp::export]]
SEXP RGDAL_GDALGetRasterBand(SEXP x, int n)
{
  GDALDatasetH h = unwrapHandle<GDALDataset>(x);
  GDALRasterBandH b = GDALGetRasterBand(h, n);
  return b ?
    wrapHandle<GDALRasterBand>(b) :
    R_NilValue;
}

// [[Rcpp::export]]
int GDALGetRasterXSize(SEXP x)
{
  GDALDatasetH h = unwrapHandle<GDALDataset>(x);
  return GDALGetRasterXSize(h);
}

// [[Rcpp::export]]
int GDALGetRasterYSize(SEXP x)
{
  GDALDatasetH h = unwrapHandle<GDALDataset>(x);
  return GDALGetRasterYSize(h);
}

// [[Rcpp::export]]
int GDALGetRasterCount(SEXP x)
{
  GDALDatasetH h = unwrapHandle<GDALDataset>(x);
  return GDALGetRasterCount(h);
}

// [[Rcpp::export]]
int GDALGetRasterBandXSize(SEXP x)
{
  GDALRasterBandH h = unwrapHandle<GDALRasterBand>(x);
  return GDALGetRasterBandXSize(h);
}

// [[Rcpp::export]]
int GDALGetRasterBandYSize(SEXP x)
{
  GDALRasterBandH h = unwrapHandle<GDALRasterBand>(x);
  return GDALGetRasterBandYSize(h);
}

// [[Rcpp::export]]
const char* versionInfo(const char* what = "--version")
{
  return GDALVersionInfo(what);
}

// [[Rcpp::export]]
void RGDAL_CleanupAll()
{
    OGRCleanupAll();
    OSRCleanup();
}

// [[Rcpp::export]]
SEXP RGDALCreateDataset(const char* driver, const char* fname,
                        int nrow, int ncol, int nbands, const char* type)
{
  GDALDataType dtype = GDALGetDataTypeByName(type);
  if ( !dtype ) stop("Unknown data type specified");
  GDALDriverH hDR = GDALGetDriverByName(driver);
  if ( !hDR ) stop("Invalid GDAL driver\n");
  GDALDatasetH ds = GDALCreate(hDR, fname, ncol, nrow, nbands, dtype, NULL);
  if ( ds ) GDALFlushCache(ds);
  return wrapHandle<GDALDataset>(ds);
}

// [[Rcpp::export]]
SEXP RGDAL_CreateCopy(SEXP x, const char* fname, const char* dname)
{
    GDALDatasetH h = unwrapHandle<GDALDataset>(x);
    GDALDriverH hD = GDALGetDriverByName(dname);
    GDALDatasetH res = GDALCreateCopy(hD, fname, h, 0, NULL, NULL, NULL);
    return wrapHandle<GDALDataset>(res);
}

// [[Rcpp::export]]
SEXP RGDAL_GetGeoTransform(SEXP x)
{
    NumericVector res(6);
    GDALDatasetH h = unwrapHandle<GDALDataset>(x);
    _(GDALGetGeoTransform(h, &res[0]));
    return res;
}

// [[Rcpp::export]]
int RGDAL_SetGeoTransform(SEXP x, SEXP y)
{
    NumericVector gt(y);
    if ( gt.size() != 6 ) return 1;
    GDALDatasetH h = unwrapHandle<GDALDataset>(x); 
    return GDALSetGeoTransform(h, &gt[0]);
}

// [[Rcpp::export]]
SEXP GDALGetMaskBand(SEXP x)
{
  GDALRasterBandH h = unwrapHandle<GDALRasterBand>(x);
  GDALRasterBandH res = GDALGetMaskBand(h);
  return wrapHandle<GDALRasterBandH>(res);
}

// [[Rcpp::export]]
int GDALGetMaskFlags(SEXP x)
{
  GDALRasterBandH h = unwrapHandle<GDALRasterBand>(x);
  return GDALGetMaskFlags(h);
}

// [[Rcpp::export]]
IntegerVector GDALGetBlockSize(SEXP x)
{
  GDALDatasetH h = unwrapHandle<GDALDataset>(x);
  IntegerVector res(2);
  GDALGetBlockSize(h, &res[1], &res[0]);
  return res;
}

// [[Rcpp::export]]
SEXP readRasterData(SEXP rb,
                    int x, int y,
                    int xszin, int yszin,
                    int xszout, int yszout,
                    int pixsp = 0, int lnsp = 0)
{
  GDALRasterBandH h = unwrapHandle<GDALRasterBand>(rb);
  NumericVector buf(xszout * yszout);
  _(GDALRasterIO(h, GF_Read, x, y, xszin, yszin, &buf[0],
                xszout, yszout, GDT_Float64, pixsp, lnsp));
  double scale = GDALGetRasterScale(h, NULL), 
         offset = GDALGetRasterOffset(h, NULL),
         nodata = GDALGetRasterNoDataValue(h, NULL);
  NumericMatrix res(yszout, xszout);
  for ( int xx = 0; xx != xszout; ++xx )
    for ( int yy = 0; yy != yszout; ++yy )
    {
      double pixv = buf[yy * xszout + xx];
      res[xx * yszout + yy] =
        pixv == nodata ? NA_REAL : scale * pixv + offset;
    }
  return res;
}

// [[Rcpp::export]]
int writeRasterData(SEXP rb,
                    NumericMatrix raster,
                    int x, int y,
                    int xszout, int yszout,
                    int pixsp = 0, int lnsp = 0)
{
  GDALDatasetH h = unwrapHandle<GDALRasterBand>(rb);
  int xszin = raster.cols(), yszin = raster.rows();
  double scale = GDALGetRasterScale(h, NULL), 
         offset = GDALGetRasterOffset(h, NULL),
         nodata = GDALGetRasterNoDataValue(h, NULL);
  NumericVector buf(xszin * yszin);
  for ( int xx = 0; xx != xszin; ++xx )
    for ( int yy = 0; yy != yszin; ++yy )
    {
      double pixv = raster[xx * yszin + yy];
      buf[yy * xszin + xx] =
        pixv == NA_REAL ? nodata : (pixv - offset) / scale;
    }
  return GDALRasterIO(h, GF_Write, x, y, xszout, yszout, &buf[0],
                      xszin, yszin, GDT_Float64, pixsp, lnsp);
}

// [[Rcpp::export]]
SEXP RGDAL_ReadBlock(SEXP band, int i, int j)
{
    GDALRasterBandH hB = unwrapHandle<GDALRasterBand>(band);
    int xsz, ysz;
    GDALGetBlockSize(hB, &xsz, &ysz);
    double scale = GDALGetRasterScale(hB, NULL), 
           offset = GDALGetRasterOffset(hB, NULL),
           nodata = GDALGetRasterNoDataValue(hB, NULL);
    GDALDataType dt = GDALGetRasterDataType(hB);
    switch ( dt )
    {
        case GDT_Int32:
          {
            IntegerVector buf(xsz * ysz, NA_INTEGER);
            GDALReadBlock(hB, j - 1, i - 1, &buf[0]);
            IntegerMatrix res(ysz, xsz);
            for ( int x = 0; x != xsz; ++x )
              for ( int y = 0; y != ysz; ++y )
              {
                double pixv = buf[y * xsz + x];
                res[x * ysz + y] =
                  pixv == nodata ? NA_INTEGER : scale * pixv + offset;
              }
            return res;
          }
        case GDT_Float64:
          {
            NumericVector buf(xsz * ysz, NA_REAL);
            GDALReadBlock(hB, j - 1, i - 1, &buf[0]);
            NumericMatrix res(ysz, xsz);
            for ( int x = 0; x != xsz; ++x )
              for ( int y = 0; y != ysz; ++y )
              {
                double pixv = buf[y * xsz + x];
                res[x * ysz + y] =
                  pixv == nodata ? NA_REAL : scale * pixv + offset;
              }
            return res;
          }
        case GDT_Byte:
          {
            RawVector buf(xsz * ysz);
            GDALReadBlock(hB, j - 1, i - 1, &buf[0]);
            RawMatrix res(ysz, xsz);
            for ( int x = 0; x != xsz; ++x )
              for ( int y = 0; y != ysz; ++y )
              {
                double pixv = buf[y * xsz + x];
                res[x * ysz + y] = scale * pixv + offset;
              }
            return res;
          }
        case GDT_Int16:
          {
            std::vector<int16_t> buf(xsz * ysz);
            GDALReadBlock(hB, j - 1, i - 1, &buf[0]);
            IntegerMatrix res(ysz, xsz);
            for ( int x = 0; x != xsz; ++x )
              for ( int y = 0; y != ysz; ++y )
              {
                double pixv = buf[y * xsz + x];
                res[x * ysz + y] =
                  pixv == nodata ? NA_INTEGER : scale * pixv + offset;
              }
            return res;
          }
        default:
            stop("Unsupported data type in block read\n");
    }
}

// [[Rcpp::export]]
int RGDAL_WriteBlock(SEXP band, int i, int j, SEXP blk)
{
    GDALRasterBandH hB = unwrapHandle<GDALRasterBand>(band);
    int xsz, ysz;
    GDALGetBlockSize(hB, &xsz, &ysz);
    if ( Rf_length(blk) < xsz * ysz )
        stop("Input does not match block size\n");
    GDALDataType dt = GDALGetRasterDataType(hB);
    switch ( dt )
    {
        case GDT_Int32:
            return GDALWriteBlock(hB, j - 1, i - 1, INTEGER(blk));
        case GDT_Float64:
            return GDALWriteBlock(hB, j - 1, i - 1, REAL(blk));
        case GDT_Byte:
            return GDALWriteBlock(hB, j - 1, i - 1, RAW(blk));
        case GDT_Int16:
          {
            int16_t* buf = (int16_t*) R_alloc(xsz * ysz, 2);
            for ( size_t k = 0; k != xsz * ysz; ++k )
                buf[k] = INTEGER(blk)[k];
            return GDALWriteBlock(hB, j - 1, i - 1, &(buf[0]));
          }
        default:
            stop("Unsupported data type in block read\n");
    }
}

// [[Rcpp::export]]
SEXP RGDAL_OGROpen(const char* file, int readonly)
{
  OGRDataSourceH res = OGROpen(file, !readonly, NULL);
  if ( !res ) stop("Cannot open file\n");
  return wrapHandle<OGRDataSource>(res);
}

// [[Rcpp::export]]
void OGRReleaseDataSource(SEXP ds)
{
  OGRDataSourceH h = unwrapHandle<OGRDataSource>(ds);
  OGRReleaseDataSource(h);
}

// [[Rcpp::export]]
int OGR_DS_GetLayerCount(SEXP ds)
{
  OGRDataSourceH h = unwrapHandle<OGRDataSource>(ds);
  return OGR_DS_GetLayerCount(h);
}

// [[Rcpp::export]]
const char* OGR_DS_GetName(SEXP ds)
{
  OGRDataSourceH h = unwrapHandle<OGRDataSource>(ds);
  return OGR_DS_GetName(h);
}

// [[Rcpp::export]]
const char* RGDAL_GetDSDriverName(SEXP ds)
{
    OGRDataSourceH h = unwrapHandle<OGRDataSource>(ds);
    OGRSFDriverH hDr = OGR_DS_GetDriver(h);
    return OGR_Dr_GetName(hDr);
}

// [[Rcpp::export]]
SEXP OGR_DS_GetLayer(SEXP ds, int i)
{
  OGRDataSourceH h = unwrapHandle<OGRDataSource>(ds);
  OGRLayerH res = OGR_DS_GetLayer(h, i);
  return wrapHandle<OGRLayer>(res);
}

// [[Rcpp::export]]
SEXP RGDAL_GetRasterExtent(SEXP ds)
{
    GDALDatasetH hDS = unwrapHandle<GDALDataset>(ds);
  
    double transf[6],
           geox, geoy,
           xmin = 0, ymin = 0,
           xmax = GDALGetRasterXSize(hDS),
           ymax = GDALGetRasterYSize(hDS);

    OGRGeometryH hGeom = OGR_G_CreateGeometry(wkbPolygon),
                 hRing = OGR_G_CreateGeometry(wkbLinearRing);
    
    if ( GDALGetGeoTransform(hDS, &(transf[0])) )
    {
      OGR_G_AddPoint_2D(hRing, xmin, ymin);
      OGR_G_AddPoint_2D(hRing, xmin, ymax);
      OGR_G_AddPoint_2D(hRing, xmax, ymax);
      OGR_G_AddPoint_2D(hRing, xmax, ymin);
      OGR_G_AddPoint_2D(hRing, xmin, ymin);
    }
    else
    {
      GDALApplyGeoTransform(&(transf[0]), xmin, ymin, &geox, &geoy);
      OGR_G_AddPoint_2D(hRing, geox, geoy);
      GDALApplyGeoTransform(&(transf[0]), xmin, ymax, &geox, &geoy);
      OGR_G_AddPoint_2D(hRing, geox, geoy);
      GDALApplyGeoTransform(&(transf[0]), xmax, ymax, &geox, &geoy);
      OGR_G_AddPoint_2D(hRing, geox, geoy);
      GDALApplyGeoTransform(&(transf[0]), xmax, ymin, &geox, &geoy);
      OGR_G_AddPoint_2D(hRing, geox, geoy);
      GDALApplyGeoTransform(&(transf[0]), xmin, ymin, &geox, &geoy);
      OGR_G_AddPoint_2D(hRing, geox, geoy);
    }
    _(OGR_G_AddGeometry(hGeom, hRing));
    return wrapHandle<OGRGeometry>(hGeom);
}

// [[Rcpp::export]]
const char* OGR_L_GetName(SEXP lyr)
{
  OGRLayerH h = unwrapHandle<OGRLayer>(lyr);
  return OGR_L_GetName(h);
}

// [[Rcpp::export]]
SEXP OGR_DS_GetLayerByName(SEXP ds, const char* lnam)
{
  OGRDataSourceH h = unwrapHandle<OGRDataSource>(ds);
  OGRLayerH res = OGR_DS_GetLayerByName(h, lnam);
  return wrapHandle<OGRLayer>(res);
}

static OGRGeometryH extentToGeom(OGREnvelope env)
{
    OGRGeometryH res = OGR_G_CreateGeometry(wkbPolygon),
                 ring = OGR_G_CreateGeometry(wkbLinearRing);
    OGR_G_AddPoint_2D(ring, env.MinX, env.MinY);
    OGR_G_AddPoint_2D(ring, env.MinX, env.MaxY);
    OGR_G_AddPoint_2D(ring, env.MaxX, env.MaxY);
    OGR_G_AddPoint_2D(ring, env.MaxX, env.MinY);
    OGR_G_AddPoint_2D(ring, env.MinX, env.MinY);
    OGR_G_AddGeometry(res, ring); // copy?
    return res; // leak?
}

// [[Rcpp::export]]
SEXP RGDAL_GetLayerEnv(SEXP layer)
{
    OGRLayerH hL = unwrapHandle<OGRLayer>(layer);
    OGREnvelope env;
    OGR_L_GetExtent(hL, &env, 1);
    OGRGeometryH res = extentToGeom(env);
    return wrapHandle<OGRGeometry>(res);
}

// [[Rcpp::export]]
const char* GDALGetProjectionRef(SEXP ds)
{
  GDALDatasetH h = unwrapHandle<GDALDataset>(ds);
  GDALGetProjectionRef(h);
}

// [[Rcpp::export]]
SEXP RGDAL_OSRNewSpatialReference(const char* srss)
{
  OGRSpatialReferenceH h = OSRNewSpatialReference(srss);
  return wrapHandle<OGRSpatialReference>(h);
}

// [[Rcpp::export]]
int RGDAL_OSRSetFromUserInput(SEXP srs, const char* def)
{
  OGRSpatialReferenceH h = unwrapHandle<OGRSpatialReference>(srs);
  return OSRSetFromUserInput(h, def);
}

// [[Rcpp::export]]
void RGDAL_OSRRelease(SEXP srs)
{
  OGRSpatialReferenceH h = unwrapHandle<OGRSpatialReference>(srs);
  OSRRelease(h);
}

// [[Rcpp::export]]
SEXP RGDAL_GetWKT(SEXP srs)
{
    OGRSpatialReferenceH hSR = unwrapHandle<OGRSpatialReference>(srs);
    char* wktstring;
    _(OSRExportToPrettyWkt(hSR, &wktstring, 0));
    SEXP res = Rf_ScalarString(Rf_mkChar(wktstring));
    CPLFree(wktstring);
    return res;
}

// [[Rcpp::export]]
SEXP RGDAL_GetPROJ4(SEXP srs)
{
    OGRSpatialReferenceH hSRS = unwrapHandle<OGRSpatialReference>(srs);
    if ( ! hSRS )
        return(Rf_ScalarString(Rf_mkChar("Null SRS")));
    char* proj4string;
    _(OSRExportToProj4(hSRS, &proj4string));
    SEXP res = Rf_ScalarString(Rf_mkChar(proj4string));
    CPLFree(proj4string);
    return res;
}

// [[Rcpp::export]]
int GDALSetProjection(SEXP ds, const char* proj)
{
  GDALDatasetH h = unwrapHandle<GDALDataset>(ds);
  return GDALSetProjection(h, proj);
}

// [[Rcpp::export]]
int RGDAL_OGR_L_GetFeatureCount(SEXP lyr)
{
  OGRLayerH h = unwrapHandle<OGRLayer>(lyr);
  return OGR_L_GetFeatureCount(h, 1);
}

// [[Rcpp::export]]
SEXP RGDAL_OGR_L_GetSpatialFilter(SEXP lyr)
{
  OGRLayerH h = unwrapHandle<OGRLayer>(lyr);
  OGRGeometryH res = OGR_L_GetSpatialFilter(h);
  return res ?
    wrapHandle<OGRGeometry>(res) : 
    R_NilValue;
}

// [[Rcpp::export]]
void RGDAL_OGR_L_ResetReading(SEXP lyr)
{
  OGRLayerH h = unwrapHandle<OGRLayer>(lyr);
  OGR_L_ResetReading(h);
}

// [[Rcpp::export]]
SEXP RGDAL_OGR_L_GetSpatialRef(SEXP lyr)
{
  OGRLayerH h = unwrapHandle<OGRLayer>(lyr);
  OGRSpatialReferenceH res = OGR_L_GetSpatialRef(h);
  return res ?
    wrapHandle<OGRSpatialReference>(res) :
    R_NilValue;
}

// [[Rcpp::export]]
void RGDAL_OGR_G_DestroyGeometry(SEXP geom)
{
  OGRGeometryH h = unwrapHandle<OGRGeometry>(geom);
  OGR_G_DestroyGeometry(h);
}

// [[Rcpp::export]]
SEXP OGR_G_GetSpatialReference(SEXP geom)
{
  OGRGeometryH h = unwrapHandle<OGRGeometry>(geom);
  OGRSpatialReferenceH res = OGR_G_GetSpatialReference(h);
  return wrapHandle<OGRSpatialReference>(res);
}

// [[Rcpp::export]]
SEXP OSRClone(SEXP sref)
{
  OGRSpatialReferenceH h = unwrapHandle<OGRSpatialReference>(sref);
  OGRSpatialReferenceH res = OSRClone(h);
  return wrapHandle<OGRSpatialReference>(res);
}

// [[Rcpp::export]]
void RGDAL_OGR_G_AssignSpatialReference(SEXP geom, SEXP srs)
{
  OGRGeometryH h1 = unwrapHandle<OGRGeometry>(geom);
  OGRSpatialReferenceH h2 = unwrapHandle<OGRSpatialReference>(srs);
  OGR_G_AssignSpatialReference(h1, h2);
}

// [[Rcpp::export]]
int OGR_G_GetCoordinateDimension(SEXP geom)
{
  OGRGeometryH h = unwrapHandle<OGRGeometry>(geom);
  return OGR_G_GetCoordinateDimension(h);
}

// These are carried over from my first cut with SWIG
// They have not been coverted to Rcpp
static void attachPoints(SEXP res, OGRGeometryH hG)
{
    SEXP x, y, z;
    int stride = sizeof(double),
        n = OGR_G_GetPointCount(hG);
    if ( n == 0 ) return;
    x = PROTECT(Rf_allocVector(REALSXP, n));
    y = PROTECT(Rf_allocVector(REALSXP, n));
    z = PROTECT(Rf_allocVector(REALSXP, n));
    OGR_G_GetPoints(hG, REAL(x), stride, REAL(y), stride, REAL(z), stride);
    switch ( Rf_length(res) )
    {
      case 3:;
        SET_VECTOR_ELT(res, 0, x);  
        SET_VECTOR_ELT(res, 1, y);
        SET_VECTOR_ELT(res, 2, z);
        break;
      case 2:;
        SET_VECTOR_ELT(res, 0, x);  
        SET_VECTOR_ELT(res, 1, y);
        break;
      default:;
        Rf_warning("Empty geometry\n");
        break;
    }
    UNPROTECT(3);
    return;
}

static void attachNames(SEXP x)
{
    int dim = Rf_length(x);
    SEXP names = PROTECT(Rf_allocVector(STRSXP, dim));
    switch ( dim )
    {
      case 3:;
        SET_STRING_ELT(names, 2, Rf_mkChar("z"));
      case 2:;
        SET_STRING_ELT(names, 1, Rf_mkChar("y"));
        SET_STRING_ELT(names, 0, Rf_mkChar("x"));
        break;
      default:;
        Rf_warning("Empty geometry\n");      
        break;
    }
    Rf_setAttrib(x, R_NamesSymbol, names);
    UNPROTECT(1);
    return;
}

static SEXP GetPointsInternal(OGRGeometryH hG)
{
    SEXP res;
    int n = OGR_G_GetGeometryCount(hG);
    switch ( n )
    {
      case 0:
        {
          int dim = OGR_G_GetCoordinateDimension(hG);   
          res = PROTECT(Rf_allocVector(VECSXP, dim));
          attachPoints(res, hG);
          attachNames(res);
          UNPROTECT(1);
        }
        break;
      case 1:
        {
          OGRGeometryH hR = OGR_G_GetGeometryRef(hG, 0);
          res = GetPointsInternal(hR);
        }
        break;
      default:
        {
          res = PROTECT(Rf_allocVector(VECSXP, n));
          for ( size_t i = 0; i != n; ++i )
          {
              OGRGeometryH hR = OGR_G_GetGeometryRef(hG, i);
              SET_VECTOR_ELT(res, i, GetPointsInternal(hR));
          }
          UNPROTECT(1);
        }
        break;
    }
    return res;
}

// [[Rcpp::export]]
SEXP RGDAL_GetPoints(SEXP geom)
{
  OGRGeometryH h = unwrapHandle<OGRGeometry>(geom);
  return GetPointsInternal(h);
}

// [[Rcpp::export]]
void RGDAL_DumpGeometry(SEXP geom, const char* fname)
{
    OGRGeometryH hG = unwrapHandle<OGRGeometry>(geom);
    FILE* f = std::fopen(fname, "w");
    OGR_G_DumpReadable(hG, f, "");
    std::fclose(f);
}

// [[Rcpp::export]]
SEXP RGDAL_ExecSQL(SEXP ds, const char* sql)
{
  OGRDataSourceH hDS = unwrapHandle<OGRDataSource>(ds);
  OGRLayerH res = OGR_DS_ExecuteSQL(hDS, sql, NULL, NULL);
  return res ? wrapHandle<OGRLayer>(res) : R_NilValue;
}

// [[Rcpp::export]]
void OGR_DS_ReleaseResultSet(SEXP ds, SEXP lyr)
{
  OGRDataSourceH h1 = unwrapHandle<OGRDataSource>(ds);
  OGRLayerH h2 = unwrapHandle<OGRLayer>(lyr);
  OGR_DS_ReleaseResultSet(h1, h2);
}

// [[Rcpp::export]]
SEXP RGDAL_GetGeomEnv(SEXP geom)
{
    OGRGeometryH hG = unwrapHandle<OGRGeometry>(geom);
    OGREnvelope env;
    OGR_G_GetEnvelope(hG, &env);
    OGRGeometryH res = extentToGeom(env);
    return wrapHandle<OGRGeometry>(res);
}

// [[Rcpp::export]]
SEXP RGDAL_GetFIDs(SEXP lyr)
{
    OGRLayerH hL = unwrapHandle<OGRLayer>(lyr);
    OGR_L_ResetReading(hL);
    int n = OGR_L_GetFeatureCount(hL, 1);
    NumericVector res(n);
    for ( int i = 0; i != n; ++i )
    {
        OGRFeatureH hF = OGR_L_GetNextFeature(hL);
        res[i] = (double) OGR_F_GetFID(hF);
    }
    OGR_L_ResetReading(hL);
    return res;
}

// [[Rcpp::export]]
SEXP RGDAL_GetFeature(SEXP lyr, double index)
{
    OGRLayerH hL = unwrapHandle<OGRLayer>(lyr);
    OGRFeatureH res = OGR_L_GetFeature(hL, (long) index);
    return wrapHandle<OGRFeature>(res);
}

// [[Rcpp::export]]
SEXP OGR_F_GetGeometryRef(SEXP feat)
{
  OGRFeatureH h = unwrapHandle<OGRFeature>(feat);
  OGRGeometryH res = OGR_F_GetGeometryRef(h);
  return wrapHandle<OGRGeometry>(res);
}

// [[Rcpp::export]]
std::string RGDAL_OGR_G_GetGeometryType(SEXP geom)
{
  OGRGeometryH hG = unwrapHandle<OGRGeometry>(geom);
  return std::string(OGR_G_GetGeometryName(hG));  // needs to copy
}

static OGRwkbGeometryType typeFromName(std::string name)
{
  std::transform(name.begin(), name.end(), name.begin(), ::tolower);
  if ( name == "unknown" ) return wkbUnknown;   
  if ( name == "point" ) return wkbPoint;
  if ( name == "linestring") return wkbLineString;
  if ( name == "polygon" ) return wkbPolygon;
  if ( name == "multipoint") return wkbMultiPoint;
  if ( name == "multilinestring" ) return wkbMultiLineString;
  if ( name == "multipolygon" ) return wkbMultiPolygon;
  if ( name == "geometrycollection") return wkbGeometryCollection;
  if ( name == "none" ) return wkbNone;
  if ( name == "linearring" ) return wkbLinearRing;
  if ( name == "point25d") return wkbPoint25D;	
  if ( name == "linestring25d" ) return wkbLineString25D;
  if ( name == "polygon25d" ) return wkbPolygon25D;
  if ( name == "multipoint25d" ) return wkbMultiPoint25D;
  if ( name == "multilinestring25d" ) return wkbMultiLineString25D;
  if ( name == "multipolgon25d" ) return wkbMultiPolygon25D;
  if ( name == "geometrycollection25d" ) return wkbGeometryCollection25D;	
  stop("Invalid geometry type\n");
}

// [[Rcpp::export]]
SEXP RGDAL_OGR_L_GetNextFeature(SEXP lyr)
{
  OGRLayerH h = unwrapHandle<OGRLayer>(lyr);
  OGRFeatureH res = OGR_L_GetNextFeature(h);
  return res ?
      wrapHandle<OGRFeature>(res) :
      R_NilValue;
}

// [[Rcpp::export]]
int GetLayerFieldCount(SEXP lyr)
{
  OGRLayerH h = unwrapHandle<OGRLayer>(lyr);
  OGRFeatureDefnH f = OGR_L_GetLayerDefn(h);
  return f ? OGR_FD_GetFieldCount(f) : 0;
}

// [[Rcpp::export]]
SEXP GetFieldNames(SEXP lyr)
{
  CharacterVector res;
  OGRLayerH h = unwrapHandle<OGRLayer>(lyr);
  OGRFeatureDefnH f = OGR_L_GetLayerDefn(h);
  if ( ! f ) stop("Cannot get field definitions\n");
  for ( int i = 0; i != OGR_FD_GetFieldCount(f); ++i )
  {
    OGRFieldDefnH fld = OGR_FD_GetFieldDefn(f, i);
    if ( ! fld ) stop("Error retrieving field definition\n");
    const char* fnam = OGR_Fld_GetNameRef(fld);
    res.push_back(fnam);
  }
  return res;
}

// [[Rcpp::export]]
const char* RGDAL_OGR_L_GetFIDColumn(SEXP lyr)
{
  OGRLayerH h = unwrapHandle<OGRLayer>(lyr);
  return OGR_L_GetFIDColumn(h); 
}

// [[Rcpp::export]]
const char* RGDAL_OGR_L_GetGeometryColumn(SEXP lyr)
{
  OGRLayerH h = unwrapHandle<OGRLayer>(lyr);
  return OGR_L_GetGeometryColumn(h);
}

// Need to convert to Rcpp
// [[Rcpp::export]]
SEXP RGDAL_GetFields(SEXP feat)
{
    OGRFeatureH hF = unwrapHandle<OGRFeature>(feat);
    size_t len = OGR_F_GetFieldCount(hF);
    SEXP res = PROTECT(Rf_allocVector(VECSXP, len));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, len));
    for ( size_t i = 0; i != len; ++i )
    {
        OGRFieldDefnH fd = OGR_F_GetFieldDefnRef(hF, i);
        SET_STRING_ELT(names, i, Rf_mkChar(OGR_Fld_GetNameRef(fd)));
        switch ( OGR_Fld_GetType(fd) )
        {
            case OFTInteger:
                SET_VECTOR_ELT(res, i, Rf_ScalarInteger(OGR_F_GetFieldAsInteger(hF, i)));
            break;
            case OFTReal:
                SET_VECTOR_ELT(res, i, Rf_ScalarReal(OGR_F_GetFieldAsDouble(hF, i)));                
            break;
            default:
                SET_VECTOR_ELT(res, i, Rf_mkString(OGR_F_GetFieldAsString(hF, i)));                                
            break;
        }
    }
    Rf_setAttrib(res, R_NamesSymbol, names);
    UNPROTECT(2);
    return res;
}

// [[Rcpp::export]]
void RGDAL_PrintFeature(SEXP feat, const char* fname)
{
    OGRFeatureH hF = unwrapHandle<OGRFeature>(feat);
    FILE* f = std::fopen(fname, "w");
    OGR_F_DumpReadable(hF, f);
    std::fclose(f);
}

// [[Rcpp::export]]
SEXP RGDAL_GetGeometries(SEXP lyr)
{
    OGRLayerH hL = unwrapHandle<OGRLayer>(lyr);
    OGR_L_ResetReading(hL);
    size_t n = OGR_L_GetFeatureCount(hL, 1);
    SEXP res = PROTECT(Rf_allocVector(VECSXP, n));
    for ( size_t i = 0; i != n; ++i )
    {
        OGRFeatureH hF = OGR_L_GetNextFeature(hL);
        OGRGeometryH hG = OGR_F_GetGeometryRef(hF);
        SEXP geomPtr = PROTECT(R_MakeExternalPtr((void*) hG, R_NilValue, R_NilValue));
        SET_VECTOR_ELT(res, i, geomPtr);                                
    }
    OGR_L_ResetReading(hL);
    UNPROTECT(n + 1);
    return res;
}

// [[Rcpp::export]]
SEXP RGDAL_OGR_G_CreateGeometry(const char* gtype)
{
  OGRwkbGeometryType t = typeFromName(gtype);
  OGRGeometryH res = OGR_G_CreateGeometry(t);
  return wrapHandle<OGRGeometry>(res);
}

// [[Rcpp::export]]
int RGDAL_OGR_DS_TestCapability(SEXP ds, const char* which)
{
  OGRDataSourceH h = unwrapHandle<OGRDataSource>(ds);
  return OGR_DS_TestCapability(h, which);
}

// [[Rcpp::export]]
int RGDAL_OGR_L_TestCapability(SEXP lyr, const char* which)
{
  OGRDataSourceH h = unwrapHandle<OGRLayer>(lyr);
  return OGR_L_TestCapability(h, which);
}

// [[Rcpp::export]]
SEXP RGDAL_MakeExtent(double xmin, double xmax, double ymin, double ymax)
{
    OGRGeometryH hGeom = OGR_G_CreateGeometry(wkbPolygon),
                 hRing = OGR_G_CreateGeometry(wkbLinearRing);
    
    OGR_G_AddPoint_2D(hRing, xmin, ymin);
    OGR_G_AddPoint_2D(hRing, xmin, ymax);
    OGR_G_AddPoint_2D(hRing, xmax, ymax);
    OGR_G_AddPoint_2D(hRing, xmax, ymin);
    OGR_G_AddPoint_2D(hRing, xmin, ymin);
    _(OGR_G_AddGeometry(hGeom, hRing));
    return wrapHandle<OGRGeometry>(hGeom);
}

// [[Rcpp::export]]
SEXP RGDAL_OGR_G_GetSpatialReference(SEXP geom)
{
  OGRGeometryH h = unwrapHandle<OGRGeometry>(geom);
  OGRSpatialReferenceH srs = OGR_G_GetSpatialReference(h);
  return srs ?
    wrapHandle<OGRSpatialReference>(srs) :
    R_NilValue;
}

// [[Rcpp::export]]
SEXP RGDAL_OSRClone(SEXP srs)
{
  OGRSpatialReferenceH h = unwrapHandle<OGRSpatialReference>(srs);
  OGRSpatialReferenceH res = OSRClone(h);
  return res ?
    wrapHandle<OGRSpatialReference>(res) :
    R_NilValue;
}

// [[Rcpp::export]]
const char* RGDAL_GDALGetProjectionRef(SEXP ds)
{
  GDALDatasetH h = unwrapHandle<GDALDataset>(ds);
  return GDALGetProjectionRef(h);
}

// [[Rcpp::export]]
SEXP RGDAL_ApplyGeoTransform(SEXP ds, SEXP point_list, int inverse)
{
    GDALDatasetH hDS = unwrapHandle<GDALDataset>(ds);
    SEXP xi = PROTECT(VECTOR_ELT(point_list, 0)),
         yi = PROTECT(VECTOR_ELT(point_list, 1));
    SEXP res = PROTECT(Rf_allocVector(VECSXP, 2)),
         x = PROTECT(Rf_allocVector(REALSXP, Rf_length(xi))),
         y = PROTECT(Rf_allocVector(REALSXP, Rf_length(yi)));
    double gt[6], igt[6];
    double* gtrans;
    GDALGetGeoTransform(hDS, &(gt[0]));
    if ( inverse )
    {
        GDALInvGeoTransform(&(gt[0]), &(igt[0]));
        gtrans = &(igt[0]);
    }
    else
    {
        gtrans = &(gt[0]);
    }
    for ( size_t i = 0; i != Rf_length(x); ++i )
        GDALApplyGeoTransform(gtrans, REAL(xi)[i], REAL(yi)[i], &(REAL(x)[i]), &(REAL(y)[i]));
    SET_VECTOR_ELT(res, 0, x);
    SET_VECTOR_ELT(res, 1, y);
    attachNames(res);
    UNPROTECT(5);
    return res;
}

// [[Rcpp::export]]
SEXP RGDAL_OGR_G_Clone(SEXP geom)
{
  OGRGeometryH hGeom = unwrapHandle<OGRGeometry>(geom);
  OGRGeometryH res = OGR_G_Clone(hGeom);
  return res ?
    wrapHandle<OGRGeometry>(res) :
    R_NilValue;
}

// [[Rcpp::export]]
int RGDAL_OGR_G_TransformTo(SEXP geom, SEXP srs)
{
  OGRGeometryH g = unwrapHandle<OGRGeometry>(geom);
  OGRSpatialReferenceH s = unwrapHandle<OGRSpatialReference>(srs);
  return OGR_G_TransformTo(g, s);
}

// [[Rcpp::export]]
SEXP RGDAL_CopySubset(SEXP rb, int xos, int yos, int xsz, int ysz)
{
    GDALRasterBandH hRB = unwrapHandle<GDALRasterBand>(rb);
    GDALDataType dt = GDALGetRasterDataType(hRB);
    GDALDriverH hD = GDALGetDriverByName("MEM");
    GDALDatasetH res = GDALCreate(hD, "", xsz, ysz, 1, dt, NULL);
    GDALRasterBandH band = GDALGetRasterBand(res, 1);
    void* buf = R_alloc(xsz * ysz, GDALGetDataTypeSize(dt) / 8);
    _(GDALRasterIO(hRB, GF_Read, xos, yos, xsz, ysz, buf, xsz, ysz, dt, 0, 0));
    _(GDALRasterIO(band, GF_Write, 0, 0, xsz, ysz, buf, xsz, ysz, dt, 0, 0));
    int hasNoDataValue;
    double noDataValue = GDALGetRasterNoDataValue(hRB, &hasNoDataValue);
    if ( hasNoDataValue ) GDALSetRasterNoDataValue(band, noDataValue);
    double gt[6];
    GDALDatasetH hDS = GDALGetBandDataset(hRB);
    GDALGetGeoTransform(hDS, &(gt[0]));
    double xos_gt, yos_gt;
    GDALApplyGeoTransform(&(gt[0]), xos, yos, &xos_gt, &yos_gt);
    gt[0] = xos_gt; gt[3] = yos_gt;
    GDALSetGeoTransform(res, &(gt[0]));
    GDALSetProjection(res, GDALGetProjectionRef(hDS));
    return wrapHandle<GDALDataset>(res);
}

// [[Rcpp::export]]
void RGDALWriteRasterBand(SEXP band, SEXP data,
                          int nDSXOff, int nDSYOff, int nDSXSize, int nDSYSize)
{
    GDALRasterBandH hRB = unwrapHandle<GDALRasterBand>(band);
    if ( GDALGetRasterAccess(hRB) == GA_ReadOnly )
        stop("Raster band is read-only\n");
    void* buf;
    size_t nr = Rf_nrows(data), nc = Rf_ncols(data), nelem = nr * nc;
    double scale = 1 / GDALGetRasterScale(hRB, NULL),
           offset = GDALGetRasterOffset(hRB, NULL);
    if ( Rf_isInteger(data) )
    {
        buf = R_alloc(nelem, sizeof(int));
        for ( size_t r = 0; r != nr; ++r )
            for ( size_t c = 0; c != nc; ++c )
                ((int*)buf)[r * nc + c] = scale * INTEGER(data)[c * nr + r] - offset;
        GDALRasterIO(hRB, GF_Write,
                     nDSXOff, nDSYOff,
                     nDSXSize, nDSYSize,
                     buf, nc, nr, GDT_Int32,
                     0, 0);
        return;
    }
    if ( Rf_isReal(data) )
    {
        buf = R_alloc(nelem, sizeof(double));
        for ( size_t r = 0; r != nr; ++r )
            for ( size_t c = 0; c != nc; ++c )
                ((double*)buf)[r * nc + c] = scale * REAL(data)[c * nr + r] - offset;
        GDALRasterIO(hRB, GF_Write,
                     nDSXOff, nDSYOff,
                     nDSXSize, nDSYSize,
                     buf, nc, nr, GDT_Float64,
                     0, 0);
        return;
    }
    stop("Unsupported data type\n");
}

// [[Rcpp::export]]
void RGDAL_OGR_G_AddPoint_2D(SEXP geom, double x, double y)
{
  OGRGeometryH h = unwrapHandle<OGRGeometry>(geom);
  OGR_G_AddPoint_2D(h, x, y);
}

// [[Rcpp::export]]
void RGDAL_OGR_G_Segmentize(SEXP geom, double l)
{
  OGRGeometryH h = unwrapHandle<OGRGeometry>(geom);
  OGR_G_Segmentize(h, l);
}

// [[Rcpp::export]]
int RGDAL_OGR_G_AddGeometry(SEXP geom1, SEXP geom2)
{
  OGRGeometryH h1 = unwrapHandle<OGRGeometry>(geom1),
               h2 = unwrapHandle<OGRGeometry>(geom2);
  return OGR_G_AddGeometry(h1, h2);
}
