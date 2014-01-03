#include "pkg_config.h"
#include <gdal.h>
#include <gdal_alg.h>
#include <ogr_api.h>
#include <ogr_srs_api.h>
#include <Rcpp.h>

using namespace Rcpp;

//
// I am trying to stick to the GDAL public interface,
// so these need to be defined solely to have a type
// for the XPtr template. I suppose I could also have
// typedef'd them as void.
//
class GDALDataset;
class GDALRasterBand;

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
            ::Rf_warning("GDAL Message %d: %s\n", err_no, msg);
            break;
        case 3:
            ::Rf_warning("GDAL Error %d: %s\n", err_no, msg);
            break;
        case 4:
            ::Rf_warning("GDAL Error %d: %s\n", err_no, msg);
            stop("Unrecoverable error\n");
            break;        
        default:
            ::Rf_warning("Received invalid error class %d (errno %d: %s)\n", eErrClass, err_no, msg);
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
  ::Rf_warning("Checked for null pointer\n");
  return EXTPTR_PTR(x) == NULL;
}

// [[Rcpp::export]]
const char* GDALGetDescription(SEXP x)
{
  GDALDatasetH h = unwrapHandle<GDALDataset>(x);
  return GDALGetDescription(h);  
}

// [[Rcpp::export]]
SEXP GDALGetRasterBand(SEXP x, int n)
{
  GDALDatasetH h = unwrapHandle<GDALDataset>(x);
  GDALRasterBandH b = GDALGetRasterBand(h, n);
  return wrapHandle<GDALRasterBand>(b);
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
    GDALGetGeoTransform(h, &res[0]);
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
  GDALRasterIO(h, GF_Read, x, y, xszin, yszin, &buf[0],
               xszout, yszout, GDT_Float64, pixsp, lnsp);
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

/*
int RGDAL_WriteBlock(SEXP band, int i, int j, SEXP blk)
{
    GDALRasterBandH hB = unwrapHandle<GDALRasterBand>(band);
    int xsz, ysz;
    GDALGetBlockSize(hB, &xsz, &ysz);
    if ( length(blk) < xsz * ysz )
        error("Input does not match block size\n");
    GDALDataType dt = GDALGetRasterDataType(hB);
    switch ( dt )
    {
        case GDT_Int32:;
            return GDALWriteBlock(hB, j - 1, i - 1, INTEGER(blk));
        case GDT_Float64:;
            return GDALWriteBlock(hB, j - 1, i - 1, REAL(blk));
        case GDT_Byte:;
            return GDALWriteBlock(hB, j - 1, i - 1, RAW(blk));
        case GDT_Int16:;
            int16_t* buf = (int16_t*) R_alloc(xsz * ysz, 2);
            for ( size_t k = 0; k != xsz * ysz; ++k )
                buf[k] = INTEGER(blk)[k];
            return GDALWriteBlock(hB, j - 1, i - 1, &(buf[0]));
        default:;
            error("Unsupported data type in block read\n");
    }
}

*/
