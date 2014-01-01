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
std::vector<int> GDALGetBlockSize(SEXP x)
{
  GDALDatasetH h = unwrapHandle<GDALDataset>(x);
  std::vector<int> res(2);
  GDALGetBlockSize(h, &res[1], &res[0]);
  return res;
}







