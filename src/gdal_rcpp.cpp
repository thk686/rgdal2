#include "pkg_config.h"
#include <gdal.h>
#include <gdal_alg.h>
#include <ogr_api.h>
#include <ogr_srs_api.h>
#include <cpl_conv.h>

#include <Rcpp.h>

using namespace Rcpp;

//
// Stop on error (error handler will print message)
//
#define _(a) if ((a) != 0) stop(#a)

// Class for autowrapping
#include "../inst/include/rgdal2.h"

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

// [[Rcpp::export]]
void GDALInit()
{
    CPLSetErrorHandler((CPLErrorHandler)__err_handler);
    GDALAllRegister();
    OGRRegisterAll();
}

// [[Rcpp::export]]
DatasetH RGDAL_Open(const char* file,
                    bool readonly = true,
                    bool shared = true)
{
  GDALAccess access = readonly ? GA_ReadOnly : GA_Update;
  return shared ? GDALOpenShared(file, access) : GDALOpen(file, access);
}

// [[Rcpp::export]]
void RGDAL_Close(DatasetH h)
{
  GDALClose(*h);
}

// [[Rcpp::export]]
bool isNullPtr(SEXP x)
{
  Rf_warning("Checked for null pointer\n"); // trying to deprecate this
  return EXTPTR_PTR(x) == NULL;
}

// [[Rcpp::export]]
const char* RGDAL_GetDescription(DatasetH h)
{
  return GDALGetDescription(*h);  
}

// [[Rcpp::export]]
BandH RGDAL_GetRasterBand(DatasetH h, int n)
{
  return GDALGetRasterBand(*h, n);
}

// [[Rcpp::export]]
int RGDAL_GetRasterXSize(DatasetH h)
{
  return GDALGetRasterXSize(*h);
}

// [[Rcpp::export]]
int RGDAL_GetRasterYSize(DatasetH h)
{
  return GDALGetRasterYSize(*h);
}

// [[Rcpp::export]]
int RGDAL_GetRasterCount(DatasetH h)
{
  return GDALGetRasterCount(*h);
}

// [[Rcpp::export]]
int RGDAL_GetRasterBandXSize(BandH h)
{
  return GDALGetRasterBandXSize(*h);
}

// [[Rcpp::export]]
int RGDAL_GetRasterBandYSize(BandH h)
{
  return GDALGetRasterBandYSize(*h);
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
DatasetH RGDAL_CreateDataset(const char* driver, const char* fname,
                             int nrow, int ncol, int nbands, const char* type)
{
  GDALDataType dtype = GDALGetDataTypeByName(type);
  if ( !dtype ) stop("Unknown data type specified");
  GDALDriverH hDR = GDALGetDriverByName(driver);
  if ( !hDR ) stop("Invalid GDAL driver\n");
  GDALDatasetH ds = GDALCreate(hDR, fname, ncol, nrow, nbands, dtype, NULL);
  if ( ds ) GDALFlushCache(ds);
  return DatasetH(ds);
}

// [[Rcpp::export]]
DatasetH RGDAL_CreateCopy(DatasetH h, const char* fname, const char* dname)
{
    GDALDriverH hD = GDALGetDriverByName(dname);
    GDALDatasetH res = GDALCreateCopy(hD, fname, *h, 0, NULL, NULL, NULL);
    return DatasetH(res);
}

// [[Rcpp::export]]
SEXP RGDAL_GetGeoTransform(DatasetH h)
{
    NumericVector res(6);
    _(GDALGetGeoTransform(*h, &res[0]));
    return res;
}

// [[Rcpp::export]]
int RGDAL_SetGeoTransform(DatasetH h, SEXP y)
{
    NumericVector gt(y);
    if ( gt.size() != 6 ) return 1;
    return GDALSetGeoTransform(*h, &gt[0]);
}

// [[Rcpp::export]]
BandH RGDAL_GetMaskBand(BandH h)
{
  return GDALGetMaskBand(*h);
}

// [[Rcpp::export]]
int RGDAL_GetMaskFlags(BandH h)
{
  return GDALGetMaskFlags(*h);
}

// [[Rcpp::export]]
IntegerVector RGDAL_GetBlockSize(DatasetH h)
{
  IntegerVector res(2);
  GDALGetBlockSize(*h, &res[1], &res[0]);
  return res;
}

// [[Rcpp::export]]
SEXP RGDAL_ReadRasterData(BandH h,
                          int x, int y,
                          int xszin, int yszin,
                          int xszout, int yszout,
                          int pixsp = 0, int lnsp = 0)
{
  NumericVector buf(xszout * yszout);
  _(GDALRasterIO(*h, GF_Read, x, y, xszin, yszin, &buf[0],
                 xszout, yszout, GDT_Float64, pixsp, lnsp));
  double scale = GDALGetRasterScale(*h, NULL), 
         offset = GDALGetRasterOffset(*h, NULL),
         nodata = GDALGetRasterNoDataValue(*h, NULL);
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
int RGDAL_writeRasterData(BandH h,
                          NumericMatrix raster,
                          int x, int y,
                          int xszout, int yszout,
                          int pixsp = 0, int lnsp = 0)
{
  int xszin = raster.cols(), yszin = raster.rows();
  double scale = GDALGetRasterScale(*h, NULL), 
         offset = GDALGetRasterOffset(*h, NULL),
         nodata = GDALGetRasterNoDataValue(*h, NULL);
  NumericVector buf(xszin * yszin);
  for ( int xx = 0; xx != xszin; ++xx )
    for ( int yy = 0; yy != yszin; ++yy )
    {
      double pixv = raster[xx * yszin + yy];
      buf[yy * xszin + xx] =
        pixv == NA_REAL ? nodata : (pixv - offset) / scale;
    }
  return GDALRasterIO(*h, GF_Write, x, y, xszout, yszout, &buf[0],
                      xszin, yszin, GDT_Float64, pixsp, lnsp);
}

// [[Rcpp::export]]
SEXP RGDAL_ReadBlock(BandH h, int i, int j)
{
    int xsz, ysz;
    GDALGetBlockSize(*h, &xsz, &ysz);
    double scale = GDALGetRasterScale(*h, NULL), 
           offset = GDALGetRasterOffset(*h, NULL),
           nodata = GDALGetRasterNoDataValue(*h, NULL);
    GDALDataType dt = GDALGetRasterDataType(*h);
    switch ( dt )
    {
        case GDT_Int32:
          {
            IntegerVector buf(xsz * ysz, NA_INTEGER);
            GDALReadBlock(*h, j - 1, i - 1, &buf[0]);
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
            GDALReadBlock(*h, j - 1, i - 1, &buf[0]);
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
            GDALReadBlock(*h, j - 1, i - 1, &buf[0]);
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
            GDALReadBlock(*h, j - 1, i - 1, &buf[0]);
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
int RGDAL_WriteBlock(BandH h, int i, int j, SEXP blk)
{
    int xsz, ysz;
    GDALGetBlockSize(*h, &xsz, &ysz);
    if ( Rf_length(blk) < xsz * ysz )
        stop("Input does not match block size\n");
    GDALDataType dt = GDALGetRasterDataType(*h);
    switch ( dt )
    {
        case GDT_Int32:
            return GDALWriteBlock(*h, j - 1, i - 1, INTEGER(blk));
        case GDT_Float64:
            return GDALWriteBlock(*h, j - 1, i - 1, REAL(blk));
        case GDT_Byte:
            return GDALWriteBlock(*h, j - 1, i - 1, RAW(blk));
        case GDT_Int16:
          {
            int16_t* buf = (int16_t*) R_alloc(xsz * ysz, 2);
            for ( size_t k = 0; k != xsz * ysz; ++k )
                buf[k] = INTEGER(blk)[k];
            return GDALWriteBlock(*h, j - 1, i - 1, &(buf[0]));
          }
        default:
            stop("Unsupported data type in block read\n");
    }
}

// [[Rcpp::export]]
DatasourceH RGDAL_OGROpen(const char* file, int readonly)
{
  return OGROpen(file, !readonly, NULL);
}

// [[Rcpp::export]]
void RGDAL_ReleaseDataSource(DatasourceH h)
{
  OGRReleaseDataSource(*h);
}

// [[Rcpp::export]]
int RGDAL_DS_GetLayerCount(DatasourceH h)
{
  return OGR_DS_GetLayerCount(*h);
}

// [[Rcpp::export]]
const char* RGDAL_DS_GetName(DatasourceH h)
{
  return OGR_DS_GetName(*h);
}

// [[Rcpp::export]]
const char* RGDAL_DS_GetDriver(DatasourceH h)
{
    OGRSFDriverH hDr = OGR_DS_GetDriver(*h);
    return OGR_Dr_GetName(hDr);
}

// [[Rcpp::export]]
LayerH RGDAL_DS_GetLayer(DatasourceH h, int i)
{
  return OGR_DS_GetLayer(*h, i);
}

// [[Rcpp::export]]
GeometryH RGDAL_GetRasterExtent(DatasetH h)
{
    double transf[6],
           geox, geoy,
           xmin = 0, ymin = 0,
           xmax = GDALGetRasterXSize(*h),
           ymax = GDALGetRasterYSize(*h);

    OGRGeometryH hGeom = OGR_G_CreateGeometry(wkbPolygon),
                 hRing = OGR_G_CreateGeometry(wkbLinearRing);
    
    if ( GDALGetGeoTransform(*h, &(transf[0])) )
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
    return hGeom;
}

// [[Rcpp::export]]
const char* RGDAL_L_GetName(LayerH h)
{
  return OGR_L_GetName(*h);
}

// [[Rcpp::export]]
LayerH RGDAL_DS_GetLayerByName(DatasourceH h, const char* lnam)
{
  return OGR_DS_GetLayerByName(*h, lnam);
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
GeometryH RGDAL_GetLayerEnv(LayerH h)
{
    OGREnvelope env;
    OGR_L_GetExtent(*h, &env, 1);
    return extentToGeom(env);
}

// [[Rcpp::export]]
const char* RGDAL_GetProjectionRef(DatasourceH h)
{
  GDALGetProjectionRef(*h);
}

// [[Rcpp::export]]
SpRefSysH RGDAL_OSRNewSpatialReference(const char* srss)
{
  return OSRNewSpatialReference(srss);
}

// [[Rcpp::export]]
int RGDAL_OSRSetFromUserInput(SpRefSysH h, const char* def)
{
  return OSRSetFromUserInput(*h, def);
}

// [[Rcpp::export]]
void RGDAL_OSRRelease(SpRefSysH h)
{
  OSRRelease(*h);
}

// [[Rcpp::export]]
SEXP RGDAL_GetWKT(SpRefSysH h)
{
    char* wktstring;
    _(OSRExportToPrettyWkt(*h, &wktstring, 0));
    SEXP res = Rf_ScalarString(Rf_mkChar(wktstring));
    CPLFree(wktstring);
    return res;
}

// [[Rcpp::export]]
SEXP RGDAL_GetPROJ4(SpRefSysH h)
{
    if ( ! *h )
        return(Rf_ScalarString(Rf_mkChar("Null SRS")));
    char* proj4string;
    _(OSRExportToProj4(*h, &proj4string));
    SEXP res = Rf_ScalarString(Rf_mkChar(proj4string));
    CPLFree(proj4string);
    return res;
}

// [[Rcpp::export]]
int RGDAL_SetProjection(DatasetH h, const char* proj)
{
  return GDALSetProjection(*h, proj);
}

// [[Rcpp::export]]
int RGDAL_L_GetFeatureCount(LayerH h)
{
  return OGR_L_GetFeatureCount(*h, 1);
}

// [[Rcpp::export]]
GeometryH RGDAL_L_GetSpatialFilter(LayerH h)
{
  return OGR_L_GetSpatialFilter(*h);
}

// [[Rcpp::export]]
void RGDAL_L_ResetReading(LayerH h)
{
  OGR_L_ResetReading(*h);
}

// [[Rcpp::export]]
SpRefSysH RGDAL_L_GetSpatialRef(LayerH h)
{
  return OGR_L_GetSpatialRef(h);
}

// [[Rcpp::export]]
void RGDAL_G_DestroyGeometry(GeometryH h)
{
  OGR_G_DestroyGeometry(*h);
}

// [[Rcpp::export]]
void RGDAL_G_AssignSpatialReference(GeometryH h1, SpRefSysH h2)
{
  OGR_G_AssignSpatialReference(*h1, *h2);
}

// [[Rcpp::export]]
int RGDAL_G_GetCoordinateDimension(GeometryH h)
{
  return OGR_G_GetCoordinateDimension(*h);
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
SEXP RGDAL_GetPoints(GeometryH h)
{
  return GetPointsInternal(*h);
}

// [[Rcpp::export]]
void RGDAL_DumpGeometry(GeometryH h, const char* fname)
{
    FILE* f = std::fopen(fname, "w");
    OGR_G_DumpReadable(*h, f, "");
    std::fclose(f);
}

// [[Rcpp::export]]
LayerH RGDAL_ExecSQL(DatasourceH h, const char* sql)
{
  return OGR_DS_ExecuteSQL(*h, sql, NULL, NULL);
}

// [[Rcpp::export]]
void RGDAL_DS_ReleaseResultSet(DatasourceH h1, LayerH h2)
{
  OGR_DS_ReleaseResultSet(*h1, *h2);
}

// [[Rcpp::export]]
GeometryH RGDAL_GetGeomEnv(GeometryH h)
{
    OGREnvelope env;
    OGR_G_GetEnvelope(*h, &env);
    return extentToGeom(env);
}

// [[Rcpp::export]]
SEXP RGDAL_GetFIDs(LayerH h)
{
    OGR_L_ResetReading(*h);
    int n = OGR_L_GetFeatureCount(*h, 1);
    NumericVector res(n);
    for ( int i = 0; i != n; ++i )
    {
        OGRFeatureH hF = OGR_L_GetNextFeature(*h);
        res[i] = (double) OGR_F_GetFID(hF);
    }
    OGR_L_ResetReading(*h);
    return res;
}

// [[Rcpp::export]]
FeatureH RGDAL_GetFeature(LayerH h, double index)
{
    return OGR_F_Clone(OGR_L_GetFeature(*h, (long) index));
}

// [[Rcpp::export]]
GeometryH RGDAL_F_GetGeometryRef(FeatureH h)
{
  return OGR_G_Clone(OGR_F_GetGeometryRef(*h));
}

// [[Rcpp::export]]
std::string RGDAL_G_GetGeometryType(GeometryH h)
{
  return std::string(OGR_G_GetGeometryName(*h));  // needs to copy
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
FeatureH RGDAL_L_GetNextFeature(LayerH h)
{
  return OGR_F_Clone(OGR_L_GetNextFeature(*h));
}

// [[Rcpp::export]]
int GetLayerFieldCount(LayerH h)
{
  OGRFeatureDefnH f = OGR_L_GetLayerDefn(*h);
  return f ? OGR_FD_GetFieldCount(f) : 0;
}

// [[Rcpp::export]]
SEXP GetFieldNames(LayerH h)
{
  CharacterVector res;
  OGRFeatureDefnH f = OGR_L_GetLayerDefn(*h);
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
const char* RGDAL_L_GetFIDColumn(LayerH h)
{
  return OGR_L_GetFIDColumn(*h); 
}

// [[Rcpp::export]]
const char* RGDAL_L_GetGeometryColumn(LayerH h)
{
  return OGR_L_GetGeometryColumn(*h);
}

// Need to convert to Rcpp
// [[Rcpp::export]]
SEXP RGDAL_GetFields(FeatureH h)
{
    size_t len = OGR_F_GetFieldCount(*h);
    SEXP res = PROTECT(Rf_allocVector(VECSXP, len));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, len));
    for ( size_t i = 0; i != len; ++i )
    {
        OGRFieldDefnH fd = OGR_F_GetFieldDefnRef(*h, i);
        SET_STRING_ELT(names, i, Rf_mkChar(OGR_Fld_GetNameRef(fd)));
        switch ( OGR_Fld_GetType(fd) )
        {
            case OFTInteger:
                SET_VECTOR_ELT(res, i, Rf_ScalarInteger(OGR_F_GetFieldAsInteger(*h, i)));
            break;
            case OFTReal:
                SET_VECTOR_ELT(res, i, Rf_ScalarReal(OGR_F_GetFieldAsDouble(*h, i)));                
            break;
            default:
                SET_VECTOR_ELT(res, i, Rf_mkString(OGR_F_GetFieldAsString(*h, i)));                                
            break;
        }
    }
    Rf_setAttrib(res, R_NamesSymbol, names);
    UNPROTECT(2);
    return res;
}

// [[Rcpp::export]]
void RGDAL_PrintFeature(FeatureH h, const char* fname)
{
    FILE* f = std::fopen(fname, "w");
    OGR_F_DumpReadable(*h, f);
    std::fclose(f);
}

// [[Rcpp::export]]
SEXP RGDAL_GetGeometries(LayerH h)
{
    OGR_L_ResetReading(*h);
    size_t n = OGR_L_GetFeatureCount(*h, 1);
    SEXP res = PROTECT(Rf_allocVector(VECSXP, n));
    for ( size_t i = 0; i != n; ++i )
    {
        OGRFeatureH hF = OGR_L_GetNextFeature(*h);
        OGRGeometryH hG = OGR_G_Clone(OGR_F_GetGeometryRef(hF));
        SEXP geomPtr = PROTECT(R_MakeExternalPtr((void*) hG, R_NilValue, R_NilValue));
        SET_VECTOR_ELT(res, i, geomPtr);                                
    }
    OGR_L_ResetReading(*h);
    UNPROTECT(n + 1);
    return res;
}

// [[Rcpp::export]]
GeometryH RGDAL_G_CreateGeometry(const char* gtype)
{
  OGRwkbGeometryType t = typeFromName(gtype);
  OGRGeometryH res = OGR_G_CreateGeometry(t);
  return res;
}

// [[Rcpp::export]]
int RGDAL_DS_TestCapability(DatasetH h, const char* which)
{
  return OGR_DS_TestCapability(*h, which);
}

// [[Rcpp::export]]
int RGDAL_L_TestCapability(LayerH h, const char* which)
{
  return OGR_L_TestCapability(*h, which);
}

// [[Rcpp::export]]
GeometryH RGDAL_MakeExtent(double xmin, double xmax, double ymin, double ymax)
{
    OGRGeometryH hGeom = OGR_G_CreateGeometry(wkbPolygon),
                 hRing = OGR_G_CreateGeometry(wkbLinearRing);
    
    OGR_G_AddPoint_2D(hRing, xmin, ymin);
    OGR_G_AddPoint_2D(hRing, xmin, ymax);
    OGR_G_AddPoint_2D(hRing, xmax, ymax);
    OGR_G_AddPoint_2D(hRing, xmax, ymin);
    OGR_G_AddPoint_2D(hRing, xmin, ymin);
    _(OGR_G_AddGeometry(hGeom, hRing));
    return hGeom;
}

// [[Rcpp::export]]
SpRefSysH RGDAL_G_GetSpatialReference(GeometryH h)
{
  return OGR_G_GetSpatialReference(*h);
}

// [[Rcpp::export]]
SpRefSysH RGDAL_OSRClone(SpRefSysH h)
{
  return OSRClone(*h);
}

// [[Rcpp::export]]
SEXP RGDAL_ApplyGeoTransform(DatasetH ds, SEXP point_list, int inverse)
{
    SEXP xi = PROTECT(VECTOR_ELT(point_list, 0)),
         yi = PROTECT(VECTOR_ELT(point_list, 1));
    SEXP res = PROTECT(Rf_allocVector(VECSXP, 2)),
         x = PROTECT(Rf_allocVector(REALSXP, Rf_length(xi))),
         y = PROTECT(Rf_allocVector(REALSXP, Rf_length(yi)));
    double gt[6], igt[6];
    double* gtrans;
    GDALGetGeoTransform(*ds, &(gt[0]));
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
GeometryH RGDAL_G_Clone(GeometryH g)
{
  return OGR_G_Clone(*g);
}

// [[Rcpp::export]]
int RGDAL_G_TransformTo(GeometryH g, SpRefSysH s)
{
  return OGR_G_TransformTo(*g, *s);
}

// [[Rcpp::export]]
DatasetH RGDAL_CopySubset(BandH hRB, int xos, int yos, int xsz, int ysz)
{
    GDALDataType dt = GDALGetRasterDataType(*hRB);
    GDALDriverH hD = GDALGetDriverByName("MEM");
    GDALDatasetH res = GDALCreate(hD, "", xsz, ysz, 1, dt, NULL);
    GDALRasterBandH band = GDALGetRasterBand(res, 1);
    void* buf = R_alloc(xsz * ysz, GDALGetDataTypeSize(dt) / 8);
    _(GDALRasterIO(*hRB, GF_Read, xos, yos, xsz, ysz, buf, xsz, ysz, dt, 0, 0));
    _(GDALRasterIO(band, GF_Write, 0, 0, xsz, ysz, buf, xsz, ysz, dt, 0, 0));
    int hasNoDataValue;
    double noDataValue = GDALGetRasterNoDataValue(*hRB, &hasNoDataValue);
    if ( hasNoDataValue ) GDALSetRasterNoDataValue(band, noDataValue);
    double gt[6];
    GDALDatasetH hDS = GDALGetBandDataset(*hRB);
    GDALGetGeoTransform(hDS, &(gt[0]));
    double xos_gt, yos_gt;
    GDALApplyGeoTransform(&(gt[0]), xos, yos, &xos_gt, &yos_gt);
    gt[0] = xos_gt; gt[3] = yos_gt;
    GDALSetGeoTransform(res, &(gt[0]));
    GDALSetProjection(res, GDALGetProjectionRef(hDS));
    return res;
}

// [[Rcpp::export]]
void RGDALWriteRasterBand(BandH hRB, SEXP data,
                          int nDSXOff, int nDSYOff,
                          int nDSXSize, int nDSYSize)
{
    if ( GDALGetRasterAccess(*hRB) == GA_ReadOnly )
        stop("Raster band is read-only\n");
    void* buf;
    size_t nr = Rf_nrows(data), nc = Rf_ncols(data), nelem = nr * nc;
    double scale = 1 / GDALGetRasterScale(*hRB, NULL),
           offset = GDALGetRasterOffset(*hRB, NULL);
    if ( Rf_isInteger(data) )
    {
        buf = R_alloc(nelem, sizeof(int));
        for ( size_t r = 0; r != nr; ++r )
            for ( size_t c = 0; c != nc; ++c )
                ((int*)buf)[r * nc + c] = scale * INTEGER(data)[c * nr + r] - offset;
        GDALRasterIO(*hRB, GF_Write,
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
        GDALRasterIO(*hRB, GF_Write,
                     nDSXOff, nDSYOff,
                     nDSXSize, nDSYSize,
                     buf, nc, nr, GDT_Float64,
                     0, 0);
        return;
    }
    stop("Unsupported data type\n");
}

// [[Rcpp::export]]
void RGDAL_G_AddPoint_2D(GeometryH h, double x, double y)
{
  OGR_G_AddPoint_2D(*h, x, y);
}

// [[Rcpp::export]]
void RGDAL_G_Segmentize(GeometryH h, double l)
{
  OGR_G_Segmentize(*h, l);
}

// [[Rcpp::export]]
int RGDAL_G_AddGeometry(GeometryH h1, GeometryH h2)
{
  return OGR_G_AddGeometry(*h1, *h2);
}

// [[Rcpp::export]]
void RGDAL_G_CloseRings(GeometryH h)
{
  OGR_G_CloseRings(*h);
}

extern "C"
GDALDatasetH
RGDAL_RasterWarp_Internal(GDALDatasetH hSrcDS,
                          const char* pszDstFilename,
                          const char* pszTargetSRS,
                          const char* pszFormat,
                          double dfErrorThreshold);
                          
// [[Rcpp::export]]
DatasetH RGDAL_RasterWarp(DatasetH h, const char* file, const char* srs,
                          const char* driver, double err)
{
  return RGDAL_RasterWarp_Internal(*h, file, srs, driver, err);
}

// [[Rcpp::export]]
const char* RGDAL_GetDSDriverName(DatasourceH h)
{
    OGRSFDriverH hDr = OGR_DS_GetDriver(*h);
    return OGR_Dr_GetName(hDr);
}

// [[Rcpp::export]]
int RGDAL_G_Intersects(GeometryH g1, GeometryH g2)
{
  return OGR_G_Intersects(*g1, *g2);
}

// [[Rcpp::export]]
int RGDAL_G_Equals(GeometryH g1, GeometryH g2)
{
  return OGR_G_Equals(*g1, *g2);
}

// [[Rcpp::export]]
int RGDAL_G_Disjoint(GeometryH g1, GeometryH g2)
{
  return OGR_G_Disjoint(*g1, *g2);
}

// [[Rcpp::export]]
int RGDAL_G_Touches(GeometryH g1, GeometryH g2)
{
  return OGR_G_Touches(*g1, *g2);
}

// [[Rcpp::export]]
int RGDAL_G_Crosses(GeometryH g1, GeometryH g2)
{
  return OGR_G_Crosses(*g1, *g2);
}

// [[Rcpp::export]]
int RGDAL_G_Within(GeometryH g1, GeometryH g2)
{
  return OGR_G_Within(*g1, *g2);
}

// [[Rcpp::export]]
int RGDAL_G_Contains(GeometryH g1, GeometryH g2)
{
  return OGR_G_Contains(*g1, *g2);
}

// [[Rcpp::export]]
int RGDAL_G_Overlaps(GeometryH g1, GeometryH g2)
{
  return OGR_G_Overlaps(*g1, *g2);
}

// [[Rcpp::export]]
GeometryH RGDAL_G_Union(GeometryH g1, GeometryH g2)
{
  return OGR_G_Union(*g1, *g2);
}

// [[Rcpp::export]]
GeometryH RGDAL_G_Difference(GeometryH g1, GeometryH g2)
{
  return OGR_G_Difference(*g1, *g2);
}

// [[Rcpp::export]]
GeometryH RGDAL_G_SymDifference(GeometryH g1, GeometryH g2)
{
  return OGR_G_SymDifference(*g1, *g2);
}

// [[Rcpp::export]]
double RGDAL_G_Distance(GeometryH g1, GeometryH g2)
{
  return OGR_G_Distance(*g1, *g2);
}

// [[Rcpp::export]]
int RGDAL_G_Centroid(GeometryH g1, GeometryH g2)
{
  return OGR_G_Centroid(*g1, *g2);
}

// [[Rcpp::export]]
GeometryH RGDAL_G_Boundary(GeometryH g)
{
  return OGR_G_Boundary(*g);
}

// [[Rcpp::export]]
GeometryH RGDAL_G_ConvexHull(GeometryH g)
{
  return OGR_G_ConvexHull(*g);
}

// [[Rcpp::export]]
GeometryH RGDAL_G_UnionCascaded(GeometryH g)
{
  return OGR_G_UnionCascaded(*g);
}

// [[Rcpp::export]]
double RGDAL_G_Length(GeometryH g)
{
  return OGR_G_Length(*g);
}

// [[Rcpp::export]]
double RGDAL_G_Area(GeometryH g)
{
  return OGR_G_Area(*g);
}

// [[Rcpp::export]]
GeometryH RGDAL_G_Simplify(GeometryH g, double tol)
{
  return OGR_G_Simplify(*g, tol);
}

// [[Rcpp::export]]
GeometryH RGDAL_G_SimplifyPreserveTopology(GeometryH g, double tol)
{
  return OGR_G_SimplifyPreserveTopology(*g, tol);
}

// [[Rcpp::export]]
GeometryH RGDAL_G_Polygonize(GeometryH g)
{
  return OGR_G_Polygonize(*g);
}

// [[Rcpp::export]]
void RGDAL_F_Destroy(FeatureH h)
{
  OGR_F_Destroy(*h);
}