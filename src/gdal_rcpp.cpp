#include "pkg_config.h"
#include <gdal.h>
#include <gdal_priv.h>
#include <gdal_alg.h>
#include <ogr_api.h>
#include <ogr_srs_api.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP GDALInit(const char* ignored = "dummy")
{
  GDALAllRegister();
  return R_NilValue;
}

// [[Rcpp::export]]
SEXP GDALOpen(const char* file,
              bool readonly = true,
              bool shared = true)
{
  GDALAccess access = readonly ? GA_ReadOnly : GA_Update;
  GDALDataset* handle = shared ?
    (GDALDataset*) GDALOpenShared(file, access) :
    (GDALDataset*) GDALOpen(file, access);
  return XPtr<GDALDataset>(handle, false);
}

// [[Rcpp::export]]
SEXP GDALClose(SEXP handleXPtr)
{
  XPtr<GDALDataset> handle(handleXPtr);
  GDALClose(&*handle);
  return handle;
}

// [[Rcpp::export]]
bool isNullPtr(SEXP x)
{
  return EXTPTR_PTR(x) == NULL;
}

// [[Rcpp::export]]
const char* GDALGetDescription(SEXP handleXPtr)
{
  XPtr<GDALDataset> handle(handleXPtr);
  return handle->GetDescription();  
}

// [[Rcpp::export]]
SEXP GDALGetRasterBand(SEXP handleXPtr, int n)
{
  XPtr<GDALDataset> handle(handleXPtr);
  GDALRasterBand* b = handle->GetRasterBand(n);
  return XPtr<GDALRasterBand>(b, false);
}

// [[Rcpp::export]]
int GDALGetRasterBandXSize(SEXP handleXPtr)
{
  XPtr<GDALRasterBand> handle(handleXPtr);
  return handle->GetXSize();
}

// [[Rcpp::export]]
int GDALGetRasterBandYSize(SEXP handleXPtr)
{
  XPtr<GDALRasterBand> handle(handleXPtr);
  return handle->GetYSize();
}

// [[Rcpp::export]]
const char* versionInfo(const char* what = "--version")
{
  return GDALVersionInfo(what);
}

// [[Rcpp::export]]
SEXP RGDAL_CleanupAll(const char* ignored = "dummy")
{
    OGRCleanupAll();
    OSRCleanup();
    return R_NilValue;
}
