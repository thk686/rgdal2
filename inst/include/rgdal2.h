#ifndef __RGDAL2_H__
#define __RGDAL2_H__

#include <Rcpp.h>

using namespace Rcpp;

class RGDALHandleWrapper
{
public:
  RGDALHandleWrapper(void* h)
  : handle(h) {}  // If initialized NULL will return NULL SEXP
  RGDALHandleWrapper(const SEXP h)
  : handle(R_ExternalPtrAddr(h))
  {
    if ( !handle ) stop("Null pointer passed to function\n");
  }
  operator SEXP() const
  {
    return handle ?  // Return NULL SEXP on NULL pointer
      R_MakeExternalPtr(handle, R_NilValue, R_NilValue) :
      R_NilValue;
  }
  void* operator*() const { return handle; }
private:
  void* handle;
};

typedef RGDALHandleWrapper DatasetH;
typedef RGDALHandleWrapper BandH;
typedef RGDALHandleWrapper DatasourceH;
typedef RGDALHandleWrapper LayerH;
typedef RGDALHandleWrapper GeometryH;
typedef RGDALHandleWrapper SpRefSysH;
typedef RGDALHandleWrapper FeatureH;

#endif // __RGDAL2_H__
