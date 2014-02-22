#ifndef __RGDAL2_H__
#define __RGDAL2_H__

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

template<class T>
SEXP wrapHandle(void* x)
{
  if ( !x ) stop("Attempt to wrap null handle\n");
  return XPtr<T>((T*) x, false);
}

template<class T>
void* unwrapHandle(SEXP x)
{
  XPtr<T> y(x);
  void* z = (void*) &*y;
  if ( !z ) stop("Attempt to unwrap null handle\n");
  return z;
}

template<class T>
class RGDALHandle
{
public:
  RGDALHandle(void* h)
  : handle(h) {}
  RGDALHandle(const SEXP h)
  : handle(unwrapHandle<T>(h)) {}
  operator SEXP() const
  {
    return handle ?
      wrapHandle<T>(handle) :
      R_NilValue;
  }
  void* operator*() const { return handle; }
private:
  void* handle;
};

typedef RGDALHandle<GDALDataset> DatasetH;
typedef RGDALHandle<GDALRasterBand> BandH;
typedef RGDALHandle<OGRDataSource> DatasourceH;
typedef RGDALHandle<OGRLayer> LayerH;
typedef RGDALHandle<OGRGeometry> GeometryH;
typedef RGDALHandle<OGRSpatialReference> SpRefSysH;
typedef RGDALHandle<OGRFeature> FeatureH;

#endif // __RGDAL2_H__
