test_that("GDAL public functions",
{
  file = tempfile(); on.exit(unlink(file))
  x = newDataset(nrow = 64L, ncol = 64L, nbands = 2L,
                 dataType = "Float64", driver = "GTiff", file = file,
                 opts = c("TILED=YES", "BLOCKXSIZE=32", "BLOCKYSIZE=32"))
  expect_that(x, is_a("RGDAL2Dataset"))
  expect_that(dim(x), equals(c(64, 64, 2)))
  expect_that(c(nrow(x), ncol(x), nband(x)), equals(dim(x)))
  expect_that(getBlockSize(x), equals(c(32, 32)))
})