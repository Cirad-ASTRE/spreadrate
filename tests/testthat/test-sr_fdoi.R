context("First-date of invasion")

test_that("sr_fdoi() interpolates data in geographical coordinates", {

  set.seed(20190619)
  ## sr_obs from random points in the unit square (of decimal degrees)
  d <- data.frame(lon = runif(30), lat = runif(30))
  d <- transform(d, date = round(10 * sqrt(lon**2 + lat**2)))
  sro <- sr_obs(d, "date")

  ## A estimation mask covering the unit square
  r <- raster(
    extent(c(0, 1, 0, 1)),
    nrows = 11,
    ncols = 11,
    crs = st_crs(sro)$proj4string,
    vals = 1
  )

  fdoi <- expect_error(sr_fdoi(sro, r), NA)

  # plot(fdoi)
  # points(d)

  ## The fdoi values must be approximately the distance of the cells
  ## to the origin times 10
  cell_dists <- sqrt(rowSums(xyFromCell(fdoi, seq.int(ncell(fdoi)))**2))

  # plot(getValues(fdoi), 10 * cell_dists)
  # abline(0, 1)

  expect_equal(getValues(fdoi), 10 * cell_dists,
               tolerance = maxValue(fdoi) / 10)


})


test_that("sr_fdoi() interpolates data in planar coordinates", {

  set.seed(20190619)
  ## sr_obs from random points in a 10x10 square
  d <- st_cast(
    st_sfc(
      st_multipoint(
        matrix(runif(2*30), ncol = 2) * 10
      ),
      crs = 3857
    ),
    "POINT"
  )
  d_dists <- sqrt(rowSums(st_coordinates(d)**2))
  dsf <- st_sf(d, date = round(d_dists))
  sro <- sr_obs(dsf, "date")

  ## A estimation mask covering the 10x10 square
  r <- raster(
    extent(c(0, 1, 0, 1)*10),
    nrows = 21,
    ncols = 21,
    crs = st_crs(sro)$proj4string,
    vals = 1
  )

  fdoi <- expect_error(sr_fdoi(sro, r), NA)

  # plot(fdoi)
  # points(st_coordinates(sro))

  ## The fdoi values must be approximately the distance of the cells
  ## to the origin times 10
  cell_dists <- sqrt(rowSums(xyFromCell(fdoi, seq.int(ncell(fdoi)))**2))

  # plot(getValues(fdoi), cell_dists)
  # abline(0, 1)

  expect_equal(getValues(fdoi), cell_dists,
               tolerance = maxValue(fdoi) / 10)


})


test_that("sr_fdoi() estimates the surface for the MC replicates", {

  ## Use always the same relative neghbourhood threshold parameter
  nsim <- 10L
  sro <- sr_obs(obs_prj, "date", uq = sr_uq(nsim, 1, 1, -5))

  ## A estimation mask covering the 10x10 square
  r <- raster(
    extent(c(0, 1, 0, 1)*10),
    nrows = 21,
    ncols = 21,
    crs = st_crs(sro)$proj4string,
    vals = 1
  )

  fdoi <- expect_error(sr_fdoi(sro, r), NA)
  mc <- expect_is(attr(fdoi, "mc"), "RasterBrick")
  # plot(mc)

  expect_identical(nsim, nlayers(mc))

})
