context("First-date of invasion")

test_that("sr_fdoi() correctly interpolates data in geographical coordinates", {

  sro <- sr_obs(obs_geo, "date")

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
  # points(obs_geo)

  ## The fdoi values must be approximately the distance of the cells
  ## to the origin times 10
  cell_dists <- sqrt(rowSums(xyFromCell(fdoi, seq.int(ncell(fdoi)))**2))

  # plot(getValues(fdoi), 10 * cell_dists)
  # abline(0, 1)

  expect_equal(getValues(fdoi), 10 * cell_dists,
               tolerance = maxValue(fdoi) / 10)


})


test_that("sr_fdoi() correctly interpolates data in planar coordinates", {

  sro <- sr_obs(obs_prj, "date")

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

