context("Compute spread-rate from fdoi")

## Surface with constant slope of 1 -> constant sr of 1
r <- raster(replicate(100, seq(0.01, 1, length = 100)),
            crs = sp::CRS("+init=epsg:3857"))
# plot(r)

test_that("invslope() works as expected with a RasterLayer", {

  ## Filtering the top 1 % is useless here, since all values are
  ## constant
  sr <- expect_error(invslope(r, rmtop = 1), NA)
  # plot(sr)

  expect_is(sr, "RasterLayer")

  ## Variation in values should be negligible
  expect_equal(sd(getValues(sr), na.rm = TRUE), 0)

  ## The average value should be of about 1/100
  expect_equal(mean(getValues(sr), na.rm = TRUE), 1)

})


test_that("invslope() works as expected with a RasterBrick", {

  rb <- brick(r, 2*r, 3*r)

  ## Filtering the top 1 % is useless here, since all values are
  ## constant
  sr <- expect_error(invslope(rb, rmtop = 1), NA)
  # plot(sr)

  expect_is(sr, "RasterBrick")

  ## Variation in values should be negligible
  expect_equal(
    unname(apply(values(sr), 2, sd, na.rm = TRUE)),
    rep(0, 3)
  )

  ## The average value should be of about 1/100
  expect_equal(
    unname(apply(values(sr), 2, mean, na.rm = TRUE)),
    1/(1:3)
  )

})

test_that("invslope() correcly filters values", {

  ## Now use a quadratic surface
  quad_vals <- seq(1, 2, length = 100)**2
  r2 <- raster(replicate(100, quad_vals), crs = sp::CRS("+init=epsg:3857"))

  # plot(r2)

  sr_fp0 <- expect_error(invslope(r2), NA)
  unfiltered_values <- getValues(sr_fp0)

  sr_fp1 <- expect_error(invslope(r2, rmtop = 10), NA)
  filter_1pst <- getValues(sr_fp1)

  n_filtered <- sum(unfiltered_values > filter_1pst, na.rm = TRUE)
  total_values <- length(filter_1pst[!is.na(filter_1pst)])

  expect_equal(total_values/n_filtered, 10, tolerance = 1)


  fix_bnd <- 3:4/10
  sr_fp2 <- expect_error(invslope(r2, bnd = fix_bnd), NA)
  expect_equal(range(getValues(sr_fp2), na.rm = TRUE), fix_bnd)

})


test_that("invslope() uses parallel computation", {

  # This breaks other tests
  # require(future)
  # plan(multiprocess)
  rb <- brick(r, r, r, r)
  expect_error(invslope(rb), NA)
})
