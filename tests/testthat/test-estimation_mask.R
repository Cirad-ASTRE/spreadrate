context("Estimation mask")

test_that("estimation_mask() works as expected", {


  ## Corner points of the unit square
  pp <- st_multipoint(as.matrix(expand.grid(0:1, 0:1)))
  bs <- .3
  res <- .1

  em <- expect_error(
    estimation_mask(pp, buffer_size = bs, res = res),
    NA
  )
  # plot(em); plot(pp, add = T)

  expect_is(em, "RasterLayer")
  expect_equal(extent(em), extent(rep(0:1 + c(-1, 1)*bs, 2)))
  expect_equal(res(em), rep(res, 2))

  ## Single point
  p1 <- st_point(c(0, 0))
  em1 <- expect_error(
    estimation_mask(p1, buffer_size = 1, res = .1),
    NA
  )
  # plot(em1); plot(p1, add = T)

  ## sr_obs object with coordinates in longlat
  sro <- sr_obs(obs_geo, "date")
  em_sr <- expect_warning(
    estimation_mask(sro, buffer_size = bs, res = res),
    "longitude/latitude data"
  )
  # plot(em_sr); plot(sro, add = T)

})
