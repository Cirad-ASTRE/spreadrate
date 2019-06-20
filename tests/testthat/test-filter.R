context("Filtering cases")

set.seed(20190620)

test_that("Filtering earliest neighbours works as expected", {

  ## Use 30 % of data diameter as neighbouring tolerance.
  sro <- sr_obs(obs_geo, "date", uq = sr_uq(neigh_tol = -c(20, 30)))
  srf <- expect_error(filter_earliest_neigh(sro), NA)
  ntp <- neigh_tol(srf)

  # ggplot(sro) +
  #   geom_sf() +
  #   geom_sf(data = srf, col = "red") +
  #   ggforce::geom_circle(
  #     data = data.frame(
  #       st_coordinates(srf),
  #       r = as.numeric(dist2arc(st_sfc(st_point(c(.5, .5)), crs = 4326), ntp))
  #     ),
  #     aes(x0 = X, y0 = Y, r = r))

  ## Note that there is considerable overlap of neighbourhoods.
  ## If the earliest observation from two neighbourhood lies in their
  ## intersection, the same date will be assigned to both.
  ## A different approach, also reasonable, would be to consider the
  ## dates among the points that are closest to each representative
  ## point than to any other, using a Delaunay partition of the space.

  ## The resulting object is of class "sr_obs"
  expect_is(srf, "sr_obs")

  ## The realised neighbourhood distance parameter is the midpoint of
  ## the original interval
  expect_equal(ntp, mean(neigh_tol(sro)))

  ## The overall earliest day has been selected
  expect_true(all(sro$date >= min(srf$date)))

  neighbours <-
    st_is_within_distance(srf, sro, ntp, sparse = TRUE)
  earliest_neighbour_dates <-
    vapply(neighbours, function(x) min(sro$date[x]), 1)
  expect_equal(srf$date, earliest_neighbour_dates)

})


test_that("Filter earliest neighbours of MC replicas with an interval of relative values of ntp", {

  #### Interval of relative neigh_tol values ####
  sro <- sr_obs(obs_prj, "date", uq = sr_uq(10, 1, 1, -c(10, 20)))
  srf <- expect_error(filter_earliest_neigh(sro), NA)


  ## Since the neigh_tol interval was expressed in relative terms,
  ## the MC replicates of the interval are also random.
  ntp_int <- neigh_tol(sro)  # neigh_tol interval in m
  ntp_mc_int <- vapply(attr(sro, "mc"), neigh_tol, numeric(2))
  expect_true(all(apply(ntp_mc_int, 1, sd) > 0))

  mc <- attr(srf, "mc")
  expect_true(all(vapply(mc, inherits, TRUE, "sr_obs")))
  expect_true(all(vapply(mc, nrow, 1) <= nrow(sro)))

  ## The filtered datasets though, have a __realised__ ntp
  ## which must be within the correspongin range (at 99.9%)
  ntp_mc <- vapply(attr(srf, "mc"), neigh_tol, 1)
  expect_true(all(ntp_mc >= ntp_mc_int[1,] & ntp_mc <= ntp_mc_int[2,]))

})

test_that("Filter earliest neighbours of MC replicas with constant ntp value", {

  #### Constant absolute value of neigh_tol ####
  sro <- sr_obs(obs_prj, "date", uq = sr_uq(10, 1, 1, 2))
  srf <- expect_error(filter_earliest_neigh(sro), NA)


  ## Since the neigh_tol value was expressed in absolute terms,
  ## the MC replicates share the same value.
  ntp_int <- neigh_tol(sro)  # neigh_tol interval in m
  ntp_mc_int <- vapply(attr(sro, "mc"), neigh_tol, numeric(1))
  expect_identical(sd(ntp_mc_int), 0)
  expect_identical(ntp_mc_int[1], ntp_int)

  mc <- attr(srf, "mc")
  expect_true(all(vapply(mc, inherits, TRUE, "sr_obs")))
  expect_true(all(vapply(mc, nrow, 1) <= nrow(sro)))

  ## The filtered datasets also have exactly the same ntp value
  ntp_mc <- vapply(attr(srf, "mc"), neigh_tol, 1)
  expect_true(all(ntp_mc == 2))

})

test_that("Filter earliest neighbours of MC replicas with an interval of constant ntp values", {

  #### Constant absolute value of neigh_tol ####
  sro <- sr_obs(obs_prj, "date", uq = sr_uq(10, 1, 1, c(2, 3)))
  srf <- expect_error(filter_earliest_neigh(sro), NA)


  ## Since the neigh_tol interval was expressed in absolute terms,
  ## the MC replicates share the same values.
  ntp_int <- neigh_tol(sro)  # neigh_tol interval in m
  ntp_mc_int <- vapply(attr(sro, "mc"), neigh_tol, numeric(2))
  expect_identical(apply(ntp_mc_int, 1, sd), c(0, 0))
  expect_identical(ntp_mc_int[, 1], ntp_int)

  mc <- attr(srf, "mc")
  expect_true(all(vapply(mc, inherits, TRUE, "sr_obs")))
  expect_true(all(vapply(mc, nrow, 1) <= nrow(sro)))

  ## Since an interval was given, realised values are randomly
  ## chosen from that constant interval (at 99.9%)
  ntp_mc <- vapply(attr(srf, "mc"), neigh_tol, 1)
  expect_true(all(ntp_mc >= 2 & ntp_mc <= 3))

})

test_that("Selection of representative points is reasonable", {

  d_tol <- .5
  rx <- representative_points(obs_prj, dTolerance = d_tol)

  # ggplot(obs_prj) + geom_sf() + geom_sf(data = rx, col = "red") +
  #   ggforce::geom_circle(
  #     data = data.frame(st_coordinates(rx), r = d_tol),
  #     aes(x0 = X, y0 = Y, r = r))

  ## All points are from x, but some are left out
  expect_equal(st_intersection(st_geometry(obs_prj), rx), st_geometry(rx),
               check.attributes = FALSE)
  expect_true(nrow(rx) < nrow(obs_prj))

  ## All original points are within dTolerance from one of the
  ## selected points. Either the point itself is selected, and the
  ## distance to the nearest selected point is 0, or it is not selected
  ## and the distance must be within tolerance.
  expect_true(all(apply(st_distance(obs_prj, rx), 1, min) < d_tol))

  ## If the tolerance is small enough, all points are taken
  small_d <- sort(unique(st_distance(obs_prj)))[2] / 2
  rx <- representative_points(obs_prj, small_d)
  expect_equal(st_intersection(st_geometry(obs_prj), rx), st_geometry(rx),
               check.attributes = FALSE)
  expect_identical(nrow(rx), nrow(obs_prj))

})



test_that("The approximation of arc-distances works as expected", {

  expect_maxerror <- function(x, test_d, tol) {
    ang_1km <- dist2arc(x, test_d)
    avg_err <- attr(ang_1km, "average_error")

    x_e <- st_set_crs(x + c(ang_1km, 0), 4326)
    x_n <- st_set_crs(x + c(0, ang_1km), 4326)

    err_e <- abs(as.numeric(st_distance(x, x_e)) - test_d)
    err_n <- abs(as.numeric(st_distance(x, x_n)) - test_d)

    ## The average error is between the easting and northing errors
    expect_true(min(err_e, err_n) < avg_err)
    expect_true(max(err_e, err_n) > avg_err)

    ## The average error is within tolerance
    expect_gt(test_d / avg_err, 1/tol)
  }

  ## A point in France
  x_fr <- st_sfc(st_point(c(2, 47), dim = "XY"), crs = 4326)

  ## Expect accuracies up to 20 %
  expect_maxerror(x_fr, 1e4, 20/100)
  expect_maxerror(x_fr, 1e3, 20/100)
  expect_maxerror(x_fr, 1e2, 20/100)
  expect_maxerror(x_fr, 1e1, 20/100)
  ## Cannot expect precisions to the meter. Ok.
  # expect_maxerror(x_fr, 1e0, 20/100)

  ## A point in Sweden
  x_sw <- st_sfc(st_point(c(16, 64), dim = "XY"), crs = 4326)

  ## Cannot expect accuracies of 20 %. Ok.
  # expect_maxerror(x_sw, 1e4, 20/100)
  ## Expect accuracies up to 40 %
  expect_maxerror(x_sw, 1e4, 40/100)
  expect_maxerror(x_sw, 1e3, 40/100)
  expect_maxerror(x_sw, 1e2, 40/100)
  expect_maxerror(x_sw, 1e1, 40/100)
  ## Cannot expect precisions to the meter. Ok.
  # expect_maxerror(x_sw, 1e0, 40/100)

  ## A point in RDC
  x_rdc <- st_sfc(st_point(c(26, 0), dim = "XY"), crs = 4326)

  ## Cannot expect accuracies of 1 %. Ok.
  # expect_maxerror(x_rdc, 1e4, 1/100)
  ## Expect accuracies up to 10 %
  expect_maxerror(x_rdc, 1e4, 1/100)
  expect_maxerror(x_rdc, 1e3, 1/100)
  expect_maxerror(x_rdc, 1e2, 1/100)
  expect_maxerror(x_rdc, 1e1, 1/100)
  ## Cannot expect precisions to the meter. Ok.
  # expect_maxerror(x_rdc, 1e0, 1/10)

  ## Extreme values
  x0 <- st_sfc(st_point(c(0, 0)), crs = 4326)
  x1 <- st_sfc(st_point(c(180, 0)), crs = 4326)
  x2 <- st_sfc(st_point(c(-180, 0)), crs = 4326)
  x3 <- st_sfc(st_point(c(0, 90)), crs = 4326)
  x4 <- st_sfc(st_point(c(0, -90)), crs = 4326)
  expect_equal(dist2arc(x0, 1e3), dist2arc(x1, 1e3))
  expect_equal(dist2arc(x0, 1e3), dist2arc(x2, 1e3))
  expect_equal(dist2arc(x3, 1e3), dist2arc(x4, 1e3))
})
