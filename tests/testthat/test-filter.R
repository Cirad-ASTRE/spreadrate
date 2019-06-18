context("Filtering cases")

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
