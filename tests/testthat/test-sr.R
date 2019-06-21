context("Srpead-rate computations")

test_that("sr() works as expected without UQ and default values", {

  sro <- sr_obs(obs_prj, "date")
  sr <- expect_error(sr(sro), NA)

  expect_is(sr, "RasterLayer")

  ## It returns the fdoi as an attribute
  fdoi <- attr(sr, "fdoi")
  expect_true(!is.null(fdoi))
  expect_is(fdoi, "RasterLayer")

  # plot(fdoi, col = hcl.colors(12))
  # plot(sr, col = hcl.colors(12))
})


test_that("sr() yields MC estimates of spread-rate", {

  n_mc <- 10L
  uq <- sr_uq(n_mc, space = 1, time = 1, neigh_tol = -c(10, 20))
  sro <- sr_obs(obs_prj, "date", uq = uq)
  sr <- expect_error(sr(sro), NA)

  sr_mc <- attr(sr, "mc")

  expect_is(sr_mc, "RasterBrick")
  expect_equal(nlayers(sr_mc), n_mc)

  ## It returns the fdoi MC replicas as an "mc" attribute of fdoi
  fdoi <- attr(sr, "fdoi")
  fdoi_mc <- attr(fdoi, "mc")

  expect_true(!is.null(fdoi_mc))
  expect_is(fdoi_mc, "RasterBrick")
  expect_equal(nlayers(fdoi_mc), n_mc)

  # plot(fdoi_mc, col = hcl.colors(12))
  # plot(sr_mc, col = hcl.colors(12))
})
