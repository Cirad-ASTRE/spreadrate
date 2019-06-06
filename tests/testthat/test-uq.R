context("Specify Uncertainty Quantification")


test_that("sr_uq() returns a list with the given values", {

  uq <- sr_uq(nsim = 1, space = 2, time = 3, neigh_tol = 4)

  expect_uq <- function(x) {
    expect_s3_class(x, c("sr_uq", "list"))
    expect_length(x, 4)
    expect_named(x, c("nsim", "space", "time", "neigh_tol"))
  }

  expect_uq(sr_uq())
  expect_uq(sr_uq(neigh_tol = 0))
  expect_uq(sr_uq(neigh_tol = 800))
  expect_uq(sr_uq(neigh_tol = c(-100, 10000)))
  expect_uq(sr_uq(nsim = 1, space = 2, time = 3, neigh_tol = 4))
  uq <- expect_warning(
    # Trivial replicates
    sr_uq(nsim = 10),
    "the replicated datasets will be identical"
  )
  expect_uq(uq)
  expect_identical(uq$nsim, 1L)

  uq <- expect_warning(sr_uq(nsim = 2.5, space = 10))
  expect_uq(uq)
  expect_identical(uq$nsim, 2L)
})

test_that("sr_uq() rejects invalid arguments and values", {

  expect_error(sr_uq(nsim = 0))
  expect_error(sr_uq(nsim = 0.5))
  expect_error(sr_uq(nsim = -1))
  expect_error(sr_uq(space = -1))
  expect_error(sr_uq(time = -1))
  expect_error(sr_uq(neigh_tol = -101), "percentage")
  expect_error(sr_uq(neigh_tol = c(-1e3, 10)), "percentage")
  expect_error(sr_uq(neigh_tol = 1:3), "length")
})
