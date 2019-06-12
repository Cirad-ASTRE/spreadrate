context("Creation of spread-rate observation objects")


test_that(
  "get_lonlatvars correctly identifies variable names when possible",
  {

    dummy_df <- function(x) {
      setNames(do.call("data.frame", as.list(seq_along(x))), nm = x)
    }

    ## Identify variables `x` among those in `among`
    expect_identify_among <- function (x, among) {
      d <- dummy_df(among)
      # eval(bquote(expect_identical(.(get_lonlatvars(d)), .(x))))
      expect_identical(get_lonlatvars(d), x,
                       label = paste(get_lonlatvars(d), collapse = ", "),
                       expected.label = paste(x, collapse = ", "))
    }


    ## Variations in name specification include any combination of
    ## capitalisation, order, location, abbreviation
    expect_identify_among(c("lon", "lat"), c("lon", "lat", "a"))
    expect_identify_among(c("Long", "Lat"), c("a", "Long", "b", "Lat", "c"))
    expect_identify_among(c("LON", "LAT"), c("a", "LAT", "b", "LON", "c"))
    expect_identify_among(c("longitude", "latitude"), c("latitude", "longitude"))

    #### Expected errors ####

    ## Not uniquely identifiable
    expect_error(
      get_lonlatvars(dummy_df(c("latitude", "lon", "longitude"))),
      "Could not identify variables for longitude"
    )

    ## Missing or not in expected pattern
    expect_error(
      get_lonlatvars(dummy_df(c("latitude", "lonitude"))),
      "Could not identify variables for longitude"
    )

  }
)

test_that("sr_obs() handles different names for the time variable", {
  d <- data.frame(lon = 1, lat = 1, t = 1)
  uq <- sr_uq(3, 1, 1)
  r <- sr_obs(d, "t", uq)

  mc <- attr(r, "mc")
  expect_true("t" %in% names(r))
  expect_true(
    all(vapply(mc, function(x) "t" %in% names(x), TRUE))
  )
})


test_that("sr_obs() handles dates in the time variable", {

  d <- data.frame(lon = 1:3, lat = 1:3, t = Sys.Date() + 1:3)
  uq <- sr_uq(3, 1, 1)
  r <- sr_obs(d, "t", uq)

  mc <- attr(r, "mc")
  expect_true("t" %in% names(r))
  inherits(r$t, "Date")

  expect_true(
    all(vapply(mc, function(x) "t" %in% names(x), TRUE))
  )
  expect_true(
    all(vapply(mc, function(x) inherits(x$t, "Date"), TRUE))
  )

})

test_that("sr_obs() fails if time variable is missing", {
  d <- data.frame(lon = 1:3, lat = 1:3, t = Sys.Date() + 1:3)
  expect_error(sr_obs(d), "timevar")  # missing
  expect_error(sr_obs(d, "time"), "timevar")  # wrong name
})

test_that("sr_obs() preserves coordinates", {
  d <- data.frame(lon = 1:3, lat = 1:3, t = Sys.Date() + 1:3)
  r <- sr_obs(d, "t", uq)

  expect_true(
    isTRUE(
      all.equal(
        st_coordinates(r),
        as.matrix(d[, c("lon", "lat")]),
        check.attributes = FALSE
      )
    )
  )

})

test_that("sr_obs() produces Monte Carlo samples conforming to uq",{

  set.seed(20190617)
  Npoints <- 50
  nsim = 50

  d <- data.frame(
    lon = runif(Npoints, 0, 10),
    lat = runif(Npoints, -10, 10),
    t = Sys.Date() + seq.int(Npoints))

  ## the points in mc are within space tolerance from r
  ## coordinates are jittered independently within the interval
  ## (-radius, radius).
  expect_within_radius <- function(x, radius) {
    all(
      abs(st_coordinates(x) - st_coordinates(r)) <= radius
    )
  }

  expect_within_time <- function(x, time) {
    ## the times are jittered within time tolerance with a decreasing
    ## distribution approximately halving each time
    time_diffs <- as.numeric(x$t - r$t)
    if (any(abs(time_diffs) > time)) return(FALSE)

    ## The first half of this vector should be appr. 1 and the second
    ## half should be appr. -1
    td_difflog2 <- as.numeric(diff(log2(table(time_diffs))))

    if (!isTRUE(
      all.equal(
        td_difflog2 + rev(td_difflog2),  # so this should be about 0
        rep(0, length(td_difflog2)),
        tolerance = 1
      )
    )) return(FALSE)

    return(TRUE)
  }

  # space = 1
  # time = 1


  expect_within_tolerance <- function(space, time) {

    uq <- sr_uq(nsim, space, time)
    r <- sr_obs(d, "t", uq)
    mc <- attr(r, "mc")

    ## mc is a list of nsim sr_obs objects of the same size as d
    expect_is(mc, "list")
    expect_length(mc, nsim)
    expect_true(all(vapply(mc, inherits, TRUE, "sr_obs")))
    expect_true(all(vapply(mc, nrow, 1) == nrow(d)))

    ## the points are jittered within space tolerance in each
    ## dimension separately
    expect_true(all(vapply(mc, expect_within_radius, TRUE, space)))
    expect_true(all(vapply(mc, expect_within_time, TRUE, time)))

  }

  expect_within_tolerance(1, 1)
  expect_within_tolerance(3, 0)
  expect_within_tolerance(0, 5)



})
