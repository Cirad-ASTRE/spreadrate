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
