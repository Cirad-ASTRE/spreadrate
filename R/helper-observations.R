## Projected and unprojected obserrvations to be used in tests
## Both objects can be read directly by sr_obs()

set.seed(20190619)

## Unprojected observations (geographical coordinates) as data.frame
## random points in the unit square (of decimal degrees) the date
## variable is approximately (rounded) 10 times the distance to the
## origin.
obs_geo <- local({

  d <- data.frame(lon = runif(30), lat = runif(30))
  transform(d, date = round(10 * sqrt(lon**2 + lat**2)))
})

## Projected observations (EPSG = 3857 ~ webMercator) as sf object
## random points in the 10 x 10 m square the date variable is
## approximately (rounded) 10 times the distance to the origin.
obs_prj <- local({

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
  st_sf(d, date = round(d_dists))
})
