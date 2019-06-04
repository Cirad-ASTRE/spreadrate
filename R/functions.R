#' Convert angles from sexagesimal to decimal format
sexa2dec <- function(deg, min, sec) {
  as.numeric(deg) + as.numeric(min)/60 + as.numeric(sec)/3600
}


#' Clean up ASF reported cases in Belgium
cleanup <- function(dat_raw, target_crs) {

  asf_be <-
    dat_raw %>%
    ## Remove empty columns
    select_if(~!all(is.na(.))) %>%
    ## Compute coordinates in decimal degrees
    mutate(
      lon = sexa2dec(`Longitude x�`, `Longitude y'`, `Longitude z''`),
      lat = sexa2dec(`Latitude x�`, `Latitude y'`, `Latitude z''`)
    ) %>%
    ## Dates
    mutate(
      date = as.Date(
        ifelse(
          is.na(`Suspicion date`),
          `Confirmation date`,
          `Suspicion date`
        ),
        format="%d/%m/%Y"
      )
    ) %>%
    arrange(
      ## Descending order, for plotting
      ## most recent cases plotted first and earlier cases
      ## plotted on top.
      desc(date)
    ) %>%
    mutate(
      month = fct_inorder(months(date), ordered = TRUE)
    ) %>%
    ## Select relevant variables
    dplyr::select(
      lon, lat, date, month
    ) %>%
    clean_names() %>%
    st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326
    ) %>%
    st_transform(st_crs(target_crs))

  return(asf_be)
}

#' Prepare cartography of the region
get_cartography <- function(countries.names, crs) {

  countries.iso3 <-
    raster::ccodes() %>%
    filter(NAME %in% countries.names) %>%
    with(., as.vector(ISO3))

  # I tried cartography::countries.spdf but it is too coarse
  countries <-
    purrr::map(
      countries.iso3,
      ~getData(path = here::here("data"), country = ., level = 0)
    ) %>%
    purrr::map(st_as_sf) %>%
    do.call(rbind, .) %>%
    st_transform(crs) %>%   # https://epsg.io/27561
    ## relationship between attributes and geometries
    st_set_agr("identity")

}

asfbel_animation <- function(asf_be, countries, output_file) {

  # asf_be <- readd(clean_data)
  # countries <- readd(cartography)

  pacman::p_load(
    concaveman,    # concave hull
    ggalt,         # geometry for encircling points geom_encircle
    gganimate,     # animated plots
    ggsn,          # scale bar and north arrow in ggmaps
    tmaptools
  )

  p_static <-
    asf_be %>%
    ## Include the corresponding year in the first and last month of
    ## the observed data
    mutate(
      month = fct_recode(
        month,
        `Sep. 2018` = "September",
        `Apr. 2019` = "April"
      )
    ) %>%
    ggplot() +
    geom_sf(
      data = st_intersection(countries, st_as_sfc(bb(asf_be, ext = -2.3))),
      fill = "grey85",
      colour = "grey95",
      lwd = 1
    ) +
    geom_text(
      data = countries %>%
        st_intersection(st_as_sfc(bb(asf_be, ext = -2.3))) %>%
        st_set_agr("identity") %>%
        st_centroid %>%
        st_coordinates %>%
        data.frame() %>%
        mutate(
          Y = Y + c(1e4, -2e3, 1e4)
        ) %>%
        bind_cols(st_drop_geometry(countries)),
      aes(X, Y, label = NAME_0)
    ) +
    ## Concave Hull
    geom_envelope(
      data = asf_be %>%
        rename(occurrence = date) %>%
        tidyr::crossing(
          date = seq(from = min(asf_be$date), to = max(asf_be$date), by = 1)
        ) %>%
        filter(occurrence <= date),
      colour = "steelblue",
      fill = "white",
      alpha = .5
    ) +
    # ## Concave Hull 2
    # geom_chull(
    #   aes(X, Y),
    #   data = asf_be %>%
    #     ## For each day, gather all the points that appeared up to it
    #     st_coordinates() %>%
    #     data.frame() %>%
    #     add_column(occurrence = asf_be$date) %>%
    #     tidyr::crossing(
    #       date = seq(from = min(asf_be$date), to = max(asf_be$date), by = 1)
    #     ) %>%
    #     filter(occurrence <= date),
    #   colour = "steelblue",
    #   fill = "white",
    #   alpha = .5
    # ) +
    # ## Convex Hull
    # geom_encircle(
    #   aes(X, Y),
    #   data = asf_be %>%
    #     ## For each day, gather all the points that appeared up to it
    #     st_coordinates() %>%
    #     data.frame() %>%
    #     add_column(occurrence = asf_be$date) %>%
    #     tidyr::crossing(
    #       date = seq(from = min(asf_be$date), to = max(asf_be$date), by = 1)
    #     ) %>%
  #     filter(occurrence <= date),
  #   expand = 0.01,
  #   s_shape = .8,
  #   colour = "steelblue",
  #   fill = "white",
  #   alpha = .5
  # ) +
  ## Points of observation
  geom_sf(aes(colour = month, fill = month)) +
    coord_sf(expand = FALSE) +  # remove expansion to panel area
    labs(
      title = "Progress of wild boar African Swine Fever in Belgium",
      subtitle = "Date: {frame_time}",
      x = "Easting",
      y = "Northing"
    ) +
    # north(
    #   asf_be,
    #   location = "topleft",
    #   scale = .3,
    #   anchor = c(x = 810e3, y = 245e3)
    # ) +
    ggsn::scalebar(
      asf_be,
      dist = 5,
      dist_unit = "km",
      transform = FALSE,
      anchor = c(x = 860e3, y = 2e5),
      nudge_y = -1e3,
      height = .04,
      border.size = .5,
      st.size = 4
    ) +
    # ## Custom scale bar
    # geom_polygon(
    #   data = data.frame(
    #     x = c(850e3 + c(0, 5e3), 850e3 + c(5e3, 0)),
    #     y = c(200e3 + c(0, 0), 200e3 + c(1e3, 1e3))
    #   ),
    #   aes(x, y),
    #   fill = "black",
    #   colour = "black"
    # ) +
    # geom_polygon(
    #   data = data.frame(
    #     x = 5e3 + c(850e3 + c(0, 5e3), 850e3 + c(5e3, 0)),
    #     y = c(200e3 + c(0, 0), 200e3 + c(1e3, 1e3))
    #   ),
    #   aes(x, y),
    #   fill = "white",
    #   colour = "black"
    # ) +
    # annotate(
    #   "text",
    #   label = c("0", "5", "10 km"),
    #   x = 850e3 + 5e3 * (0:2),
    #   y = 199e3
    # ) +
    theme_ipsum(
      axis=T,
      grid = FALSE,
      plot_margin = margin(10, 10, 10, 10)
    ) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(colour = "transparent"),
      panel.border = element_rect(colour = "black", fill = "transparent"),
      legend.justification = c(0, 0),
      legend.position = c(0, 0),
      legend.background = element_rect(
        colour = alpha("black", .15),
        fill = alpha("white", .15)
      )
    ) +
    scale_colour_viridis_d(NULL, guide = guide_legend(reverse = T)) +
    scale_fill_viridis_d(NULL, guide = guide_legend(reverse = T))

  # p_static + ggthemes::theme_map()

  p <- p_static +
    transition_time(date) +
    # transition_time(date, range = max(asf_be$date) - c(10, 0)) +
    shadow_mark(size = 1, colour = colour, alpha = .5, exclude_layer = 3)

  # ## Smaller animation for debugging
  # gganimate::animate(
  #   p,
  #   nframes = 21,
  #   fps = 6,
  #   width = 600,
  #   height = 600,
  #   res = 72
  # )


  gganimate::animate(
    p,
    nframes = length(unique(asf_be$date)) + 3,
    fps = 6,
    width = 800,
    height = 800,
    res = 96,
    start_pause = 5*1,
    end_pause = 5*4
  )

  anim_save(output_file)

}

#' Compute an envelope for a set of points
envelope <- function(x, buffer_dist = .8e3) {
  # ## Using concaveman
  # x %>%
  #   st_buffer(dist = buffer_dist) %>%
  #   st_union() %>%
  #   st_segmentize(buffer_dist) %>%
  #   st_cast("POINT") %>%
  #   st_sf() %>%
  #   concaveman(
  #     concavity = 5,
  #     length_threshold = buffer_dist
  #   )
  ## Using INLA fmesher
  require(INLA)
  x %>%
  st_coordinates() %>%
  inla.nonconvex.hull(
    convex = buffer_dist,   # buffer distance from points
    concave = 10*buffer_dist,   # minimum curvature radius
    resolution = 100
  ) %>%
  with(., st_polygon(list(rbind(loc, loc[1,])))) %>%
  st_sfc(crs = st_crs(x))
}

StatEnvelope <- ggproto(
  "StatEnvelope", Stat,
  required_aes = "geometry",
  compute_group = function(data, scales) {
    if(nrow(data) <= 2) return (NULL)
    data %>%
      group_by_at(vars(-geometry)) %>%
      summarise(geometry = envelope(sf::st_combine(geometry))) %>%
      ungroup()
  }
)

geom_envelope <- function(...){
  suppressWarnings(
    geom_sf(
      stat = StatEnvelope,
      ..., # any aesthetic argument specified in the function
      # will take precedence over the default arguments
      # below, with suppressWarning to mute warnings on
      # any duplicated aesthetics
      alpha = 0.5, color = "steelblue", fill = "white")
  )
}


# StatChull <- ggproto(
#   "StatChull", Stat,
#   compute_group = function(data, scales) {
#     # data[chull(data$x, data$y), , drop = FALSE]
#
#     hull_df <- data.frame(st_coordinates(envelope(st_sfc(sf::st_multipoint(cbind(data$x, data$y))))))
#     names(hull_df) <- c("x", "y", "L1", "L2")
#
#
#     ## Further aesthetics
#     data.frame(hull_df, data[1, -match(c("x", "y"), names(data)), drop = FALSE], row.names = NULL)
#
#   },
#
#   required_aes = c("x", "y")
# )
# GeomPolygonHollow <- ggproto("GeomPolygonHollow", GeomPolygon,
#                              default_aes = aes(colour = "black", fill = NA, size = 0.5, linetype = 1,
#                                                alpha = NA)
# )
# geom_chull <- function(mapping = NULL, data = NULL,
#                        position = "identity", na.rm = FALSE, show.legend = NA,
#                        inherit.aes = TRUE, ...) {
#   layer(
#     stat = StatChull, geom = GeomPolygonHollow, data = data, mapping = mapping,
#     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#     params = list(na.rm = na.rm, ...)
#   )
# }


## Given a set of points and a column name with values
## return a subset of points at least a given distance apart from
## each other, with the minimum value found in the neighbourhood
min_neigh <- function(x, value, dist) {

  ## Subset of points at least dist far apart from each other
  # ans <-
  #   st_triangulate(st_combine(x), dTolerance = dist) %>%
  #   st_collection_extract("POLYGON") %>%
  #   st_coordinates() %>%
  #   `[`(, c("X", "Y")) %>%
  #   unique() %>%
  #   unname() %>%
  #   st_multipoint() %>%
  #   st_sfc(crs = st_crs(x)) %>%
  #   st_sf() %>%
  #   st_cast("POINT")

  ## Better use the triangulation methdos in INLA's fmesher
  ans <- representative_points(x, dist)

  ## Capture the minimum value of the neighbouring points
  ans[[value]] <-
    ans %>%
    st_is_within_distance(x, dist = dist, sparse = TRUE) %>%
    # map_dbl(~min(x[[value]][.]))
    vapply(function(.) min(x[[value]][.]), x[[value]][1])

  if (!is.null(cl <- attr(x[[value]], "class"))) {
    ## preserve class (e.g. for dates) if any
    class(ans[[value]]) <- cl
  }

  ## Move geometry to last position
  ans <- dplyr::select(ans, -geometry, geometry)

  return(ans)
}

representative_points <- function(x, dTolerance) {

  require(INLA)

  coord_x <- st_coordinates(x)

  # bnd <- inla.nonconvex.hull(
  #   coord_x,
  #   convex = 0,
  #   concave = -0.15,
  #   resolution = 40
  # )

  mesh <- inla.mesh.create(
    loc = st_coordinates(x),
    boundary = inla.mesh.segment(coord_x[rev(chull(coord_x)),]),
    extend = FALSE,
    refine = FALSE,
    cutoff = dTolerance
  )
  # x %>% ggplot() + gg(mesh) + geom_sf()

  ans <-
    mesh$loc[, 1:2] %>%
    st_multipoint() %>%
    st_sfc(crs = st_crs(x)) %>%
    st_cast("POINT") %>%
    st_sf()

  return(ans)
}

#' Raster termplate for prediction
#' x: scf_POINT dataset
#' buffer_size: expansion of the extent
estimation_mask <- function(x, buffer_size, res = buffer_size/2) {

  estimation_region <-
    x %>%
    st_combine() %>% # many points to 1 multipoint
    st_convex_hull() %>%
    st_buffer(dist = buffer_size) %>%  # 1 km buffer
    st_sf()  # create sf object with the previous geometry

  # estimation_region %>%
  #   ggplot() +
  #   geom_sf() +
  #   geom_sf(data = x)

  ## RasterLayer with values of 1 within the estimation_region
  ## and NA elsewhere
  # estimation_mask <-
  #   fasterize(
  #     estimation_region,
  #     raster(estimation_region, res = as.numeric(buffer_size/2))
  #   )

  estimation_mask <-
    rasterize(
      as(estimation_region, "Spatial"),
      raster(
        as(estimation_region, "Spatial"),
        res = as.numeric(res)
      )
    )

  return(estimation_mask)
}


#' x: dataset
#' r: raster template for prediction
#' uq: list with parameters for Uncertainty Quantification
#'   uq$space: maximum distance error in spatial coordinates
#'   uq$time: maximum temporal error in number of days
#'   uq$neigh_bnd: bounds of neighbourhood measure
#'   uq$nsim: number of samples from each source of variation
estimate_sr <- function(x, r, uq) {

  ## Average spread-rate: half-diameter of the dataset
  ## divided by total number of days
  diameter_km <- as.numeric(max(st_distance(x)) / 1e3)
  period_mn <- as.numeric(difftime(max(x$date), min(x$date)))/30
  avg_sr <- diameter_km/2/period_mn

  ## Prior spread-rate support (in km/month)
  ## 20 times less or more than average
  sr_bnd <- signif(avg_sr, 2) * c(1/20, 20)


  set.seed(20190426)

  ## Variation in data resolution
  sim_datasets <-
    future_map(
      seq.int(uq$nsim),
      ~st_jitter(x, uq$space) %>%
        mutate(
          date = time_jitter(date, uq$time),
          month = fct_inorder(months(date), ordered = TRUE)
        ) %>%
        arrange(desc(date))
    )
  # plot(st_geometry(x), pch = 19)
  # plot(st_geometry(st_jitter(x, 1e3)), pch = 19)

  ## Variation in neighbourhood threshold
  neigh_tol <- round(rnorm(uq$nsim, mean(uq$neigh_bnd), diff(uq$neigh_bnd)/6))

  mcmcdat <-
    tibble(
      data = sim_datasets,
      neigh_tol = neigh_tol
    )

  ## Run in parallel
  mcmcdat <-
    mcmcdat %>%
    mutate(
      x_earliest = future_map2(data, neigh_tol, filter_earliest_neigh),
      tps = future_map(x_earliest, fit_surface),
      ## The following two fail in parallel for somme reason
      fa = lapply(tps, estimate_fa, estimation_mask = r),
      sr = lapply(fa, fa2sr, filter_percentile = 1, bnd = sr_bnd)
    )

  ## Variation in conditional realisation of surface
  ## This takes a lot of time and even hangs depending on
  ## the case. Thus, I leave this factor out of the UQ for now.
  # sim.Krig(fm, xp = coordinates(r))

  return(mcmcdat)
}

filter_earliest_neigh <- function(x, tol) {
  min_neigh(x, "date", tol) %>%
    arrange(desc(date)) %>%
    mutate(
      month = fct_inorder(months(date), ordered = TRUE),
      day = as.numeric(date - min(date) + 1)
    )
}

fit_surface <- function(x) {
  tps <- Tps(
    st_coordinates(x),
    x$day,
    give.warning = TRUE
  )
}

estimate_fa <- function(model, estimation_mask) {

  ans <- setNames(interpolate(estimation_mask, model), "day")
  ans[is.na(estimation_mask)] <- NA
  return(ans)
}

#' fa_est: RasterLayer with estimated first-arrival (in days)
#' filter_percentile: thershold for topping-up the highest values of
#' spread-rate
#' bnd: reasonable expected boundaries for the result in km/month
#' Return spread rate in units of km/month
fa2sr <- function(fa_est, filter_percentile = 1, bnd) {

  ## spread-rate-neighbouring
  ## need to use some projection so it understands that the coordinates
  ## are in meters (not in degrees)
  fa_est_slope <- terrain(fa_est, unit = "tangent")
  sr_est <- setNames(1/fa_est_slope, "est_fasr")
  ## Convert units from m/day to km/month
  sr_est <- sr_est * 30 / 1e3

  # image(
  #   sr_est,
  #   col = viridis::viridis(256),
  #   asp = 1,
  #   axes = FALSE,
  #   ann = FALSE
  #   )
  # hist(sr_est)

  ## Filter-out the x highest values of spread-rate
  # min_sr <- quantile(values(sr_est), filter_percentile/100, na.rm = TRUE)
  max_sr <- min(
    bnd[2],
    quantile(values(sr_est), 1-filter_percentile/100, na.rm = TRUE)
  )

  ## Apply limits
  sr_est[sr_est > max_sr] <- max_sr
  sr_est[sr_est < bnd[1]] <- bnd[1]

  return(sr_est)
}


#' Modify a vector of dates by a random quantity in [-e, e]
#' The distribution decreases exponentially, with rate 1/2
time_jitter <- function(x, e) {

  prob_p <- 1/2^seq(0, e)
  probs <- c(rev(prob_p), tail(prob_p, -1))
  # probs <- probs/sum(probs)  # unnecessary

  x + sample(seq(-e, e), size = length(x), prob = probs, replace = TRUE)

}

#' Summaries of Monte Carlo samples
#'
#' x: data.frame with MC results: data, neigh_tol, x_earliest, tps, fa, sr
#' pts: sfc_POINTS with observed locations (where SR is assessed)
#' int_prob: prbability level for MC intervals
sr_summaries <- function(x, pts, int_prob) {
  stack_sr <- stack(x$sr)

  ## Temporal trends

  temp_trends <-
    purrr::pmap_dfr(
      x %>% select(data, sr),
      function(data, sr)
        data %>%
        mutate(
          sr = raster::extract(sr, as(data, "Spatial"))
        ) %>%
        st_drop_geometry() %>%
        select(date, sr),
      .id = "replicate"
    )

  ## MC samples of global SR density
  mc_global_density <-
    raster::extract(stack_sr, as(pts, "Spatial")) %>%
    data.frame() %>%
    purrr::map(
      ~data.frame(
        stats::density(
          .,
          from = min(minValue(stack_sr)),
          to = max(maxValue(stack_sr))
        )[c("x", "y")]
      )
    ) %>%
    bind_rows(.id = "replicate") %>%
    mutate(replicate = as.numeric(gsub("^est_fasr\\.", "", replicate)))

  ## Point-wise SR MC-density
  point_density <-
    raster::extract(stack_sr, as(pts, "Spatial")) %>%
    t() %>%
    data.frame() %>%
    purrr::map(
      ~data.frame(
        stats::density(
          .,
          from = min(minValue(stack_sr)),
          to = max(maxValue(stack_sr))
        )[c("x", "y")]
      )
    ) %>%
    bind_rows(.id = "location") %>%
    mutate(location = as.numeric(gsub("^X", "", location)))

  ## Mean global SR density
  mean_sr_dens <-
    mc_global_density %>%
    group_by(x) %>%
    summarise(y = mean(y))

  ## MC samples of Mean global SR values
  mc_avgs <- raster::extract(stack_sr, as(pts, "Spatial")) %>%
    colMeans()

  ## Estimate of global SR value
  global_avg <- c(
    mean = mean(mc_avgs), quantile(mc_avgs, probs = c(1 - c(1, -1) * int_prob) / 2 )
  )

  ## Map MC summaries
  map_summaries <- brick(
    # sr_est,
    mean(stack_sr),
    calc(stack_sr, fun = median),
    calc(stack_sr, fun = sd)
  ) %>%
    setNames(c("avg", "mdn", "sd"))


  ans <- list(
    mc_global_density = mc_global_density,
    temp_trends = temp_trends,
    point_density = point_density,
    mean_sr_dens = mean_sr_dens,
    mc_avgs = mc_avgs,
    global_avg = global_avg,
    map_summaries = map_summaries
  )
}
