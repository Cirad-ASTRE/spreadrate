### Currently unused, but potentially interesting functions ###

# asfbel_animation <- function(asf_be, countries, output_file) {
#
#   # asf_be <- readd(clean_data)
#   # countries <- readd(cartography)
#
#   pacman::p_load(
#     concaveman,    # concave hull
#     ggalt,         # geometry for encircling points geom_encircle
#     gganimate,     # animated plots
#     ggsn,          # scale bar and north arrow in ggmaps
#     tmaptools
#   )
#
#   p_static <-
#     asf_be %>%
#     ## Include the corresponding year in the first and last month of
#     ## the observed data
#     mutate(
#       month = fct_recode(
#         month,
#         `Sep. 2018` = "September",
#         `Apr. 2019` = "April"
#       )
#     ) %>%
#     ggplot() +
#     geom_sf(
#       data = st_intersection(countries, st_as_sfc(bb(asf_be, ext = -2.3))),
#       fill = "grey85",
#       colour = "grey95",
#       lwd = 1
#     ) +
#     geom_text(
#       data = countries %>%
#         st_intersection(st_as_sfc(bb(asf_be, ext = -2.3))) %>%
#         st_set_agr("identity") %>%
#         st_centroid %>%
#         st_coordinates %>%
#         data.frame() %>%
#         mutate(
#           Y = Y + c(1e4, -2e3, 1e4)
#         ) %>%
#         bind_cols(st_drop_geometry(countries)),
#       aes(X, Y, label = NAME_0)
#     ) +
#     ## Concave Hull
#     geom_envelope(
#       data = asf_be %>%
#         rename(occurrence = date) %>%
#         tidyr::crossing(
#           date = seq(from = min(asf_be$date), to = max(asf_be$date), by = 1)
#         ) %>%
#         filter(occurrence <= date),
#       colour = "steelblue",
#       fill = "white",
#       alpha = .5
#     ) +
#     # ## Concave Hull 2
#     # geom_chull(
#     #   aes(X, Y),
#     #   data = asf_be %>%
#     #     ## For each day, gather all the points that appeared up to it
#     #     st_coordinates() %>%
#     #     data.frame() %>%
#     #     add_column(occurrence = asf_be$date) %>%
#     #     tidyr::crossing(
#     #       date = seq(from = min(asf_be$date), to = max(asf_be$date), by = 1)
#     #     ) %>%
#     #     filter(occurrence <= date),
#     #   colour = "steelblue",
#     #   fill = "white",
#     #   alpha = .5
#     # ) +
#     # ## Convex Hull
#     # geom_encircle(
#     #   aes(X, Y),
#     #   data = asf_be %>%
#     #     ## For each day, gather all the points that appeared up to it
#     #     st_coordinates() %>%
#     #     data.frame() %>%
#     #     add_column(occurrence = asf_be$date) %>%
#     #     tidyr::crossing(
#     #       date = seq(from = min(asf_be$date), to = max(asf_be$date), by = 1)
#     #     ) %>%
#   #     filter(occurrence <= date),
#   #   expand = 0.01,
#   #   s_shape = .8,
#   #   colour = "steelblue",
#   #   fill = "white",
#   #   alpha = .5
#   # ) +
#   ## Points of observation
#   geom_sf(aes(colour = month, fill = month)) +
#     coord_sf(expand = FALSE) +  # remove expansion to panel area
#     labs(
#       title = "Progress of wild boar African Swine Fever in Belgium",
#       subtitle = "Date: {frame_time}",
#       x = "Easting",
#       y = "Northing"
#     ) +
#     # north(
#     #   asf_be,
#     #   location = "topleft",
#     #   scale = .3,
#     #   anchor = c(x = 810e3, y = 245e3)
#     # ) +
#     ggsn::scalebar(
#       asf_be,
#       dist = 5,
#       dist_unit = "km",
#       transform = FALSE,
#       anchor = c(x = 860e3, y = 2e5),
#       nudge_y = -1e3,
#       height = .04,
#       border.size = .5,
#       st.size = 4
#     ) +
#     # ## Custom scale bar
#     # geom_polygon(
#     #   data = data.frame(
#     #     x = c(850e3 + c(0, 5e3), 850e3 + c(5e3, 0)),
#     #     y = c(200e3 + c(0, 0), 200e3 + c(1e3, 1e3))
#     #   ),
#     #   aes(x, y),
#     #   fill = "black",
#     #   colour = "black"
#     # ) +
#     # geom_polygon(
#     #   data = data.frame(
#     #     x = 5e3 + c(850e3 + c(0, 5e3), 850e3 + c(5e3, 0)),
#     #     y = c(200e3 + c(0, 0), 200e3 + c(1e3, 1e3))
#     #   ),
#     #   aes(x, y),
#     #   fill = "white",
#     #   colour = "black"
#     # ) +
#     # annotate(
#     #   "text",
#     #   label = c("0", "5", "10 km"),
#     #   x = 850e3 + 5e3 * (0:2),
#     #   y = 199e3
#     # ) +
#     theme_ipsum(
#       axis=T,
#       grid = FALSE,
#       plot_margin = margin(10, 10, 10, 10)
#     ) +
#     theme(
#       axis.text = element_blank(),
#       axis.ticks = element_blank(),
#       panel.grid.major = element_line(colour = "transparent"),
#       panel.border = element_rect(colour = "black", fill = "transparent"),
#       legend.justification = c(0, 0),
#       legend.position = c(0, 0),
#       legend.background = element_rect(
#         colour = alpha("black", .15),
#         fill = alpha("white", .15)
#       )
#     ) +
#     scale_colour_viridis_d(NULL, guide = guide_legend(reverse = T)) +
#     scale_fill_viridis_d(NULL, guide = guide_legend(reverse = T))
#
#   # p_static + ggthemes::theme_map()
#
#   p <- p_static +
#     transition_time(date) +
#     # transition_time(date, range = max(asf_be$date) - c(10, 0)) +
#     shadow_mark(size = 1, colour = colour, alpha = .5, exclude_layer = 3)
#
#   # ## Smaller animation for debugging
#   # gganimate::animate(
#   #   p,
#   #   nframes = 21,
#   #   fps = 6,
#   #   width = 600,
#   #   height = 600,
#   #   res = 72
#   # )
#
#
#   gganimate::animate(
#     p,
#     nframes = length(unique(asf_be$date)) + 3,
#     fps = 6,
#     width = 800,
#     height = 800,
#     res = 96,
#     start_pause = 5*1,
#     end_pause = 5*4
#   )
#
#   anim_save(output_file)
#
# }

# Compute an envelope for a set of points
# envelope <- function(x, buffer_dist = .8e3) {
#   # ## Using concaveman
#   # x %>%
#   #   st_buffer(dist = buffer_dist) %>%
#   #   st_union() %>%
#   #   st_segmentize(buffer_dist) %>%
#   #   st_cast("POINT") %>%
#   #   st_sf() %>%
#   #   concaveman(
#   #     concavity = 5,
#   #     length_threshold = buffer_dist
#   #   )
#   ## Using INLA fmesher
#   # require(INLA)
#   x %>%
#   st_coordinates() %>%
#   inla.nonconvex.hull(
#     convex = buffer_dist,   # buffer distance from points
#     concave = 10*buffer_dist,   # minimum curvature radius
#     resolution = 100
#   ) %>%
#   with(., st_polygon(list(rbind(loc, loc[1,])))) %>%
#   st_sfc(crs = st_crs(x))
# }

# StatEnvelope <- ggproto(
#   "StatEnvelope", Stat,
#   required_aes = "geometry",
#   compute_group = function(data, scales) {
#     if(nrow(data) <= 2) return (NULL)
#     data %>%
#       group_by_at(vars(-geometry)) %>%
#       summarise(geometry = envelope(sf::st_combine(geometry))) %>%
#       ungroup()
#   }
# )

# geom_envelope <- function(...){
#   suppressWarnings(
#     geom_sf(
#       stat = StatEnvelope,
#       ..., # any aesthetic argument specified in the function
#       # will take precedence over the default arguments
#       # below, with suppressWarning to mute warnings on
#       # any duplicated aesthetics
#       alpha = 0.5, color = "steelblue", fill = "white")
#   )
# }



# Summaries of Monte Carlo samples
#
# x: data.frame with MC results: data, neigh_tol, x_earliest, tps, fa, sr
# pts: sfc_POINTS with observed locations (where SR is assessed)
# int_prob: prbability level for MC intervals
# sr_summaries <- function(x, pts, int_prob) {
#   stack_sr <- stack(x$sr)
#
#   ## Temporal trends
#
#   temp_trends <-
#     purrr::pmap_dfr(
#       x %>% select(data, sr),
#       function(data, sr)
#         data %>%
#         mutate(
#           sr = raster::extract(sr, as(data, "Spatial"))
#         ) %>%
#         st_drop_geometry() %>%
#         select(date, sr),
#       .id = "replicate"
#     )
#
#   ## MC samples of global SR density
#   mc_global_density <-
#     raster::extract(stack_sr, as(pts, "Spatial")) %>%
#     data.frame() %>%
#     purrr::map(
#       ~data.frame(
#         stats::density(
#           .,
#           from = min(minValue(stack_sr)),
#           to = max(maxValue(stack_sr))
#         )[c("x", "y")]
#       )
#     ) %>%
#     bind_rows(.id = "replicate") %>%
#     mutate(replicate = as.numeric(gsub("^est_fasr\\.", "", replicate)))
#
#   ## Point-wise SR MC-density
#   point_density <-
#     raster::extract(stack_sr, as(pts, "Spatial")) %>%
#     t() %>%
#     data.frame() %>%
#     purrr::map(
#       ~data.frame(
#         stats::density(
#           .,
#           from = min(minValue(stack_sr)),
#           to = max(maxValue(stack_sr))
#         )[c("x", "y")]
#       )
#     ) %>%
#     bind_rows(.id = "location") %>%
#     mutate(location = as.numeric(gsub("^X", "", location)))
#
#   ## Mean global SR density
#   mean_sr_dens <-
#     mc_global_density %>%
#     group_by(x) %>%
#     summarise(y = mean(y))
#
#   ## MC samples of Mean global SR values
#   mc_avgs <- raster::extract(stack_sr, as(pts, "Spatial")) %>%
#     colMeans()
#
#   ## Estimate of global SR value
#   global_avg <- c(
#     mean = mean(mc_avgs), quantile(mc_avgs, probs = c(1 - c(1, -1) * int_prob) / 2 )
#   )
#
#   ## Map MC summaries
#   map_summaries <- brick(
#     # sr_est,
#     mean(stack_sr),
#     calc(stack_sr, fun = median),
#     calc(stack_sr, fun = sd)
#   ) %>%
#     setNames(c("avg", "mdn", "sd"))
#
#
#   ans <- list(
#     mc_global_density = mc_global_density,
#     temp_trends = temp_trends,
#     point_density = point_density,
#     mean_sr_dens = mean_sr_dens,
#     mc_avgs = mc_avgs,
#     global_avg = global_avg,
#     map_summaries = map_summaries
#   )
# }
