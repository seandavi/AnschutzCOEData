#' map of FIT and CCSP activity
#'
#' @examples
#'
#' map_fit_and_ccsp_activity()
#'
#' @export
map_fit_and_ccsp_activity <- function() {
  fit_and_ccsp_activity <- fit_ccsp_by_county()
  fit_and_ccsp_activity$county <- tolower(fit_and_ccsp_activity$county)

  # Load Colorado counties shapefile from the US Census Bureau (or another source)
  # Here we use the tigris package to get county boundaries
  options(tigris_use_cache = TRUE)
  co_counties <- suppressMessages(
    tigris::counties(state = "CO", class = "sf")
  )

  # Ensure consistent naming
  co_counties$county <- tolower(co_counties$NAME)

  ######
  ## Cross-hatch
  ######

  co_map <- co_counties |>
    dplyr::left_join(fit_and_ccsp_activity, by = "county")

  co_map <- co_map |>
    dplyr::mutate(
      CCSP_label = ifelse(CCSP, "Yes", "No"),
      FIT_label = ifelse(FIT, "Yes", "No"),
      fill_color = ifelse(CCSP, "lightblue", "white"),
      pattern_type = ifelse(FIT, "stripe", "none")
    )


  # Plot with OSM as the base layer
  ggplot2::ggplot(co_map) +
    ggpattern::geom_sf_pattern(data = co_map, ggplot2::aes(fill = CCSP_label, pattern = FIT_label),
                    inherit.aes = FALSE, color = "black", size = 0.3,
                    pattern_fill = "black", pattern_density = 0.1,
                    pattern_spacing = 0.01, pattern_size = 0.2,
                    pattern_angle = 45, alpha = 0.7) +
    ggplot2::scale_fill_manual(name = "CCSP",
                      values = c("Yes" = "lightblue", "No" = "white"),
                      labels = c("Yes" = "Yes", "No" = "No"),
                      na.value = "white",
                      guide = ggplot2::guide_legend(override.aes = list(pattern = "none"))) +
    ggpattern::scale_pattern_manual(name = "FIT",
                         values = c("Yes" = "stripe", "No" = "none"),
                         labels = c("Yes" = "Yes", "No" = "No"),
                         guide = ggplot2::guide_legend(override.aes = list(fill = "white"))) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Colorado Counties - FIT (Diagonal Stripes) & CCSP (Light Blue)")
}
