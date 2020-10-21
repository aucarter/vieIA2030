#' Make a showing presence or absence of an indicator
#' 
#' @param locations A character vector of iso3 codes for locations
#' @param title A string with the title of the plot
#' @returns A ggplot object with a world map
#' @examples 
#' dt <- prep_wuenic_data()
#' map_locations(
#'      unique(dt$country_iso3),
#'      "Availability of WUENIC Coverage Estimates"
#' )
#' @export
map_locations <- function(locations, title) {
    ggplot2::theme_set(ggplot2::theme_bw())
    world <- rnaturalearth::ne_countries(
        scale = "medium",
        continent = c(
            "north america", "africa", "south america",
            "europe", "asia", "oceania"
        ),
        returnclass = "sf"
    )

    world$present <- ifelse(world$iso_a3 %in% locations, 1, NA)
    gg <- ggplot2::ggplot(data = world) +
        ggplot2::geom_sf(ggplot2::aes(fill = as.factor(present))) +
        ggplot2::ggtitle(
            title,
            subtitle = paste0("(", length(locations), " countries)")
        ) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_fill_discrete(na.value = "gray95")
    return(gg)
}