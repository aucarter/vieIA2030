#' Make a showing presence or absence of an indicator
#' 
#' @param locations A character vector of iso3 codes for locations
#' @param title A string with the title of the plot
#' @returns A ggplot object with a world map
#' @examples 
#' dt <- prep_wuenic_data()
#' map_locations(
#'      unique(dt$ISO_code),
#'      "Availability of WUENIC Coverage Estimates"
#' )

map_locations <- function(locations, title) {
    theme_set(theme_bw())
    world <- ne_countries(
        scale = "medium",
        continent = c(
            "north america", "africa", "south america",
            "europe", "asia", "oceania"
        ),
        returnclass = "sf"
    )

    world$vimc <- ifelse(world$iso_a3 %in% locations, 1, NA)
    gg <- ggplot(data = world) +
        geom_sf(aes(fill = as.factor(vimc))) +
        ggtitle(
            title,
            subtitle = paste0("(", length(locations), " countries)")
        ) +
        theme(legend.position = "none") +
        scale_fill_discrete(na.value = "gray95")
    return(gg)
}