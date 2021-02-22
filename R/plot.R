#' Plot coverage for a specific location
#' @param dt A data.table with coverage data
#' @return A plot of the coverage data
#' @method plot coverage
#' @export
plot.coverage <- function(x, ...) {
    gg <- ggplot2::ggplot(
            x,
            ggplot2::aes(
                x = year,
                y = value,
                color = vaccine_short,
                linetype = as.factor(sex_id)
            )
        ) +
        ggplot2::geom_line()

    return(gg)
}

## Scatter against covariates
scatter_rr <- function(dt, x_var) {
    for (a in 0:9) {
        gg <- ggplot(dt[age == a], aes(x = get(x_var), y = rr)) +
            geom_point(size = 0.1, alpha = 0.2) +
            facet_wrap(~vaccine_short, scales = "free_y") +
            theme_bw() +
            ggtitle(paste(x_var, "vs mortality reduction by vaccine: Age", a)) +
            xlab(x_var)
        print(gg)
    }
}

launch_shiny <- function() {
    wpp_input <<- db_pull("wpp_input")
    obs_wpp <<- db_pull("obs_wpp")
    shiny::runApp(system.file("shiny", package = "vieIA2030"))
}

#' Make a map showing presence or absence of an indicator
#' 
#' @param locations A character vector of iso3 codes for locations
#' @param title A string with the title of the plot
#' @returns A ggplot object with a world map
#' @examples 
#' map_locations(loc_table$location_iso3, "All locations")
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

plot_age_year <- function(dt, log_transform = F, value_name = "") {
    dt <- unique(dt[, .(year, age, value)])
    
    gg <- ggplot(dt, aes(x = year, y = age, fill = value)) + 
        geom_tile() +
        xlab("Year") + ylab("Age") + labs(fill = value_name) +
        viridis::scale_fill_viridis(
            option = "viridis", 
            direction = -1, 
            trans = ifelse(log_transform, "log10", "identity")) + 
        theme_minimal() +
        coord_fixed() +
        theme(text = element_text(size = 20))
    print(gg)
}

plot_strata_fit <- function(pred_all) {
    pdf("plots/strata_fit.pdf")
    for (s in unique(pred_all[!(d_v_at_id %in% 20:23)]$d_v_at_id)) {
        print(s)
        plot_dt <- pred_all[d_v_at_id == s & strata_deaths_averted > 0 & averted > 0]
        min_val <- min(c(plot_dt$strata_deaths_averted, plot_dt$averted))
        s_title <- paste(unlist(d_v_at_table[d_v_at_id  == s, .(vaccine, activity_type)]), collapse = " ")
        gg <- ggplot(plot_dt, aes(x = strata_deaths_averted, y = averted, color = age + 1)) +
            geom_point(size = 0.2, alpha = 0.5) +
                    viridis::scale_color_viridis(
                    option = "viridis",
                    direction = -1,
                    trans = "log10") +
            geom_abline(slope = 1) + expand_limits(x = min_val, y = min_val) +
            scale_x_continuous(trans='log10') +
            scale_y_continuous(trans='log10') +
            coord_fixed() + ggtitle(s_title) + theme_bw() +
            xlab("Observed") + ylab("Predicted")
        print(gg)
    }
    dev.off()
}