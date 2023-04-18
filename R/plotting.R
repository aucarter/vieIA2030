###########################################################
# PLOTTING
#
# xxxxx
#
###########################################################

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
    load_tables("wpp_input", "obs_wpp")
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

# ---------------------------------------------------------
# xxxxxxx
# ---------------------------------------------------------
plot_strata_fit <- function(rr_dt) {
    
    # Prepare datatable for plotting
    plot_dt = rr_dt %>%
        rename(truth   = strata_deaths_averted, 
               predict = averted) %>%
        filter(truth   > 0, 
               predict > 0) %>%
        left_join(d_v_at_table, by = "d_v_at_id") %>%
        mutate(strata = paste0(vaccine, ": ", activity_type))
    
    # Maximum value in each strata (truth or predict)
    blank_dt = plot_dt %>%
        mutate(max_value = ceiling(pmax(truth, predict))) %>%
        group_by(strata) %>%
        summarise(max_value = max(max_value)) %>%
        ungroup() %>%
        tidyr::expand_grid(type = c("truth", "predict")) %>%
        tidyr::pivot_wider(names_from  = type, 
                           values_from = max_value) %>%
        as.data.table()
    
    # Single plot with multiple facets
    #
    # NOTE: Removed the log10 scaling
    g = ggplot(plot_dt, aes(x = truth, y = predict)) +
        geom_point(aes(color = age), size = 0.5, alpha = 0.5) +
        geom_blank(data = blank_dt) +  # For square axes
        geom_abline(slope = 1) +  # To see quality of predict = truth
        facet_wrap(~strata, scales = "free")
    
    # Use a nice colour scheme
    g = g + viridis::scale_color_viridis(option = "viridis")
    
    # Prettify
    g = g + theme_bw() +
        xlab("Truth") + 
        ylab("Predicted")
    
    # Save figure to file
    fig_save(g, "strata_fit")
}

# ---------------------------------------------------------
# Save a ggplot figure to file with default settings
# ---------------------------------------------------------
fig_save = function(g, ..., path = "results", width = o$save_width, height = o$save_height) {
    
    # Collapse inputs into vector of strings
    fig_name_parts = unlist(list(...))
    
    # Construct file name to concatenate with file path
    save_name = paste(fig_name_parts, collapse = " - ")
    
    # Repeat the saving process for each image format in figure_format
    for (fig_format in o$figure_format) {
        save_pth  = paste0(o$pth[[path]], save_name, ".", fig_format)
        
        # Save figure (size specified in options.R)
        ggsave(save_pth, 
               plot   = g, 
               device = fig_format, 
               dpi    = o$save_resolution, 
               width  = width, 
               height = height, 
               units  = o$save_units)
    }
}

