## Deaths averted rough figure with placeholder data
library(data.table)
library(ggplot2)
set.seed(2)

out_path <- "/Users/aucarter/OneDrive - UW/Documents/who/requests/vie_deaths_averted.pdf"

gen_placeholder_data <- function(indicator_list) {
    dt <- data.table(
        indicator = factor(
            seq(indicator_list),
            levels = seq(indicator_list),
            labels = indicator_list
        ),
        r = runif(length(indicator_list), 0.1, 3) * 0.02
    )
    years <- 2020:2030
    dt <- rbindlist(lapply(years, function(y) {
        temp_dt <- copy(dt)
        temp_dt[, year := y]
        temp_dt[, t := y - min(years)]
    }))
    A <- 7e6
    dt[, value := A * exp(r * t) - A + runif(1, 0, 1e4)]
    return(dt[])
}

plot_healthy_ly <- function(dt) {
    options(scipen=1e8)
    gg <- ggplot(dt, aes(x = year, y = value, fill = indicator)) + geom_area() +
        theme_classic() +
        ylab("Deaths averted") + 
        theme(strip.text.x = element_blank(),
              strip.background = element_rect(colour="white", fill="white"),
              legend.position=c(.15,.7),
              legend.title = element_blank()
        ) +
        scale_x_continuous(
            name = "Year", 
            breaks = 2020:2030, 
            labels = as.character(2020:2030),
            expand = expansion(mult = c(0, 0.02))
        ) +
        scale_y_continuous(expand = expansion(0)) +
        scale_fill_brewer(palette = "Set3")
    return(gg)
}

indicator_list <- c(
    "Measles", "Diptheria", "HepB", "Yellow fever", "Tetanus", 
    "Pneumococcal diseases", "Hib", "Pertussis", "Rotavirus", 
    "Meningococcal meningitis", "Other VPD"
)
dt <- gen_placeholder_data(indicator_list)
pdf(out_path, width = 8, height = 5)
print(plot_healthy_ly(dt))
dev.off()