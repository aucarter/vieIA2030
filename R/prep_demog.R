# Pull in relevant estimates from WPP 2019 and interpolate

prepWPPdata <- function(datasets = "mx") {
    data(list = datasets)
    data_list <- lapply(datasets, function(d) {
        df <- get(d)
        df$measure <- d
        if(typeof(df$age) == "character") {
            df$age <- as.integer(tstrsplit(df$age, "-")[[1]])
        }
        return(df)
    })
    dt <- bind_rows(data_list) %>%
        pivot_longer(
            contains("-"), 
            names_to = c("year_start", "year_end"),
            names_pattern = "(.*)-(.*)",
            names_transform = list(
                year_start = as.integer, 
                year_end = as.integer
            )
        )
    return(dt)
}

datasets <- c("tfr", "mxF", "mxM", "migration", "sexRatio", "percentASFR")
dt <- prepWPPdata(datasets)
