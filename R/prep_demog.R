#' Pull in relevant datasets from WPP 2019
#' @param datasets A character vector with names of valid datasets in the 
#'                 `wpp2019` package
#' @return A dataframe with each of the requested datasets bound together
#' @example
#' datasets <- c(
#'   "tfr", "popM", "popF", "mxF", "mxM", "migration", "sexRatio", "percentASFR"
#' )
#' dt <- prep_wpp_data(datasets)
prep_wpp_data <- function(datasets = "mx") {
    data(list = datasets)
    data_list <- lapply(datasets, function(d) {
        df <- get(d)
        df$measure <- d
        # Grab the first age if age group used
        if ("age" %in% names(df)) {
            if (any(grepl("-", df$age))) {
                df$age[df$age == "100+"] <- "100"
                df$age_start <- as.integer(tstrsplit(df$age, "-")[[1]])
                df$age_end <- as.integer(tstrsplit(df$age, "-")[[2]])
            } else {
                df$age_start <- df$age
                df$age_end <- df$age + 5
                df$age_end[df$age == 0] <- 1
                df$age_end[df$age == 1] <- 5
                df$age_end[df$age == 100] <- NA
            }
            df$age <- NULL
        }
        # Remove unneeded variables
        if ("last.observed" %in% names(df)) {
            df$last.observed <- NULL
        }
        # Melt the years long
        if (any(grepl("-", names(df)))) {
            # If year interval
            df <- pivot_longer(
                df,
                contains("-"),
                names_to = c("year_start", "year_end"),
                names_pattern = "(.*)-(.*)",
                names_transform = list(
                    year_start = as.integer,
                    year_end = as.integer
                )
            )
        } else {
            # If single year
            df <- pivot_longer(
                df,
                cols = as.character(seq(1950, 2020, 5)),
                names_to = "year_start",
                names_transform = list(
                    year_start = as.integer
                )
            )
        }

        return(df)
    })
    dt <- bind_rows(data_list)

    return(dt)
}
