#' Pull in relevant datasets from WPP 2019
#' @param datasets A character vector with names of valid datasets in the
#'                 `wpp2019` package
#' @return A data.table with each of the requested datasets bound together
#' @export
#' @examples prep_wpp_data(c("popM", "popF"))
prep_wpp_data <- function(
    datasets = c(
        "tfr", "popM", "popF", "mxF", "mxM", "migration", "sexRatio",
        "percentASFR"
    )
) {
    data(list = datasets, package = "wpp2019")
    data_list <- lapply(datasets, function(d) {
        df <- get(d)
        df$measure <- d
        # Set up age_start and age_end variables
        # Note:
        #   - age_end is inclusive (age_end == 4 means 4 is included)
        #   - NA for terminal age_end
        if ("age" %in% names(df)) {
            if (any(grepl("-", df$age))) {
                df$age[df$age == "100+"] <- "100"
                df$age_start <- as.integer(
                    data.table::tstrsplit(df$age, "-")[[1]]
                )
                df$age_end <- as.integer(
                    data.table::tstrsplit(df$age, "-")[[2]]
                )
            } else {
                df$age_start <- df$age
                df$age_end <- df$age + 4
                df$age_end[df$age == 0] <- 0
                df$age_end[df$age == 1] <- 4
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
            df <- tidyr::pivot_longer(
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
            df <- tidyr::pivot_longer(
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
    dt <- data.table::rbindlist(data_list, fill = T)

    return(dt)
}
