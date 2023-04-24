# Prep GBD 2019 SDI for use as a covariate

## Read data
dt <- fread(
    system.file("extdata", "gbd19_sdi.csv", package = "vieIA2030"),
    header = T
)

## Get rid of Georgia the state
dt <- dt[-which(dt$Location == "Georgia")[2]]

## Make long
melt_dt <- melt(
    dt, id.vars = "Location", variable.name = "year"
)
melt_dt[, year := as.integer(as.character(year))]
melt_dt[, value := gsub("·", ".", value)]
melt_dt[, sdi := as.numeric(value)]
melt_dt[, value := NULL]
melt_dt <- unique(melt_dt)

## Location mapping
setnames(melt_dt, "Location", "gbd_alt_name")
gbd_sdi <- merge(country_table, melt_dt, all.x = T, by = "gbd_alt_name")

# GBD 2019 HAQI for use as a covariate

## Read data
dt <- fread(
    system.file("extdata", "gbd19_haqi.csv", package = "vieIA2030"),
    header = T
)

## Get rid of Georgia the state
g_idx <- which(dt$location_name == "Georgia")
state_idx <- g_idx[(length(g_idx) / 2 + 1):length(g_idx)]
dt <- dt[-state_idx]
setnames(dt, "location_name", "country_name")


## Location mapping
gbd_haqi <- merge(country_table, dt, all.x = T, by = "country_name")
setnames(gbd_haqi, c("year_id", "val"), c("year", "haqi"))

## Ignore uncertainty (for now)
gbd_haqi[, c("upper", "lower") := NULL]
gbd_haqi[, haqi := haqi / 100]

gbd_cov <- merge(
    gbd_haqi[, .(country, year, haqi)],
    gbd_sdi[, .(country, year, sdi)],
    by = c("country", "year")
)

forecast_gbd_cov <- function(gbd_cov, years_back = 5, plot = F) {
    fcast_list <- list()
    max_year <- max(gbd_cov$year)
    if (plot) {
        par(mfrow = c(2, 1))
    }
    for (cov in c("haqi", "sdi")) {
        dt <- gbd_cov[, c("country", "year", cov), with = F]
        cast_dt <- dcast(dt, country ~ year, value.var = cov)
        mat <- as.matrix(cast_dt[, -1])
        start_vec <-  1 - mat[, dim(mat)[2] - years_back]
        end_vec <-  1 - mat[, dim(mat)[2]]
        aroc <- log(end_vec / start_vec) / years_back
        t_mat <- matrix(
            1:(2100 - max_year), byrow = T, ncol = (2100 - max_year), 
            nrow = length(end_vec)
        )
        pred_mat <- 1 - end_vec * exp(aroc * t_mat)
        colnames(pred_mat) <- (max_year + 1):2100
        if (max(pred_mat >= 1)) {
            warning("Forecast greater than or equal to 1. Consider forecasting 
                     in logit space")
        }
        out_mat <- cbind(mat, pred_mat)
        if (plot) {
            matplot(1990:2100, t(out_mat), type = "l", ylim = c(0, 1),
                    xlab = "Year", ylab = cov)
            abline(h = 1, col = "red")
            abline(v = (max_year + 0.5), col = "black")
        }
        melt_dt <- melt(
            data.table(country = cast_dt$country, out_mat),
            id.var = "country",
            variable.name = "year",
            value.name = cov
        )
        melt_dt[, year := as.integer(as.character(year))]
        fcast_list[[cov]] <- melt_dt
    }
    out_dt <- merge(fcast_list[[1]], fcast_list[[2]])

    return(out_dt)
}

gbd_cov <- forecast_gbd_cov(gbd_cov, years_back = 5, plot = T)

usethis::use_data(gbd_cov, overwrite = TRUE)

