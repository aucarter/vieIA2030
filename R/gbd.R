prep_gbd_data <- function() {
    # Merge covariates and coverage onto GBD mortality estimates
    message("Merging on covariates and coverage...")
    dt <- gbd_estimates %>%
        rename(mort_rate = value) %>%
        left_join(gbd_cov, by = c("location_id",  "year")) %>%
        left_join(
            vaccine_table[, .(vaccine_id, vaccine_short)],
            by = "vaccine_id"
        ) %>%
        left_join(
            coverage_inputs[, .(location_id, year, vaccine_id, value)],
            by = c("location_id", "year", "vaccine_id")
        ) %>%
        rename(coverage = value)
    return(dt)
}


forecast_gbd_cov <- function(plot = F) {
    fcast_list <- list()
    if (plot) {
        par(mfrow = c(2, 1))
    }
    for (cov in c("haqi", "sdi")) {
        dt <- gbd_cov[, c("location_id", "year", cov), with = F]
        cast_dt <- dcast(dt, location_id ~ year, value.var = cov)
        mat <- as.matrix(cast_dt[, -1])
        start_vec <-  mat[, dim(mat)[2] - 5]
        end_vec <-  mat[, dim(mat)[2]]
        aroc_5 <- log(end_vec / start_vec) / 5
        t_mat <- matrix(1:11, byrow = T, ncol = 11, nrow = length(end_vec))
        pred_mat <- end_vec * exp(aroc_5 * t_mat)
        colnames(pred_mat) <- 2020:2030
        if (max(pred_mat >= 1)) {
            warning("Forecast greater than or equal to 1. Consider forecasting 
                     in logit space")
        }
        out_mat <- cbind(mat, pred_mat)
        if (plot) {
            matplot(1990:2030, t(out_mat), type = "l", ylim = c(0, 1),
                    xlab = "Year", ylab = cov)
            abline(h = 1, col = "red")
            abline(v = 2019.5, col = "black")
        }
        melt_dt <- melt(
            data.table(location_id = cast_dt$location_id, out_mat),
            id.var = "location_id",
            variable.name = "year",
            value.name = cov
        )
        melt_dt[, year := as.integer(as.character(year))]
        fcast_list[[cov]] <- melt_dt
    }
    out_dt <- merge(fcast_list[[1]], fcast_list[[2]])

    return(out_dt)
}
