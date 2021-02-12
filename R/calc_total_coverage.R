#' Calculate total coverage for every vaccine, location, sex
total_coverage <- function(coverage) {
    cov_dt <- merge(coverage, v_at_table)
    total_coverage <- rbindlist(lapply(unique(cov_dt$vaccine), function(v) {
        v_dt <- cov_dt[vaccine == v]
        vacc_dt <- rbindlist(lapply(unique(v_dt$activity_type), function(a) {
            a_dt <- v_dt[activity_type == a]
            act_dt <- rbindlist(lapply(unique(a_dt$location_id), function(l) {
                l_dt <- v_dt[location_id == l]
                loc_dt <- rbindlist(lapply(unique(l_dt$sex_id), function(s) {
                    dt <- l_dt[sex_id == s]
                    total_dt <- calc_total_cov(dt)
                    total_dt[, sex_id := s]
                }))
                loc_dt[, location_id := l]
                return(loc_dt)
            }))
            act_dt[, activity_type := a]
            return(act_dt)
        }))
        vacc_dt <- vacc_dt[, vaccine := v]
        # Set a cap on BCG effect at age 15
        if (v == "BCG") {
            vacc_dt[age >= 15, value := 0]
        }
        return(vacc_dt[])
    }))
    return(total_coverage[])
}

## Pull in various vaccine coverage data from the WHO website
calc_total_cov <- function(dt) {
    ages <- 0:95
    years <- 2000:2039
    activity_types <- unique(dt$activity_type)
    full_dt <- CJ(age = ages, year = years, activity_type = activity_types)
    merge_dt <- merge(
        full_dt,
        dt[, .(activity_type, age, year, coverage)],
        by = c("activity_type", "age", "year"), all.x = T
    )
    merge_dt[is.na(coverage), coverage := 0]
    nrow <- length(ages)
    ncol <- length(years)
    mat <- matrix(0, nrow, ncol)
    rownames(mat) <- ages
    colnames(mat) <- years
    if ("routine" %in% activity_types){
        r_mat <- as.matrix(
            dcast(
                merge_dt[activity_type == "routine"],
                age ~ year, value.var = "coverage"
            )
        )[, -1, drop = F]
        # Routine -- take the max
        for (i in (ncol - 1):1) {
            vec <- r_mat[, i]
            for (j in (i + 1):min(nrow, ncol)) {
                mat[max(1, (j - i + 1)):nrow, j] <- pmax(
                    mat[max(1, (j - i + 1)):nrow, j],
                    vec[1:min(nrow, (nrow - j + i))]
                )
            }
        }
    }
    if ("campaign" %in% activity_types) {
        c_mat <- as.matrix(
            dcast(
                merge_dt[activity_type == "campaign"],
                age ~ year, value.var = "coverage"
            )
        )[, -1, drop = F]
        # Campaign - Assume independence
        for (i in (ncol - 1):1) {
            vec <- c_mat[, i]
            for (j in (i + 1):min(nrow, ncol)) {
                mat[max(1, (j - i + 1)):nrow, j] <- 1 -
                    (1 - mat[max(1, (j - i + 1)):nrow, j]) *
                    (1 - vec[1:min(nrow, (nrow - j + i))])
            }
        }
    }
    total_dt <- melt(
        as.data.table(cbind(age = 0:95, mat)),
        id.vars = "age", variable.name = "year"
    )
    total_dt[, year := as.integer(as.character(year))]

    return(total_dt[])
}