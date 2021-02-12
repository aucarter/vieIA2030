prep_params <- function() {
    params_list <- lapply(strata_table$strata_id, function(id) {
        list(
            vaccine = strata_table[strata_id == id]$vaccine,
            activity_type = strata_table[strata_id == id]$activity_type,
            alpha = 1, beta = 1, age_knots = c(2, 5, 10, 25)
        )
    })
    names(params_list) <- strata_table$strata_id
    jsonlite::write_json(params_list, "params.json", pretty = T)
}