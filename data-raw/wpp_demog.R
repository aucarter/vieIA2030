## Pull demographic data from WPP website and save
WPP_ROOT = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES"
wpp_data_links <- list(
    pop_past = file.path(WPP_ROOT, "WPP2019_PopulationBySingleAgeSex_1950-2019.csv"),
    pop_future = file.path(WPP_ROOT, "WPP2019_PopulationBySingleAgeSex_2020-2100.csv"),
    fertility = file.path(WPP_ROOT, "WPP2019_Fertility_by_Age.csv"),
    period_indicators = file.path(WPP_ROOT, "WPP2019_Period_Indicators_Medium.csv"),
    life_table = file.path(WPP_ROOT, "WPP2019_Life_Table_Medium.csv")
)
dt_list <- lapply(wpp_data_links, fread)
locs_list <- lapply(dt_list, split, by = "Location")
un_locs <- fread("data/un_locs.csv")
locs <- names(locs_list[[1]])[names(locs_list[[1]]) %in% un_locs$country_name]
dir.create("data/demog", showWarnings = F)
lapply(locs, function(loc) {
    iso3 <- un_locs[country_name == loc]$country_iso3
    temp <- lapply(locs_list, `[[`, loc)
    temp <- lapply(temp, function(dt) {
        dt[, c("LocID", "VarID", "Variant", "MidPeriod", "Location") := NULL]
        if ("Sex" %in% names(dt)) {
            dt[, Sex := NULL]
        }
        if ("AgeGrp" %in% names(dt)) {
            dt[, AgeGrp := NULL]
        }
        return(dt)
    })
    assign(paste0("demog_", iso3), temp)
    save(
        list = paste0("demog_", iso3),
        file = paste0("data/demog/demog_", iso3, ".Rdata")
    )
})
