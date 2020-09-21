## Pull demographic data from WPP website and save
wpp_data_links <- list(
    pop_past = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_1950-2019.csv",
    pop_future = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_2020-2100.csv",
    fertility = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Fertility_by_Age.csv",
    period_indicators = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Period_Indicators_Medium.csv",
    life_table = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Life_Table_Medium.csv"
)
dt_list <- lapply(wpp_data_links, fread)
locs_list <- lapply(dt_list, split, by = "LocID")

lapply(names(locs_list[[1]]), function(loc) {
    assign(paste0("demog_", loc), lapply(locs_list, `[[`, loc))
    save(
        list = paste0("demog_", loc),
        file = paste0("data/demog_", loc, ".rdata")
    )
})
