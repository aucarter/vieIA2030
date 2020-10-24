## Pull demographic data from WPP website and save
WPP_ROOT = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES"
wpp_data_links <- list(
    pop_past = file.path(WPP_ROOT, "WPP2019_PopulationBySingleAgeSex_1950-2019.csv"),
    pop_future = file.path(WPP_ROOT, "WPP2019_PopulationBySingleAgeSex_2020-2100.csv"),
    fertility = file.path(WPP_ROOT, "WPP2019_Fertility_by_Age.csv"),
    # period_indicators = file.path(WPP_ROOT, "WPP2019_Period_Indicators_Medium.csv"),
    life_table = file.path(WPP_ROOT, "WPP2019_Life_Table_Medium.csv")
)
dt_list <- lapply(wpp_data_links, fread)

# Pull in location table to sub in IDs
un_locs <- fread("data-raw/un_locs.csv")
un_locs[country_name == "Democratic People's Republic of Korea",
        country_name := "Dem. People's Republic of Korea"]
un_locs[country_name == "Micronesia (Federated States of)",
        country_name := "Micronesia (Fed. States of)"]
setnames(un_locs, "country_name", "Location")
un_locs[, country_iso3 := NULL]

# Prep population
pop <- rbind(dt_list[["pop_past"]], dt_list[["pop_future"]])
pop[, c("LocID", "VarID", "Variant", "MidPeriod", "AgeGrp") := NULL]
pop <- data.table::melt.data.table(
    pop,
    id.vars = c("Location", "Time", "AgeGrpStart", "AgeGrpSpan")
)
pop <- merge(
    pop,
    data.table(
        variable = c("PopMale", "PopFemale", "PopTotal"),
        sex_id = 1:3
    )
)
pop[, variable := NULL]
pop <- merge(pop, un_locs)
pop[, Location := NULL]

# Prep fertility
fert <- dt_list[["fertility"]]
fert[, c("LocID", "VarID", "Variant", "MidPeriod", "AgeGrp") := NULL]
fert <- data.table::melt.data.table(
    fert,
    id.vars = c("Location", "Time", "AgeGrpStart", "AgeGrpSpan")
)
fert <- merge(fert, un_locs)
fert[, Location := NULL]

# Prep mx, px, and ax from life tables
lt <- dt_list[["life_table"]]
lt[, c("LocID", "VarID", "Variant", "MidPeriod", "Sex", "AgeGrp") := NULL]
lt <- data.table::melt.data.table(
    lt,
    id.vars = c("Location", "Time", "AgeGrpStart", "AgeGrpSpan", "SexID")
)
lt <- merge(lt, un_locs)
lt[, Location := NULL]

mydb <- dbConnect(RSQLite::SQLite(), "vieIA2030.db")

DBI::dbWriteTable(mydb, "population_inputs", pop, overwrite = TRUE)
DBI::dbWriteTable(mydb, "fertility_inputs", fert, overwrite = TRUE)
DBI::dbWriteTable(mydb, "life_table_inputs", lt, overwrite = TRUE)
dbDisconnect(mydb)