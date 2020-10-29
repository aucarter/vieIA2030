loc_table <- data.table::fread("data-raw/un_locs.csv")
remove_locs <- c("Holy See", "Liechtenstein", "Palestine")
add_locs <- c("Cook Islands", "Niue")
add_iso3 <- c("COK", "NIU")
add_table <- data.table::data.table(
    country_name = add_locs,
    country_iso3 = add_iso3
)
loc_table[, country_id := NULL]
loc_table <- rbind(
    loc_table[!country_name %in% remove_locs],
    add_table
)
loc_table <- loc_table[order(country_name)]
loc_table[, country_id := .I]

usethis::use_data(loc_table, overwrite = TRUE)