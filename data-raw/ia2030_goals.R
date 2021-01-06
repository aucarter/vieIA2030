ia2030_dtp_goal <- fread("data-raw/ia2030_dtp_goal.csv")
ia2030_dtp_goal <- merge(ia2030_dtp_goal,
    loc_table[, location_iso3, location_id])
ia2030_dtp_goal[, c("location_iso3", "location_name")  := NULL]
ia2030_dtp_goal[, year := 2030]
ia2030_dtp_goal <- ia2030_dtp_goal[, .(location_id, year, value)]
usethis::use_data(ia2030_dtp_goal, overwrite = TRUE)
