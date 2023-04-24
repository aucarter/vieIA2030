ia2030_dtp_goal <- fread("data-raw/ia2030_dtp_goal.csv")
ia2030_dtp_goal[, year := 2030]
ia2030_dtp_goal <- ia2030_dtp_goal[, .(country, year, value)]
usethis::use_data(ia2030_dtp_goal, overwrite = TRUE)
