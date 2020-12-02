efficacy <- fread("data-raw/efficacy.csv")
efficacy <- merge(
    efficacy[, .(vaccine_short, mean, lower, upper)],
    vaccine_table
)
usethis::use_data(efficacy, overwrite = TRUE)