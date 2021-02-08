# Prep HPV target coverage for IA2030 coverage scenario

## Read data
dt <- fread(
    system.file("extdata", "hpv_target_coverage.csv", package = "vieIA2030"),
    header = T
)
dt[, c("Gavi Eligibility", "Routine/MACs") := NULL]
melt_dt <- melt(dt, id.vars = c("Country", "Age", "Gender", "No of Doses"),
                variable.name = "year")
melt_dt[, year := as.integer(as.character(year))]
melt_dt[is.na(value), value := 0]

## Keep the max of the one and two does coverage levels
max_dt <- melt_dt[, .(value = max(value)), by = .(Country, Age, Gender, year)]

## Merge on location
setnames(max_dt, c("Country", "Age"), c("hpv_name", "age"))
merge_dt <- merge(max_dt, 
    loc_table[, .(hpv_name, location_id)],
    by = "hpv_name",
    all.x = T)

## Merge on sex ID
merge_dt <- merge(merge_dt,
    data.table("Gender" = c("Male", "Female", "Both"), sex_id = 1:3),
    by = c("Gender"))

## Convert both sexes to male and female
both_dt <- merge_dt[sex_id == 3]
add_dt <- rbindlist(lapply(1:2, function(i) {
    dt <- copy(both_dt)
    dt[, sex_id := i]
}))
split_dt <- rbind(merge_dt[sex_id != 3], add_dt)

## Subset to before 2027
hpv_target <- split_dt[year <= 2026, .(location_id, year, sex_id, age, value)]

## Linearly interpolate to 90% in 2030
interp_dt <- rbindlist(lapply(1:4, function(i) {
    i_dt <- copy(hpv_target[year == 2026])
    i_dt[, year := year + i]
    i_dt[value < 0.9 & value > 0, value := value + (0.9 - value) / 4 * i]
}))
hpv_target <- rbind(hpv_target, interp_dt)
hpv_target[, vaccine_id := 2]

usethis::use_data(hpv_target, overwrite = TRUE)

