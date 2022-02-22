library(data.table)
age_pattern_dt <- fread("results/age_pattern.csv")
reference_dt <- fread("outputs/reference_results.csv")
ref_dt <- merge(
    reference_dt[, .(location_name, year, disease, vaccine, activity_type, age, deaths_averted)],
    d_v_at_table
)[, .(location_name, year, d_v_at_id, age, deaths_averted)]

cal_year_dt <- rbindlist(lapply(unique(ref_dt$d_v_at_id), function(id) {
    print(id)
    id_dt <- ref_dt[d_v_at_id == id]
    out_id_dt <- rbindlist(lapply(unique(id_dt$age), function(a) {
        age_pattern <- prop.table(
            age_pattern_dt[d_v_at_id == ifelse(id == 24, 16, id) & age >= a]$age_pattern
        )
        if(length(age_pattern) == 0) age_pattern <- 1
        id_a_dt <- id_dt[age == a]
        split_dt <- cbind(id_a_dt, outer(id_a_dt$deaths_averted, age_pattern))
        melt_dt <- melt(split_dt, 
            id.vars = c("location_name", "year", "d_v_at_id", "age", "deaths_averted")
        )
        melt_dt[, diff := as.integer(gsub("V", "", variable)) - 1]
        melt_dt[, c("age", "year") := .(age + diff, year + diff)]
        out_dt <- melt_dt[, .(location_name, year, d_v_at_id, age, value)]
        setnames(out_dt, "value", "deaths_averted")
        return(out_dt)
    }))
    return(out_id_dt)
}))
cal_year_vacc_dt <- merge(cal_year_dt, d_v_at_table[, .(d_v_at_id, vaccine)])[
    , .(deaths_averted = sum(deaths_averted)), 
    by = .(location_name, year, age, vaccine)
]

## Generate calendar year estimates
full_frame <- data.table(expand.grid(
    year = 2000:2030,
    age = 0:100,
    location_name = unique(loc_table$location_name),
    vaccine = unique(vaccine_table$vaccine)
))

out_dt <- merge(
    full_frame, cal_year_vacc_dt, all.x = T, 
    by = c("vaccine", "location_name", "year", "age")
)

out_dt[is.na(deaths_averted), deaths_averted := 0]

upload_dt <- out_dt[, .(location_name, year, age, vaccine, deaths_averted)]

write.csv(upload_dt, "calendar_year_deaths_averted.csv", row.names = F)

upload_object(upload_dt, "calendar_year_deaths_averted")

test_dt <- upload_dt[, .(total = sum(deaths_averted)), by = .(year)]
