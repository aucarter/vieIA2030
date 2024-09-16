pop_21[, source := "WPP21"]
pop_23[, source := "WPP23"]
pop <- rbind(pop_21, pop_23)
cast_pop <- dcast(pop[age == 0 & sex_id == 3 & year == 2019], location_iso3 ~ source, value.var = "pop")

gg <- ggplot(cast_pop, aes(x = WPP21, y = WPP23, label = location_iso3)) + 
    geom_point() +
    coord_fixed() + theme_bw() + geom_abline()
gg
