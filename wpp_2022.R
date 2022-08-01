library(data.table)
path <- "~/Downloads/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.csv"
dt <- fread(path)
dt[, .(ISO3_code, Location, Time, AgeGrp, PopMale, PopFemale)]
