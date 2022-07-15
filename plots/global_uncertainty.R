library(data.table)
dt <- fread("https://storage.googleapis.com/vie_ia2030/ia2030_reference_results.csv")
names(dt)
col_names <- grep("draw", names(dt), value = T)
global_dt <- dt[order(year)][, lapply(.SD, sum), by = year, .SDcols = col_names]
mat <- as.matrix(global_dt[, col_names, with = F]) / 1e6
mean <- apply(mat, 1, mean)
lower <- apply(mat, 1, quantile, 0.025)
upper <- apply(mat, 1, quantile, 0.975)

plot(global_dt$year, mean, type = 'l', ylim = c(0, max(upper)),
    xlab = "Year of vaccination", ylab = "Future deaths averted (in millions)",
    main = "Global future deaths averted by year of vaccination")
lines(global_dt$year, lower, lty = 2)
lines(global_dt$year, upper, lty = 2)
dev.off()
