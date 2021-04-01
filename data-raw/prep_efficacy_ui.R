obj <- function(par, data) {
    a = exp(par[1])
    b = exp(par[2])

    mu = data[1]
    l = data[2]
    u = data[3]
    
    mean_diff = abs(a / (a + b) - mu)
    l_diff = abs(qbeta(0.05, a, b) - l)
    u_diff = abs(qbeta(0.95, a, b) - u)
    return(sum(c(20 * mean_diff, l_diff, u_diff)))
}
efficacy_ui <- data.table()
pdf("plots/efficacy_dists.pdf")
for(i in 1:4) {
    d <- unlist(efficacy[i, .(mean, lower, upper)])
    opt_pars <- exp(
        optim(par = c(1, 1), lower = log(1), upper = log(10),
        obj, data = d, method = "L-BFGS-B")$par
    )

    add_dt <- data.table(
        disease = efficacy[i]$disease,
        draw = paste0("draw_", 1:200),
        scalar = rbeta(200, opt_pars[1], opt_pars[2]) / (opt_pars[1] / (opt_pars[1] + opt_pars[2]))
    )
    efficacy_ui <- rbind(efficacy_ui, add_dt)

    curve(dbeta(x, opt_pars[1], opt_pars[2]), 0, 1, main = efficacy[i,]$disease)
    abline(v = d[1], col = "green")
    abline(v = opt_pars[1] / (opt_pars[1] + opt_pars[2]), col = "green", lty = "dashed")
    abline(v = d[2], col = "red")
    abline(v = qbeta(0.05, opt_pars[1], opt_pars[2]), col = "red", lty = "dashed")
    abline(v = d[3], col = "blue")
    abline(v = qbeta(0.95, opt_pars[1], opt_pars[2]), col = "blue", lty = "dashed")
}
dev.off()

usethis::use_data(efficacy_ui, overwrite = TRUE)