#' Plot coverage for a specific location
#' @param dt A data.table with coverage data
#' @return A plot of the coverage data
#' @method plot coverage
#' @export
plot.coverage <- function(x, ...) {
    gg <- ggplot2::ggplot(
            x,
            ggplot2::aes(
                x = year,
                y = value,
                color = vaccine_short,
                linetype = as.factor(sex_id)
            )
        ) +
        ggplot2::geom_line()

    return(gg)
}

## Scatter against covariates
scatter_rr <- function(dt, x_var) {
    for (a in 0:9) {
        gg <- ggplot(dt[age == a], aes(x = get(x_var), y = rr)) +
            geom_point(size = 0.1, alpha = 0.2) +
            facet_wrap(~vaccine_short, scales = "free_y") +
            theme_bw() +
            ggtitle(paste(x_var, "vs mortality reduction by vaccine: Age", a)) +
            xlab(x_var)
        print(gg)
    }
}