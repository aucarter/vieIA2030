#' Plot coverage for a specific location
#' 
#' @param dt A data.table with coverage data
#' @return A plot of the coverage data
#' @method plot coverage
#' @export
plot.coverage <- function(x, ...) {
    gg <- ggplot2::ggplot(
            x,
            ggplot2::aes(
                x = year, y = value, color = vaccine_short, linetype = sex_id
            )
        ) +
        ggplot2::geom_line()

    return(gg)
}