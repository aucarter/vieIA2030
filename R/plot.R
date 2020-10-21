#' Plot coverage for a specific location
#' 
#' @param dt A data.table with coverage data
#' @return A plot of the coverage data
#' @export
plot.coverage <- function(dt) {
    gg <- ggplot2::ggplot(dt, ggplot2::aes(x = year, y = value, color = vaccine_short)) +
          ggplot2::geom_line()

    return(gg)
}