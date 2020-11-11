launch_shiny <- function() {
    wpp_input <<- db_pull("wpp_input")
    obs_wpp <<- db_pull("obs_wpp")
    shiny::runApp(system.file("shiny", package = "vieIA2030"))
}