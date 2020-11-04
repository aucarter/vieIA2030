##########################################################
## user interface
##########################################################

ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  shiny::titlePanel("IA2030 CCPM Model Check"),
  shiny::sidebarPanel(
    width = 3,
    shiny::wellPanel(
      shiny::selectInput(
        "loc", "Location:", choices = countries, selected = "Tanzania"
      )
    ),
    shiny::wellPanel(
      shiny::sliderInput(
        "range", "Period:", min = 1980, max = 2090, value = c(2000, 2030),
        step = 10
      )
    )
  ),
  shiny::mainPanel(
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Summary demography",
        shiny::fluidRow(
          shiny::column(width = 6, highcharter::highchartOutput("deathp")),
          shiny::column(width = 6, highcharter::highchartOutput("birthp")),
        ),
        shiny::fluidRow(
          shiny::column(width = 4, highcharter::highchartOutput("imrp")),
          shiny::column(width = 4, highcharter::highchartOutput("e0p")),
          shiny::column(width = 4, highcharter::highchartOutput("u5mrp"))
        ),
        shiny::fluidRow(
          shiny::column(width = 4, highcharter::highchartOutput("poppy0")),
          shiny::column(width = 4, highcharter::highchartOutput("poppy1")),
          shiny::column(width = 4, highcharter::highchartOutput("poppy2")))
)
    )
  )
)
