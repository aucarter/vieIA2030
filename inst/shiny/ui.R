##########################################################
## user interface
##########################################################
locsall <- loc_table %>%
  filter(location_iso3 %in% unique(wpp_input$location_iso3)) %>%
  select(location_iso3, location_name) %>%
  arrange(location_iso3)
countries <- locsall$location_name

ui <- fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  titlePanel("IA2030 CCPM Model Check"),
  sidebarPanel(
    width = 3,
    wellPanel(
      selectInput(
        "loc", "Location:", choices = countries, selected = "Tanzania"
      )
    ),
    wellPanel(
      sliderInput(
        "range", "Period:", min = 1980, max = 2090, value = c(2000, 2030),
        step = 10
      )
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Summary demography",
        fluidRow(
          column(width = 6, highchartOutput("deathp")),
          column(width = 6, highchartOutput("birthp")),
        ),
        fluidRow(
          column(width = 4, highchartOutput("imrp")),
          column(width = 4, highchartOutput("e0p")),
          column(width = 4, highchartOutput("u5mrp"))
        ),
        fluidRow(
          column(width = 4, highchartOutput("poppy0")),
          column(width = 4, highchartOutput("poppy1")),
          column(width = 4, highchartOutput("poppy2")))
)
    )
  )
)
