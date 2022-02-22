library(shiny)
library(viridis)

vimc_dt <- readRDS("vimc.rds")
vimc_dt <- vimc_dt[activity_type == "routine"]
locs <- sort(unique(vimc_dt$location_iso3))
vaccines <- sort(unique(vimc_dt$vaccine))
# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("VIMC Estimates"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput("l", "Location:", locs, selected = "Uganda"),
            selectInput("v", "Vaccine:", vaccines, selected = "Measles"),
            checkboxInput("log_transform", "Log transform")
        ),

        # Show a plot
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({
        dt <- vimc_dt[location_iso3 == input$l & vaccine == input$v & deaths_averted > 0]
        gg <- ggplot(dt, aes(x = year, y = age, fill = deaths_averted)) + 
            geom_tile() +
            xlab("Year") + ylab("Age") + labs(fill = "Deaths averted") +
            scale_fill_viridis(
                option = "viridis", 
                direction = -1, 
                trans = ifelse(input$log_transform, "log10", "identity")) + 
            theme_minimal() +
            ggtitle(paste0(input$l, ": ", input$v)) +
            coord_fixed() +
            theme(text = element_text(size = 20))
        print(gg)
    }, height = 700)
}

# Run the application 
shinyApp(ui = ui, server = server)
