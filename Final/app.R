library(shiny)
data <- read.csv("data.csv")
ui <- fluidPage(
  titlePanel("US Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "name",
                  label = "Select State",
                  choices = c("ALL", data$state),
                  selected = "ALL"),
      selectInput(inputId = "variable",
                  label = "Select Variable",
                  choices = c("mortality",
                              "pfizer_2nd_dose_allocations_rate",
                              "moderna_2nd_dose_allocations_rate"),
                  selected = "mortality")
    ),
    mainPanel(
      plotly::plotlyOutput(outputId = "USMAP_Plot")
    )
  )
)


server <- function(input, output) {
  output$USMAP_Plot <- plotly::renderPlotly({
    if (input$name == "ALL") {
      plotdata <- data[, c("state", "ID", input$variable)]
    } else {
      plotdata <- data[data$state == input$name, c("state", "ID", input$variable)]
    }
    colnames(plotdata)[3] <- "x"
    library(plotly)
    w <- list(color = toRGB("white"), width = 2)
    g <- list(scope = 'usa', projection = list(type = 'albers usa'),
              showlakes = TRUE, lakecolor = toRGB('white'))
    plot_geo(plotdata, locationmode = 'USA-states') %>%
      add_trace(z = ~x, locations = ~ID,
                color = ~x, colors = 'Purples') %>%
      colorbar(title = input$variable) %>%
      layout(title = input$variable, geo = g)
  })
}

shinyApp(ui = ui, server = server)
