

library(ggplot2)
library(dplyr)
library(shiny)
library(rsconnect)

df_csv <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv", header= TRUE)
df <- df_csv


ui <- fluidPage(
  headerPanel('State Mortality Rate by Cause'),
  sidebarPanel(
    selectInput('Cause', 'Cause of Death', unique(df$ICD.Chapter),
                selected='Certain infectious and parasitic diseases')
    
  ),
  mainPanel(
    htmlOutput(outputId = 'selection'),
    plotOutput('plot1', height="auto"),
    h6("Number of deaths by State for the selected cause of death.")
  )
)

server <- shinyServer(function(input, output, session) {
  
  selectedData <- reactive({
    df %>% filter(ICD.Chapter == input$Cause & Year == 2010 )
  })
  
  output$selection <- renderText({
    paste('<b>Death rate for </b> <br>', input$Cause)
  })
  
  output$plot1 <- renderPlot({
    
    ggplot(selectedData(), aes(x=reorder(State, -Crude.Rate), y=Crude.Rate)) +
      geom_col(fill = "#6D97D1") +
      coord_flip() +
      geom_text(aes(label=Crude.Rate),
                size=3,
                hjust=-0.2,
                color="#2C7ECB") +xlab("State") +ylab("Rate") +
      theme(panel.background = element_blank())
  }, height = function() {
    session$clientData$output_plot1_width}
  )
})

shinyApp(ui = ui, server = server)
