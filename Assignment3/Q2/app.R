
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(sqldf)

df <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv", header= TRUE)

names(df) <- gsub('\\.', '_', names(df)) %>%
  tolower()

national <-sqldf("select ICD_Chapter 
                 , Year
                 , round(sum(Deaths)*100000.00 /sum(Population),2) as Crude_Rate
                 , 'National' as State
                  from  df    
                 Group by ICD_Chapter 
                 , Year")
names(national) <-tolower(names(national) )

state <- sqldf("select icd_chapter 
                 , year
                 , crude_rate
                 , state
                  from  df ")

national2 <- sqldf("select * from state
                   union all
                   select * from national")



ui <- fluidPage(
  headerPanel('State Mortality Rates By State and Cause'),
  sidebarPanel(
    selectInput('state', 'State', unique(national2$state), selected='NY'),
    selectInput('icd_chapter', 'Cause of Death', unique(national2$icd_chapter), selected='Certain infectious and parasitic diseases')
  ),
  mainPanel(
    plotlyOutput('plot1'),
    verbatimTextOutput('stats'),
    h6("Number of deaths per 100,000 people")
  )
)

server <- function(input, output, session) {
  
  nationalData <- reactive({
    national %>%
      filter(icd_chapter == input$icd_chapter)
  })
  
  statedata <- reactive({
    dfSlice <- national2 %>%
      filter(state == input$state, icd_chapter == input$icd_chapter)
  })
  
  combined <- reactive({
    merge(x = nationalData(), y = statedata(), all = TRUE)
  })
  
  output$plot1 <- renderPlotly({
    
    dfSlice <- national2 %>%
      filter(state == input$state, icd_chapter == input$icd_chapter)
    
    plot_ly(combined(), x = ~year, y = ~crude_rate, color = ~state, type='scatter',
            mode = 'lines')
  })
  
  output$stats <- renderPrint({
    dfSliceTier <- statedata() %>%
      filter(state == input$state)
    
    summary(dfSliceTier$crude_rate)
  })
  
}

shinyApp(ui = ui, server = server)