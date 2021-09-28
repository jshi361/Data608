library(dplyr)
library(shiny)
df <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv", header= TRUE)
df_chapter <- df%>%
   filter(Year == 2010)
  
df_chapters <- unique(df_chapter$ICD.Chapter)

shinyUI(fluidPage(
  headerPanel('State Mortality Rate by Cause'),
  sidebarPanel(
    selectInput('Cause', 'Cause of Death', df_chapters,
                selected='Certain infectious and parasitic diseases')
    
  ),
  mainPanel(
    htmlOutput(outputId = 'selection'),
    plotOutput('plot1', height="auto"),
    h6("Number of deaths by State for the selected cause of death.")
  )
))