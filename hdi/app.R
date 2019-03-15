#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(shinyWidgets)

source("hdi.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Sidebar with a slider input for number of bins 
  titlePanel("Simple Analysis of HDI"),
  
   # Application title
  tabsetPanel(tabPanel("Health",
                       sidebarLayout(
                         position = "right",
                                     sidebarPanel(
                                       fluidRow(
                                         h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose quiry options"),
                                         column(width=5,
                                                selectInput(
                                                  inputId =  "date_from", 
                                                  label = "Select start year:", 
                                                  choices = 1990:2017
                                                )),
                                         
                                         column(width=5,offset = 2,
                                                selectInput(
                                                  inputId =  "date_to", 
                                                  label = "Select end year:", 
                                                  choices = 1990:2017
                                                )
                                         )#column
                                       ),# fluidRow
                                       hr(),
                                       
                                       # fluidRow(column(12,selectInput(inputId = "heal.index",
                                       #                               "Select index",
                                       #                               choices =c("Current health expenditure (% of GDP)",
                                       #                                          "HIV prevalence, adult (% ages 15-49)",
                                       #                                          "Life expectancy at birth (years)",
                                       #                                          "Mortality rate, infant (per 1,000 live births)",
                                       #                                          "Mortality rate, under-five (per 1,000 live births)"),
                                       #                               width = 300))),
                                       # 
                                       # hr(),
                                       
                                       fluidRow(
                                         column(6, wellPanel(
                                           radioButtons("heal.choice", "Inquiry by",
                                                        choices = c(
                                                                    geography = "heal.geography",
                                                                    levels = "heal.levels"),
                                                        selected = NA)
                                         ))),
                                       hr(),
                                       
                                       fluidRow(column(9, 
                                                       # This outputs the dynamic UI component
                                                       uiOutput("heal.ui")
                                       ))
                                       
                                     ),#sidebarPanel
                                     mainPanel(plotOutput('heal.overview')))),
              
              
              tabPanel("Education",
                       sidebarLayout(position = "right",
                                     sidebarPanel("sidebar panel"),
                                     mainPanel("main panel"))),
              
              tabPanel("GDI",
                       sidebarLayout(position = "right",
                                     sidebarPanel("sidebar panel"),
                                     mainPanel("main panel")))
   )#tabsetPanel
)#fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$heal.ui <- renderUI({
    if (is.null(input$heal.choice))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$heal.choice,
           "heal.geography" = fluidRow(
                       h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose inquiry geography"),
                       column(8,
                       pickerInput('heal.geography.in', 'Options', choices = list(Region = unique(hdi.databank.m$Region),Country = unique(hdi.databank.m$country_name)), multiple=TRUE, options = list(`max-options` = 4,size=10))
                       )
                      ),
           "heal.levels" =  fluidRow(
                      h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose inquiry levels"),
                      column(8,
                      pickerInput('heal.level.in', 'Options', unique(hdi.databank.m$level), multiple=TRUE, options = list(`max-options` = 4))
                       )
                      )
           
    )
  })
  
  
  output$heal.overview <- renderPlot({
    if (is.null(input$heal.choice)|(is.null(input$heal.geography.in)&is.null(input$heal.level.in)))
      return()
    
    heal_radar_fun(heal.level.in = input$heal.level.in,
                   heal.geography.in = input$heal.geography.in,
                   date_from = input$date_from,
                   date_to = input$date_to)
    
  })
  
  
}#server


# Run the application 
shinyApp(ui = ui, server = server)