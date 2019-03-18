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
                                     sidebarPanel(style = "position:fixed;width:inherit;",
                                       fluidRow(
                                         h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose quiry options"),
                                         column(width=5,
                                                selectInput(
                                                  inputId =  "date_from", 
                                                  label = "Select start year:", 
                                                  choices =c(Choose='',2000:2017) 
                                                )),
                                         
                                         column(width=5,offset = 2,
                                                selectInput(
                                                  inputId =  "date_to", 
                                                  label = "Select end year:", 
                                                  choices =c(Choose='',2000:2017)
                                                )
                                         )#column
                                       ),# fluidRow
                                       hr(),

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
                                     mainPanel(fluidRow(
                                       verticalLayout(plotOutput("heal.overview"),
                                                   plotOutput("heal.hiv.plot"),
                                                   plotOutput("heal.expend.plot"),
                                                   plotOutput("heal.life.plot"),
                                                   plotOutput("heal.mortal.plot")
                                                 
                                       )#verticalLayout
                                     )#fluidRow
                                    )#mainPanel

                         )#sidebarLayout
                       ),#tabPanel
              
              
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
    

    switch(input$heal.choice,
           "heal.geography" = fluidRow(
                       h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose inquiry geography"),
                       column(8,
                       pickerInput('heal.geography.in', 'Options', choices = list(Region = unique(hdi.databank.m$Region),Country = unique(hdi.databank.m$country_name)), multiple=TRUE, options = list(`max-options` = 4,size=10))
                       )#,
                       # column(3, actionButton('go',"GO"),offset = 2),
                       # column(3,actionButton("clean","Clean All"),offset = 2)
                      ),
           "heal.levels" =  fluidRow(
                      h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose inquiry levels"),
                      column(8,
                      pickerInput('heal.level.in', 'Options', unique(hdi.databank.m$level), multiple=TRUE, options = list(`max-options` = 4))
                       )#,
                      #column(3, actionButton('go',"GO"),offset = 2),
                      #column(3,actionButton("clean","Clean All"),offset = 2)
                      )
           
    )
    

  })
  

  

  
  # observeEvent(input$go,{
  #   datainput <- reactiveValues(heal.level.in=input$heal.level.in,
  #                               heal.geography.in=input$heal.geography.in,
  #                               date_from=input$date_from,
  #                               date_to=input$date_to)
  #   
  # })
  # 
  # observeEvent(input$clean,{
  #   datainput <-NULL
  #   
  # })
  # 
  output$heal.overview <- renderPlot({
    if (is.null(input$heal.choice)|(is.null(input$heal.geography.in)&is.null(input$heal.level.in)))
      return()
    heal_plot_fun(heal.level.in=input$heal.level.in,
                   heal.geography.in=input$heal.geography.in,
                   date_from=input$date_from,
                   date_to=input$date_to,
                   plot_type="radar")

  })
  
  output$heal.hiv.plot<- renderPlot({
    if (is.null(input$heal.choice)|(is.null(input$heal.geography.in)&is.null(input$heal.level.in)))
      return()
    heal_plot_fun(heal.level.in=input$heal.level.in,
                  heal.geography.in=input$heal.geography.in,
                  date_from=input$date_from,
                  date_to=input$date_to,
                  plot_type="hiv.plot")
    
  })
  
  output$heal.expend.plot<- renderPlot({
    if (is.null(input$heal.choice)|(is.null(input$heal.geography.in)&is.null(input$heal.level.in)))
      return()
    heal_plot_fun(heal.level.in=input$heal.level.in,
                  heal.geography.in=input$heal.geography.in,
                  date_from=input$date_from,
                  date_to=input$date_to,
                  plot_type="expend.plot")
    
  })
  
  output$heal.life.plot<- renderPlot({
    if (is.null(input$heal.choice)|(is.null(input$heal.geography.in)&is.null(input$heal.level.in)))
      return()
    heal_plot_fun(heal.level.in=input$heal.level.in,
                  heal.geography.in=input$heal.geography.in,
                  date_from=input$date_from,
                  date_to=input$date_to,
                  plot_type="life.plot")
    
  })
  
  output$heal.mortal.plot<- renderPlot({
    if (is.null(input$heal.choice)|(is.null(input$heal.geography.in)&is.null(input$heal.level.in)))
      return()
    heal_plot_fun(heal.level.in=input$heal.level.in,
                  heal.geography.in=input$heal.geography.in,
                  date_from=input$date_from,
                  date_to=input$date_to,
                  plot_type="mortal.plot")
    
  })
  
  
  
  
}#server


# Run the application 
shinyApp(ui = ui, server = server)