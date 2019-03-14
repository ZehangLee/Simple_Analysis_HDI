#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#source("hdi.R")

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
                                         h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose inquiry period"),
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
                                       
                                       fluidRow(
                                         column(6, wellPanel(
                                           radioButtons("heal.choice", "Inquiry by",
                                                        choices = c(countries = "heal.countries",
                                                                    regions = "heal.regions",
                                                                    levels = "heal.levels"),
                                                        selected = NA)
                                         ))),
                                       hr(),
                                       
                                       fluidRow(column(9, 
                                                       # This outputs the dynamic UI component
                                                       uiOutput("heal.ui")
                                       ))
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                      
                                      
                                      
                                       
                                     ),#sidebarPanel
                                     mainPanel("main panel"))),
              
              
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
           "heal.countries" = fluidRow(
                       h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose inquiry countries"),
                       column(8,
                       selectInput('countries.in', 'Options', unique(hdi.databank.m$country_name), multiple=TRUE, selectize=TRUE)
                        )
                       ),
           "heal.regions" = fluidRow(
                       h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose inquiry regions"),
                       column(8,
                       selectInput('region.in', 'Options', unique(hdi.databank.m$Region), multiple=TRUE, selectize=TRUE)
                       )
                      ),
           "heal.levels" =  fluidRow(
                      h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose inquiry levels"),
                      column(8,
                       selectInput('region.in', 'Options', unique(hdi.databank.m$level), multiple=TRUE, selectize=TRUE)
                       )
                      )
           
    )
  })
  
  
}#server


# Run the application 
shinyApp(ui = ui, server = server)

