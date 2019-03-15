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
library(ggradar)
library(shinyWidgets)
library(ggplot2)
library(scales)
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
    if (is.null(input$heal.level.in)){
      heal.overview1 <- hdi.databank.m %>% 
        filter(Region %in% input$heal.geography.in) #%>%
      #   filter((year>= input$date_from) &(year<= date_to)) %>%
      #   na.omit(hdi)%>%
      #   group_by(Region,indicator_name)%>%summarise(avg = mean(hdi))
      # colnames(heal.overview1)=c("year","geography","indicator_name")
      
      heal.overview2 <- hdi.databank.m %>% 
        filter(country_name %in% input$heal.geography.in) %>%
        filter((year>= input$date_from) &(year<= input$date_to)) %>%
        na.omit(hdi)%>%
        group_by(country_name,indicator_name)%>%summarise(avg = mean(hdi))
      colnames(heal.overview2)=c("year","geography","indicator_name")
      
      heal.overview <- rbind(heal.overview1,heal.overview2)
      heal.overview %>%
        mutate_at(vars(avg),funs(rescale)) 
      
      heal.overview%>%spread(indicator_name,avg)->heal.overview.final
      
      ggradar(heal.overview.final,grid.max=max(heal.overview$avg)+0.01) 

    }else{
      heal.overview1 <- hdi.databank.m %>% 
        filter(level %in% input$heal.level.in) %>%
        filter((year>= input$date_from) &(year<= input$date_to)) %>%
        na.omit(hdi)%>%
        group_by(level,indicator_name)%>%summarise(avg = mean(hdi))
      
      heal.overview %>%
        mutate_at(vars(avg),funs(rescale)) 
      
      heal.overview%>%spread(indicator_name,avg)->heal.overview.final
      
      ggradar(heal.overview.final,grid.max=max(heal.overview$avg)+0.01)
    }
    
  })
  
  
}#server


# Run the application 
shinyApp(ui = ui, server = server)

