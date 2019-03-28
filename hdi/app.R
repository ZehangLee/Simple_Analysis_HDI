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
library(DT)


source("hdi.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  # Sidebar with a slider input for number of bins 
  titlePanel("Simple Analysis of HDI"),
  
   # Application title
  tabsetPanel(
              tabPanel("Demography",
                       verticalLayout(
                         
                                      fluidRow(
                                        h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose quiry options"),
                                        column(width=5,
                                               selectInput(
                                                 inputId =  "demo_date_from", 
                                                 label = "Select start year:", 
                                                 choices =c(Choose='',2000:2017) 
                                               )),
                                        
                                        column(width=5,offset = 2,
                                               selectInput(
                                                 inputId =  "demo_date_to", 
                                                 label = "Select end year:", 
                                                 choices =c(Choose='',2000:2017)
                                               )
                                        )#column
                                      ),# fluidRow
                                      
                                      leafletOutput("demo.plot"),
                                      fluidRow(
                                        column(3,
                                               selectInput("demo.level",
                                                           "Development Level:",
                                                           c("All",
                                                             unique(hdi.databank.m$level)))
                                        ),
                                        column(3,
                                               selectInput("demo.region",
                                                           "Region:",
                                                           c("All",
                                                             unique(hdi.databank.m$Region)))
                                        )
                                      ),
                                      fluidRow(
                                        column(12, DT::dataTableOutput('demo.table'))
                                      ), #fluidRow
                                      plotlyOutput('demo.table.plot')
                         )#verticalLayout
                       ),#tabPanel
              tabPanel("Health",
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
                                      
                                      
                                      fluidRow(
                                        column(6, wellPanel(
                                          radioButtons("heal.choice", "Inquiry by",
                                                       choices = c(
                                                         geography = "heal.geography",
                                                         levels = "heal.levels"),
                                                       selected = NA)
                                        )),
                                        column(6,
                                               helpText("Note:Please choose geography if you want to",
                                                        "quiry by region or countries. If you care ",
                                                        "the differences between development levels, ",
                                                        "please choose level"))
                                        
                                      ),#fluidRow
                                      
                                      
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
              ),

              
              tabPanel("Income",
                       fluidRow(h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose quiry options"),
                                column(width=5,
                                       selectInput(
                                         inputId =  "income_date_from", 
                                         label = "Select start year:", 
                                         choices =c(Choose='',2000:2017) 
                                       )),
                                
                                column(width=5,offset = 2,
                                       selectInput(
                                         inputId =  "income_date_to", 
                                         label = "Select end year:", 
                                         choices =c(Choose='',2000:2017)
                                       )
                                )),
                       splitLayout()
                       )#tabPanel
   )#tabsetPanel
)#fluidPage

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  output$heal.ui <- renderUI({
    if (is.null(input$heal.choice))
      return()
    

    switch(input$heal.choice,
           "heal.geography" =fluidRow(
             verticalLayout(fluidRow(h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose inquiry geography"),
                                     column(8,
                                     pickerInput('heal.geography.in', 'Options (Up to 4)', choices = list(Region = unique(hdi.databank.m$Region),Country = unique(hdi.databank.m$country_name)), multiple=TRUE, options = list(`max-options` = 4,size=10))
                                     ),
                                     column(4,actionButton("refresh", "Refresh",icon("refresh"),style='margin-top:25px'))),
                            #fluidRow(column(11,actionButton("refresh", "Refresh"),offset = 5)),
                            helpText("Please refresh the page before change selections.",style='margin-left:10px'))
                      ),
           "heal.levels" =  fluidRow(
             verticalLayout(fluidRow(h4(style = "margin-left: 20px; margin-bottom: 30px;", "Please choose inquiry level"),
                      column(8,
                      pickerInput('heal.level.in', 'Options', unique(hdi.databank.m$level), multiple=TRUE, options = list(`max-options` = 4))
                       ),
                      column(4,actionButton("refresh", "Refresh",icon("refresh"),style='margin-top:25px'))),
                      helpText("Please refresh the page before change selections.",style='margin-left:10px'))
             
           )
           
    )
    

  })
  
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
  
  output$demo.plot<- renderLeaflet({
    if (is.null(input$demo_date_from)|is.null(input$demo_date_to))
      return()
    demo_plot_fun(demo_date_from=input$demo_date_from,
                  demo_date_to=input$demo_date_to)
    
  })
  
  observeEvent(input$refresh, {
    session$reload()
  })
  
  demodata <- reactive({
    data <- hdi.databank.m%>%filter(indicator_name %in% 
                                      c("Total population (millions)","Urban population (%)",
                                        "Young age (0-14) dependency ratio (per 100 people ages 15-64)",
                                        "Old-age (65 and older) dependency ratio (per 100 people ages 15-64)"))%>%
      filter((year>= input$demo_date_from) & (year<= input$demo_date_to)) %>%
      select(iso3,country_name,year,indicator_name,hdi,level,Region)%>%
      arrange(year,level)%>%drop_na(hdi)%>%
      group_by(indicator_name,country_name,level,Region)%>%  summarise(avg = mean(hdi))%>%
      spread(indicator_name,avg)%>%replace_na(list(`Old-age (65 and older) dependency ratio (per 100 people ages 15-64)`=0,
                                                   `Total population (millions)`=0,
                                                   `Urban population (%)`=0,
                                                   `Young age (0-14) dependency ratio (per 100 people ages 15-64)`=0))%>%
      column_to_rownames(var = "country_name")
    
  })
  
  output$demo.table=DT::renderDataTable({
    data=demodata()
        
    if (input$demo.level != "All") {
      data <- data[data$level == input$demo.level,]
    }
    if (input$demo.region != "All") {
      data <- data[data$Region == input$demo.region,]
    }

    data
  })

  output$demo.table.plot = renderPlotly({
    data=demodata()
    data$country_name=row.names(data)
    

    #s1 = input$demo.table_rows_selected
    s2 = input$demo.table_rows_current  # rows on the current page
    #s3 = input$x1_rows_all   
    
    #par(mar = c(4, 4, 1, .1))
    
    if (length(s2)) 
      print(
        ggplotly(ggplot(data=NULL,aes(x=country_name))+
                   geom_bar(aes(y=`Urban population (%)`,fill="Urban population (%)"),data=data[s2,],stat = "identity")+
                   geom_bar(aes(y=`Young age (0-14) dependency ratio (per 100 people ages 15-64)`,fill="Young age (0-14) dependency ratio (per 100 people ages 15-64)"),data=data[s2,],stat = "identity")+
                   geom_bar(aes(y=`Old-age (65 and older) dependency ratio (per 100 people ages 15-64)`,fill="Old-age (65 and older) dependency ratio (per 100 people ages 15-64)"),data=data[s2,],stat = "identity")
           )
        )
                                                    
                             
      
  })

  
}#server


# Run the application 
shinyApp(ui = ui, server = server)