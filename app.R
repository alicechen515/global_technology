#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#Storytrlling
#Data collection/cleaning
#Modelling
#descriptions for everything done

#setup
library(shiny)
library(tidyverse)
library(shinythemes)
source(file = "child_mortality_graphs.R")
source(file = "Model.R")


# Define UI for application
ui <- navbarPage(
    "Final Project Title",
    tabPanel("My Plot",
             fluidPage(
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
                         )),
                     mainPanel(plotOutput("map",
                                          width = 450,
                                          height = 500))),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type2",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
                         )),
                     mainPanel(plotOutput("map2", 
                                          width = 450,
                                          height = 500)))
             )),
    tabPanel("Model",
             titlePanel("Predictive Model of Cell Phone Subscription Based on GDP per capita"),
             gt_output("table1"),
             plotOutput("plot3", 
                        width = 600,
                        height = 500),
             titlePanel("Predictive Model of Internet Subscription Based on GDP per capita"),
             gt_output("table2")),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is Alice Chen and I study Economics. 
             You can reach me at alicechen@college.harvard.edu."),
             h4("My Repo"),
             p("https://github.com/alicechen2002/-gov1005-recitation-week-4-"),
             h5("My Data"),
             p("I will use GapMinder for immunization information and World Bank data for a country's income and development index.")
             )
    )

# Define server logic required 
server <- function(input, output) {
    
    output$map <- renderPlotly(
        if(input$plot_type == "a"){internetcellplot5} 
        else if(input$plot_type =="b"){internetcellplot10}
    )
    
    output$map2 <- renderImage(
        if(input$plot_type2 == "a"){makecountrycell(c = "China")} 
        else if(input$plot_type2 =="b"){makecountrycell(c = "China")}
    )
    
    output$plot3 <- renderPlot(
        pecellplot
    )
    
    # Model of GDP to Cell in a Table
    output$table1 <- render_gt({
        cellGDPmodeltbl
    })
    
    
    
    # Model of GDP to Internet in a Table
    output$table2 <- render_gt({
        internetGDPmodeltbl
    })
    
    #renderImage()
    
    # output$map2 <- if(input$plot_type2 != "a")
    # {renderPlot({mean_child_mortality})}
    # else {renderPlot({child_mortality_tbl})}
    # 

}
    




# Run the application 
shinyApp(ui = ui, server = server)


