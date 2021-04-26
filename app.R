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
library(gt)
library(gtsummary)
source(file = "child_mortality_graphs.R")
source(file = "Model.R")


# Define UI for application
ui <- navbarPage(
    "Final Project Title",
    tabPanel("Technology Over Time",
             h1("How does ownership of personal technology devices and income levels change over time?"),
             p("Based on the UN Sustainable Development Goals", 
               style = "font-size:20px;"),
             br(),
             
             # Main Panel
             mainPanel(
                 selectInput(inputId = "plotly_type1",
                             label = "Choose a type of technology",
                             choices = c("Cell", "Internet"),
                             selected = "Cell")
                 
             ),
             plotlyOutput("map")
             ),
    tabPanel("Model",
             titlePanel("Predictive Model of Cell Phone Subscription Based on GDP per capita"),
             sidebarPanel(gt_output("table1")),
             mainPanel(plotOutput("plot3", 
                        width = 600,
                        height = 500)),
             titlePanel("Predictive Model of Internet Subscription Based on GDP per capita"),
             sidebarPanel(gt_output("table2")),
             mainPanel(plotOutput("plot4"))),
    tabPanel("Changes by Country",
             titlePanel("Predictive Model of Cell Phone Subscription Based on GDP per capita"),
             sidebarPanel(gt_output("table1")),
             mainPanel(plotOutput("plot3", 
                                  width = 600,
                                  height = 500)),
             titlePanel("Predictive Model of Internet Subscription Based on GDP per capita"),
             sidebarPanel(gt_output("table2")),
             mainPanel(plotOutput("plot4"))),
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
            if (input$plotly_type1 == "Internet") {GDPandInternetfig
            }
                else{GDPandCellfig}
            
            
    )
        

    
    
    output$map2 <- renderImage(
        if(input$plot_type2 == "a"){GDPandCellfig} 
        else if(input$plot_type2 =="b"){GDPandCellfig}, 
        deleteFile=TRUE
    )
    
    output$map3 <- renderPlotly(
            GDPandInternet %>%
                plot_ly(
                    x = ~GDP, 
                    y = ~internet, 
                    #size = ~pop, 
                    color = ~Continent, 
                    frame = ~Year, 
                    text = ~country, 
                    hoverinfo = "text",
                    type = 'scatter',
                    mode = 'markers'
                ) %>% 
                layout(xaxis = list(
                    type = "log",
                    title = "GDP (international $, PPP-adjusted)"
                ),
                yaxis = list(
                    title = "Internet Use (%)"
                )
                ) 

    )
    
    output$plot3 <- renderPlot(
        pecellplot
    )
    
    output$plot4 <- renderPlot(
        peinternetplot
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
    

GDPandInternet %>%
    plot_ly(
        x = ~GDP, 
        y = ~internet, 
        #size = ~pop, 
        color = ~Continent, 
        frame = ~Year, 
        text = ~country, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers'
    ) %>% 
    layout(xaxis = list(
        type = "log",
        title = "GDP (international $, PPP-adjusted)"
    ),
    yaxis = list(
        title = "Internet Use (%)"
    )
    )


# Run the application 
shinyApp(ui = ui, server = server)


