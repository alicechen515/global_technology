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
             titlePanel("Predictive Models of Personal Technology Device Access
                        Based On Country's Income Level"),
             h6("Based on data from the World Bank and the United 
               Nations, the posterior distribution of a country's personal 
               technology access demonstrates that income levels still have a 
               significant correlation with technology adoption."),
             br(),
             p("While the World Bank uses GNI per capita, this analysis uses GDP per capita.
               Furthermore, the World Bank uses US$, converted from local currencies using
               the World Bank-specific Atlas method, whereas this representation uses
               International Dollars. Each international dollar would buy, in the respective
               country, a comparable amount of goods and services a U.S. dollar would buy in
               the United States. This is to account for Purchasing Power Parity."),
             p("As of July 1, 2020, the bounds for income classification of countries are: 
               <$1036 for low-income countries,  $1036-$4045 for lower-middle income countries,
                $4046-$12535 for upper-middle income countrues, and >$12535 for high-income countries. 
               All units are in International Dollars."),
             p("The table below specifies the relationship between cell phone 
               subscription/ownership and the per capita income of a country.
               Each dollar that per capita income rises by increases cell phone 
               subscriptions per 100 people by 0.0008.4863."),
             gt_output(outputId = "table1"),
             plotOutput("plot1"),
             p("The table below specifies the relationship between internet 
               subscription and the per capita income of a country.
               Each dollar that per capita income rises by increases internet 
               subscriptions per 100 people by 0.00108876."),
             gt_output(outputId = "table2"),
             plotOutput("plot2")
             ),
    tabPanel("Average Technology Access",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them"),
             plotlyOutput()),
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
    
    output$plot1 <- renderPlot({
        pecellplot}
    )
    
    output$plot2 <- renderPlot({
        peinternetplot}
    )
    
    output$plot3 <- renderPlot({
        
        gap_anim <- average %>%
            filter(country == "China") %>%
            select(`2000`:`2019`) %>%
            pivot_longer(cols = everything(), 
                         values_to = "cell_percent", 
                         names_to = "years") %>%
            drop_na() %>%
            ggplot(aes(x = years, y = cell_percent)) +
            geom_line(show.legend = FALSE, alpha = 0.7) +
            scale_size(range = c(2, 12)) +
            scale_x_log10() +
            labs(subtitle = "Life Expectancy and GDP per Capita (1952-2007)",
                 x = "GDP per Capita, USD",
                 y = "Life Expectancy, Years") +
            theme_linedraw() +
            transition_time(year) +
            labs(title = "Year: {frame_time}") +
            shadow_wake(wake_length = 0.1, alpha = FALSE)
        
    })
    
    # Model of GDP to Cell in a Table
    output$table1 <- render_gt(
        expr = cellGDPmodeltbl,
        height = px(600),
        width = px(600)
    )
    
    
    
    # Model of GDP to Internet in a Table
    output$table2 <- render_gt(
        internetGDPmodeltbl,
        height = px(600),
        width = px(600)
    )
    
    #renderImage()
    
    # output$map2 <- if(input$plot_type2 != "a")
    # {renderPlot({mean_child_mortality})}
    # else {renderPlot({child_mortality_tbl})}
    # 

}
    

# GDPandInternet %>%
#     plot_ly(
#         x = ~GDP, 
#         y = ~internet, 
#         #size = ~pop, 
#         color = ~Continent, 
#         frame = ~Year, 
#         text = ~country, 
#         hoverinfo = "text",
#         type = 'scatter',
#         mode = 'markers'
#     ) %>% 
#     layout(xaxis = list(
#         type = "log",
#         title = "GDP (international $, PPP-adjusted)"
#     ),
#     yaxis = list(
#         title = "Internet Use (%)"
#     )
#     )



# Run the application 
shinyApp(ui = ui, server = server)


