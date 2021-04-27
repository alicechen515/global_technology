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

# Setup for App

library(shiny)
library(tidyverse)
library(shinythemes)
library(gt)
library(gtsummary)
library(rstanarm)
library(broom.mixed)
source(file = "child_mortality_graphs.R")
source(file = "Model.R")


# Define UI for application


ui <- navbarPage(
    
    # Name of the title panel displayed on the top of the webpage
    
    "Adoption of Personal Technology and Devices Across Countries and Time",
    
    # Panel 1 This is my introductory panel and the main visual I want to show,
    # at this point. The interactive diagram shows the correlation between and
    # change in GDP and technology access across a span of 20 years.
    
    tabPanel("Technology Over Time",
             h1("How does ownership of personal technology devices and income levels change over time?"),
             p("Countries experienced strong growth in technology access over time. 
               There is also a strong correlation between GDP per capita and technology access.", 
               style = "font-size:20px;"),
             br(),
             
             # Main Panel. Changes as input or selection from site visitors change
             
             mainPanel(
                 selectInput(inputId = "plotly_type1",
                             label = "Choose a type of technology",
                             choices = c("Cell", "Internet"),
                             selected = "Cell")
                 
             ),
             plotlyOutput("map")
             ),
    
    # Panel 2 This is a predictive model I created using stan_glm() that
    # highlights the correlation between income and technology access. 
    
    # Using gt was very finicky and I has significant trouble with plotting it
    # to the Shiny. Seems like the difference between creating a plot vs a gt in
    # the server part of this code is whether or not to use curly braces.
    
    # Each table is followed by a plit, that plots the median value for each income bracket.
    
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
             
             # Had to resize the plots and gt tables, or it would stretch across
             # the entire page, making values/changes in y-axis very hard to
             # see. Settled on a long rectangle shape
             
             plotOutput("plot1",
                        height = px(400),
                        width = px(600)),
             p("The table below specifies the relationship between internet 
               subscription and the per capita income of a country.
               Each dollar that per capita income rises by increases internet 
               subscriptions per 100 people by 0.00108876."),
             gt_output(outputId = "table2"),
             plotOutput("plot2",
                        height = px(400),
                        width = px(600))
             ),
    
    # Panel 3
    # This panel looks simple, but it is because I was unable to print a
    # stagnant plot_ly and instead had to revert back to ggplot. I also was
    # unable to animate a ggplot that was a line graph, instead of a scatter or
    # point graph.
    
    # This panel takes the input for a specific country and only displays the
    # graph for that country.
    
    tabPanel("National Technology Access Over Time",
             titlePanel("How does access to internet and cell phones change 
                        over time for each country?"),
             p("Most countries see clear growth in both cell phone and 
               internet access from 2000 to 2019."),
             h6("Cell Phone Access by Country"),
             mainPanel(
                 selectInput(inputId = "forplot3",
                             label = "Choose a Country",
                             choices = cell20$country,
                             selected = "China")
                 
             ),
             plotOutput("plot3",
                        height = px(400),
                        width = px(600)),
             br(),
             br(),
             br(),
             br(),
             h6("Internet Access by Country"),
             mainPanel(
                 selectInput(inputId = "forplot4",
                             label = "Choose a Country",
                             choices = internet20$country,
                             selected = "China")
                 
             ),
             plotOutput("plot4",
                        height = px(400),
                        width = px(600))
             ),
    
    # Panel 4
    # Discussion panel about my general work on this project.
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("I've spent the year working in the student startup space at 
               Boston and with various venture capital groups. Not only have I 
               been introduced to the potential for technology to make our lives
               more convenient, I've learned that access to mobile technology 
               grown significantly in countries around the world. 
               The phenomenon of 'leapfrogging' is studied in developmental 
               economics, referencing countries that jump beyond certain stages 
               of technological/economic development in an order dissimilar to 
               many western countries. I'm interested in examining the precise 
               rate of adoption of mobile technology and the spread of the internet 
               as GDP increases and time passes"),
             h3("About Me"),
             p("My name is Alice Chen and I'm a Canadian freshman at Harvard 
               College studying Economics and CS. I'm passionate about using R to 
               study economic development, innovation & venture capital, and gender equity. 
             You can reach me at alicechen@college.harvard.edu."),
             h4("My Repo"),
             p("https://github.com/alicechen2002/-gov1005-recitation-week-4-"),
             h5("My Data"),
             p("I will use GapMinder for immunization information and World Bank data for a country's income and development index.")
             )
    )
    


# Server takes input from the interactive website and outputs plots

server <- function(input, output) {
    
    # Animated graph of GDP and Internet access over time
    
    output$map <- renderPlotly( 
            if (input$plotly_type1 == "Internet") {GDPandInternetfig
            }
                else{GDPandCellfig}
            
    )
        

    # Animated graph of GDP and Cell Phone access over time
    
    output$map2 <- renderImage(
        if(input$plot_type2 == "a"){GDPandCellfig} 
        else if(input$plot_type2 =="b"){GDPandCellfig}, 
        deleteFile=TRUE
    )
    
    # Animated graph of second GDP and Cell Phone access plot over time
    
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
    
    # Plot of posterior distribution of cell phone access based on income levels
    
    output$plot1 <- renderPlot({
        pecellplot}
    )
    
    # Plot of posterior distribution of internet access based on income levels
    
    output$plot2 <- renderPlot({
        peinternetplot}
    )
    
    # Plot of specific country and cell phone access
    
    output$plot3 <- renderPlot({ 
        cell20 %>%
        filter(country == input$forplot3) %>%
        select( `2000`:`2019`) %>%
        pivot_longer(names_to = "Year", values_to = "Percentage", cols = everything() ) %>%
        mutate(Year = as.numeric(Year)) %>%
        drop_na() %>%
        ggplot(aes(x = Year, y = Percentage)) +
        geom_point( alpha = 0.7) +
        theme_bw() +
        labs(title = "National Cell Phone Subscription over a 20 Year Period",
             x = "Year",
             y = "Number per 100 People")
    })
    
    # Plot of specific country and cell phone access
    
    output$plot4 <- renderPlot({ 
        internet20 %>%
            filter(country == input$forplot4) %>%
            select( `2000`:`2019`) %>%
            pivot_longer(names_to = "Year", values_to = "Percentage", cols = everything() ) %>%
            mutate(Year = as.numeric(Year)) %>%
            drop_na() %>%
            ggplot(aes(x = Year, y = Percentage)) +
            geom_point( alpha = 0.7) +
            theme_bw() +
            labs(title = "National Internet Subscription over a 20 Year Period",
                 x = "Year",
                 y = "Number per 100 People")
    })
    
    # Model of GDP to Cell in a Table
    
    output$table1 <- render_gt(
        expr = cellGDPmodeltbl,
        height = px(300),
        width = px(600)
    )
    
    
    
    # Model of GDP to Internet in a Table
    
    output$table2 <- render_gt(
        internetGDPmodeltbl,
        height = px(300),
        width = px(600)
    )
    
    #renderImage()
    
    # output$map2 <- if(input$plot_type2 != "a")
    # {renderPlot({mean_child_mortality})}
    # else {renderPlot({child_mortality_tbl})}
    # 

}
    


# Run the application 
shinyApp(ui = ui, server = server)


