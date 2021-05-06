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
library(shinyWidgets)
library(tidyverse)
library(shinythemes)
library(gt)
library(gtsummary)
library(rstanarm)
library(broom.mixed)
source(file = "graph_creation.R")
source(file = "model.R")


# Define UI for application


ui <- navbarPage(
    
    
    # Name of the title panel displayed on the top of the webpage
    
    "Adoption of Personal Technology and Devices Across Countries and Time",
    
    # Panel 1 This is my introductory panel and the main visual I want to show,
    # at this point. The interactive diagram shows the correlation between and
    # change in GDP and technology access across a span of 20 years.
    
    tabPanel("Technology Over Time",
             titlePanel("How does ownership of personal technology devices and income levels change over time?"),
             p("Countries experienced strong growth in technology access over time. 
               There is also a strong correlation between GDP per capita and technology access.", 
               style = "font-size:20px;"),
             
             # Main Panel. Changes as input or selection from site visitors change
             
             mainPanel(
                 selectInput(inputId = "plotly_type1",
                             label = "Choose a type of technology",
                             choices = c("Cell", "Internet"),
                             selected = "Cell")
                 
             ),
             br(),
             br(),
             br(),
             br(),
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
             p("Based on data from the World Bank and the United 
               Nations, the posterior distribution of a country's personal 
               technology access demonstrates that income levels still have a 
               significant correlation with technology adoption."),
             h3("Background"),
             p("While the World Bank uses GNI per capita, this analysis uses GDP per capita.
               Furthermore, the World Bank uses US$, converted from local currencies using
               the World Bank-specific Atlas method, whereas this representation uses
               International Dollars. Each international dollar would buy, in each respective
               country, a comparable amount of goods and services a U.S. dollar would buy in
               the United States. This is to account for Purchasing Power Parity."),
             p("As of July 1, 2020, the bounds for income classification of countries are: 
               <$1036 for low-income countries,  $1036-$4045 for lower-middle income countries,
                $4046-$12535 for upper-middle income countries, and >$12535 for high-income countries. 
               All units are in International Dollars."),
             h3("Cell Phone Access"),
             withMathJax(),
             p("The equation below represents the predictive model used to predict cell phone subscription of a country i."),
             helpText('$$CellPhoneSubscription_i = \\beta_0  + \\beta_1 GDP_i+ \\epsilon_i$$'),
             p("The predictive model I used was a linear model with average GDP per capita over five years (2014-2019) 
               as the 
               only parameter to consider. The Intercept Parameter value represents 
               the model's median estimation of the ''true value'' of the number of
               cell phone subscriptions for a country
               with hypothetical GDP per capita of 0 international dollars. The 
               Intercept Parameter value is 90.482535 subscriptions per 100 people. 
               Each dollar that per capita income rises by increases cell phone 
               subscriptions per 100 people by 0.0008.4863."),
             p("The table specifies the relationship between cell phone 
               subscription/ownership and the per capita income of a country, displayed 
               to an accuracy of 8 significant digits."),
             gt_output(outputId = "table1"),
             
             # Had to resize the plots and gt tables, or it would stretch across
             # the entire page, making values/changes in y-axis very hard to
             # see. Settled on a long rectangle shape
             
             plotOutput("plot1",
                        height = px(400),
                        width = px(600)),
             h3("Internet Access"),
             withMathJax(),
             p("The equation below represents the model used to predict internet access."),
             helpText('$$Internet_i = \\beta_0  + \\beta_1 GDP_i+ \\epsilon_i$$'),
             p("The predictive model I used was a linear model with average GDP per capita over five years (2014-2019) 
               as the 
               only parameter to consider. The Intercept Parameter value represents 
               the model's median estimation of the ''true value'' of the number of
               internet subscriptions for a country
               with hypothetical GDP per capita of 0 international dollars. The 
               Intercept Parameter value is 90.482535 subscriptions per 100 people, which 
               is much lower than the Interept Parameter value for cell phone subscriptions. 
               Each dollar that per capita income rises by increases internet 
               subscriptions per 100 people by 0.00108876."),
             p("The table below specifies the relationship between internet 
               subscription and the per capita income of a country, displayed to an accuracy of 8 significant digits.
               "),
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
             h3("Cell Phone Access by Country"),
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
             p("Looking at countries individually, most experience significant 
               increases in national cell phone subscriptions over time. However, 
               some exceptions remain. Bahrain, for instance, experienced what 
               the Human Rights Watch called a ''",
               a(href = "https://www.hrw.org/world-report/2017/country-chapters/bahrain#", "marked deterioration in the 
               human rights situation"), "''in 2016 as government authorities cracked 
               down on opposition groups and activists."),
             h3("Internet Access by Country"),
             mainPanel(
                 selectInput(inputId = "forplot4",
                             label = "Choose a Country",
                             choices = internet20$country,
                             selected = "China")
                 
             ),
             plotOutput("plot4",
                        height = px(400),
                        width = px(600)),
             br(),
             br(),
             br(),
             br(),
             p("Similar to cell phone subscriptions, national 
               internet access generally increased over time as well. Interestingly, 
               while Bahrain experienced noticable decreases in cell phone 
               subscriptions from 2016 to 2019, its internet access held steady and increased slightly.")
             ),
    
    # Panel 4
    # Discussion panel about my general work on this project.
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("I've spent the year exploring the student entrepreneurship through 
               Harvard Undergraduate Capital Partners, the campus VC club. Judging the 
               Innovation Fund, I came across startups promising to bring certain services to 
               countries around the world through personal tech. From health insurance mobile apps in the 
               Philippines to post-secondary education mentorship in Colombia, startup founders 
               have big visions and big claims."),
             p("However, I wondered whether the impact of certain 
               services would be limited due to a lack of access to certain types of personal technologies.
               Is a health service best served investing in a mobile app or a website, or 
               would in-person delivery strategies be more effective?
               What does access to cell phones, personal computers, and internet look like across different countries? 
               How does that correlate with the income per capita of a country, and does it 
               vary across geographical regions?"),
               
               p("Not only have I 
               been introduced to the potential for technology to make our lives
               more convenient, I've learned that access to mobile technology 
               grown significantly in countries around the world. 
               In this project, I'm interested in examining the precise 
               rate of adoption of personal technology and the spread of the internet 
               as GDP increases and time passes"),
             h3("About Me"),
             p("Hello there! My name is Alice Chen. I'm a Canadian freshman at Harvard 
               College studying Economics and CS. I'm passionate about using R to 
               study economic development, innovation & venture capital, and gender equity. 
             You can reach me at alicechen@college.harvard.edu or via", a(href = "https://www.linkedin.com/in/alicechen515/", "Linkedin.")),
             h3("My Repository"),
             p("You can access the code used in this project via my Github:" , a(href = "https://github.com/alicechen2002/global_technology", "github.com/alicechen2002/global_technology")),
             h3("My Data"),
             p("I've complied and cleaned various sources of data for my analysis. All sources are accessible below."),
             p(a(href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD", 
               "GDP Per Capita, adjusted for Purchasing Power Parity. Data from the World Bank")
             ),
             p(a(href = "https://data.worldbank.org/indicator/IT.NET.USER.ZS", 
                 "Percentage of Individuals using the Internet (%). Data from the World Bank."),
               a(href = "https://www.gapminder.org/data/", "Accessed via Gapminder.")
             ),
             p(a(href = "https://tcdata360.worldbank.org/indicators/entrp.household.computer?country=BRA&indicator=3427&viz=bar_chart&years=2016", 
                 "Percentage of Households with Personal Computer (%). Data from the World Bank")
             ),
             p(a(href = "https://data.worldbank.org/indicator/IT.CEL.SETS.P2", 
                 "Mobile Cellular Subscriptions per 100 People. Data from the World Bank")
             )
    )
)    
 


# Server takes input from the interactive website and outputs plots

server <- function(input, output) {
    
    # Animated graph of GDP and Internet access over time. Use renderPlotly 
    # because the plot is created by Plotly, not ggplot or ggplot2
    
    output$map <- renderPlotly( 
            if (input$plotly_type1 == "Internet") {GDPandInternetfig
            }
            else{GDPandCellfig}
    )
        
    
    # Animated graph of GDP and Cell Phone access over time.
    
    output$map2 <- renderImage(
        if(input$plot_type2 == "a"){GDPandCellfig} 
        else if(input$plot_type2 =="b"){GDPandCellfig}, 
        deleteFile=TRUE
    )
    
    # Plot of posterior distribution of cell phone access based on income levels.
    
    output$plot1 <- renderPlot({pecellplot}
    )
    
    # Plot of posterior distribution of internet access based on income levels.
    
    output$plot2 <- renderPlot({peinternetplot}
    )
    
    # Plot of specific country and cell phone access. Takes an input (forplot3)
    # as a request to see a specific country.
    
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
    
    # Plot of specific country and cell phone access. Takes a value/input (forplot4).
    
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
    
    # Model of GDP to Cell in a Table. I learned not to use curly braces (used
    # in renderPlot above, for instance) for render_gt, otherwise it does not
    # work.
    
    output$table1 <- render_gt(
        expr = cellGDPmodeltbl,
        height = px(300),
        width = px(600)
    )
    
    
    
    # Model of GDP to Internet in a Table.
    
    output$table2 <- render_gt(
        internetGDPmodeltbl,
        height = px(300),
        width = px(600)
    )
    

}
    


# Run the application 

shinyApp(ui = ui, server = server)


