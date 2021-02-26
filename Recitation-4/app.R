#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#setup
library(shiny)
library(tidyverse)
library(tidycensus)

census_api_key("bb2250b0fd264a0c25486d53d100c9907eec9d62", overwrite = TRUE, install = TRUE)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application
ui <- navbarPage(
    "Final Project Title",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
                         )),
                     mainPanel(plotOutput("map")))
             )),
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
             p("I will use GapMinder for immunization information and World Bank data for a country's income and development index.")))

# Define server logic required 
server <- function(input, output) {
    
    racevars <- c(White = "B02001_002", 
                  Black = "B02001_003", 
                  Asian = "B02001_005",
                  Hispanic = "B03003_003")
    harris <- get_acs(geography = "tract",
                      variables = racevars, 
                      year = 2018,
                      state = "TX",
                      county = "Harris County",
                      geometry = TRUE,
                      summary_var = "B02001_001") 
    output$map <- renderPlot({harris %>%
            mutate(Percent = 100 * (estimate / summary_est)) %>%
            ggplot(aes(fill = Percent, color = Percent)) +
            facet_wrap(~ variable) +
            geom_sf() +
            scale_fill_viridis_c(direction = -1) +
            scale_color_viridis_c(direction = -1) +
            labs(title = "Racial geography of Harris County, Texas",
                 caption = "Source: American Community Survey 2014-2018") +
            theme_void()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)