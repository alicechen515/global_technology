#setup
library(shiny)
library(tidyverse)
library(shinythemes)
library(gt)
library(gtsummary)
source(file = "graph_creation.R")
source(file = "model.R")


ui <- navbarPage(
  "Global Access to Personal Technology",
    tabPanel("International technology adoption over time",
             h1("How does ownership of personal technology devices and income levels change over time?"),
             p("Based on the UN Sustainable Development Goals", 
               style = "font-size:20px;"),
             br(),
              #titlePanel("Harvard Qscores Data"),
              mainPanel(
                   
                  selectInput(inputId = "plotly_type1",
                                 label = "Choose a type of technology",
                                 choices = c("Cell", "Internet"),
                                 selected = "Cell"
                              )
                      
                   ),
                   width = 300),
               mainPanel(
                 plotlyOutput("map"
                            #width = 1000,
                            #height = 700)
                            )),
    
    tabPanel("Model",
             titlePanel("Predictive Model of Cell Phone Subscription Based on GDP per capita"),
             gt_output(outputId = "table1"),
             plotlyOutput("map2",
                        width = 450,
                        height = 400),
             titlePanel("Map of % HH Below Poverty Line"),
             plotlyOutput("map2",
                          width = 450,
                          height = 400)),
    
    tabPanel("Other Map",
             titlePanel("Another way to bring in a Map"),
             plotlyOutput("map3"))
    
    
  
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
  
  output$plot3 <- renderPlot({
    pecellplot}
  )
  
  output$plot4 <- renderPlot({
    peinternetplot}
  )
  
  # Model of GDP to Cell in a Table
  output$table1 <- render_gt(
    cellGDPmodeltbl
  )
  
  
  
  # Model of GDP to Internet in a Table
  output$table2 <- render_gt(
    internetGDPmodeltbl
  )
  
  #renderImage()
  
  # output$map2 <- if(input$plot_type2 != "a")
  # {renderPlot({mean_child_mortality})}
  # else {renderPlot({child_mortality_tbl})}
  # 
  
}

# tabPanel("Model",
#          titlePanel("Predictive Model of Cell Phone Subscription Based on GDP per capita"),
#          gt_output("table1"),
#          mainPanel(plotOutput("plot3", 
#                               width = 600,
#                               height = 500)),
#          titlePanel("Predictive Model of Internet Subscription Based on GDP per capita"),
#          gt_output("table2"),
#          mainPanel(plotOutput("plot4"))),
# tabPanel("Changes by Country",
#          titlePanel("Predictive Model of Cell Phone Subscription Based on GDP per capita"),
#          gt_output("table1"),
#          mainPanel(plotOutput("plot3", 
#                               width = 600,
#                               height = 500)),
#          titlePanel("Predictive Model of Internet Subscription Based on GDP per capita"),
#          gt_output("table2"),
#          mainPanel(plotOutput("plot4"))),

# Run the application 

shinyApp(ui = ui, server = server)


