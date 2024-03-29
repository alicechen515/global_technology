
library(tidyverse)
library(tidycensus)
library(shinythemes)
library(shiny)
library(plotly)
library(gganimate)
library(gifski)
library(readr)
library(janitor)
library(ggplot2)
library(sf)
library(maps)
library(transformr)


# Data cleaning for cellphone data
# Set the column types based on output from spec() function

cellphones_100 <- read_csv(file = "raw_data/cell_phones_per_100_people.csv",
                           col_types = cols(
                             country = col_character(),
                             `1960` = col_double(),
                             `1961` = col_logical(),
                             `1962` = col_logical(),
                             `1963` = col_logical(),
                             `1964` = col_logical(),
                             `1965` = col_double(),
                             `1966` = col_double(),
                             `1967` = col_double(),
                             `1968` = col_double(),
                             `1969` = col_double(),
                             `1970` = col_double(),
                             `1971` = col_double(),
                             `1972` = col_double(),
                             `1973` = col_double(),
                             `1974` = col_double(),
                             `1975` = col_double(),
                             `1976` = col_double(),
                             `1977` = col_double(),
                             `1978` = col_double(),
                             `1979` = col_double(),
                             `1980` = col_double(),
                             `1981` = col_double(),
                             `1982` = col_double(),
                             `1983` = col_double(),
                             `1984` = col_double(),
                             `1985` = col_double(),
                             `1986` = col_double(),
                             `1987` = col_double(),
                             `1988` = col_double(),
                             `1989` = col_double(),
                             `1990` = col_double(),
                             `1991` = col_double(),
                             `1992` = col_double(),
                             `1993` = col_double(),
                             `1994` = col_double(),
                             `1995` = col_double(),
                             `1996` = col_double(),
                             `1997` = col_double(),
                             `1998` = col_double(),
                             `1999` = col_double(),
                             `2000` = col_double(),
                             `2001` = col_double(),
                             `2002` = col_double(),
                             `2003` = col_double(),
                             `2004` = col_double(),
                             `2005` = col_double(),
                             `2006` = col_double(),
                             `2007` = col_double(),
                             `2008` = col_double(),
                             `2009` = col_double(),
                             `2010` = col_double(),
                             `2011` = col_double(),
                             `2012` = col_double(),
                             `2013` = col_double(),
                             `2014` = col_double(),
                             `2015` = col_double(),
                             `2016` = col_double(),
                             `2017` = col_double(),
                             `2018` = col_double(),
                             `2019` = col_double()
                           )
                           
)

# Takes only the recent 20 years of values.

cell100 <- as_tibble(cellphones_100) %>%
  select(country, `2000`:`2019`)

# GDP Data cleaning

# "An international dollar would buy in the 
# cited country a comparable amount of goods and services a 
# U.S. dollar would buy in the United States. This term is often used in 
# conjunction with Purchasing Power Parity (PPP) data." (World Bank)

GDPraw1 <- read_csv(file = "raw_data/GDP_PPP.csv",
            
                   col_types = cols(
                     `Country Name` = col_character(),
                     `Country Code` = col_character(),
                     `Indicator Name` = col_character(),
                     `Indicator Code` = col_character(),
                     `1960` = col_logical(),
                     `1961` = col_logical(),
                     `1962` = col_logical(),
                     `1963` = col_logical(),
                     `1964` = col_logical(),
                     `1965` = col_logical(),
                     `1966` = col_logical(),
                     `1967` = col_logical(),
                     `1968` = col_logical(),
                     `1969` = col_logical(),
                     `1970` = col_logical(),
                     `1971` = col_logical(),
                     `1972` = col_logical(),
                     `1973` = col_logical(),
                     `1974` = col_logical(),
                     `1975` = col_logical(),
                     `1976` = col_logical(),
                     `1977` = col_logical(),
                     `1978` = col_logical(),
                     `1979` = col_logical(),
                     `1980` = col_logical(),
                     `1981` = col_logical(),
                     `1982` = col_logical(),
                     `1983` = col_logical(),
                     `1984` = col_logical(),
                     `1985` = col_logical(),
                     `1986` = col_logical(),
                     `1987` = col_logical(),
                     `1988` = col_logical(),
                     `1989` = col_logical(),
                     `1990` = col_double(),
                     `1991` = col_double(),
                     `1992` = col_double(),
                     `1993` = col_double(),
                     `1994` = col_double(),
                     `1995` = col_double(),
                     `1996` = col_double(),
                     `1997` = col_double(),
                     `1998` = col_double(),
                     `1999` = col_double(),
                     `2000` = col_double(),
                     `2001` = col_double(),
                     `2002` = col_double(),
                     `2003` = col_double(),
                     `2004` = col_double(),
                     `2005` = col_double(),
                     `2006` = col_double(),
                     `2007` = col_double(),
                     `2008` = col_double(),
                     `2009` = col_double(),
                     `2010` = col_double(),
                     `2011` = col_double(),
                     `2012` = col_double(),
                     `2013` = col_double(),
                     `2014` = col_double(),
                     `2015` = col_double(),
                     `2016` = col_double(),
                     `2017` = col_double(),
                     `2018` = col_double(),
                     `2019` = col_double(),
                     `2020` = col_logical()
                   )
                   )

# Delete the X66 column, since we don't need it, and the 2020 column as most
# countries do not have an updated value.

# For some reason, this still shows as an error when I run my Shiny App, but it
# still runs effectively.

GDPraw <- as_tibble(GDPraw1) %>%
  select(-`2020`, -`X66`)

# Internet data cleaning. Loads csv file.

internetraw <- read_csv(file = "raw_data/internet_users.csv",
                        col_types = cols(
                          country = col_character(),
                          `1960` = col_double(),
                          `1961` = col_logical(),
                          `1962` = col_logical(),
                          `1963` = col_logical(),
                          `1964` = col_logical(),
                          `1965` = col_double(),
                          `1966` = col_logical(),
                          `1967` = col_logical(),
                          `1968` = col_logical(),
                          `1969` = col_logical(),
                          `1970` = col_double(),
                          `1971` = col_logical(),
                          `1972` = col_logical(),
                          `1973` = col_logical(),
                          `1974` = col_logical(),
                          `1975` = col_double(),
                          `1976` = col_double(),
                          `1977` = col_double(),
                          `1978` = col_double(),
                          `1979` = col_double(),
                          `1980` = col_double(),
                          `1981` = col_double(),
                          `1982` = col_double(),
                          `1983` = col_double(),
                          `1984` = col_double(),
                          `1985` = col_double(),
                          `1986` = col_double(),
                          `1987` = col_double(),
                          `1988` = col_double(),
                          `1989` = col_double(),
                          `1990` = col_double(),
                          `1991` = col_double(),
                          `1992` = col_double(),
                          `1993` = col_double(),
                          `1994` = col_double(),
                          `1995` = col_double(),
                          `1996` = col_double(),
                          `1997` = col_double(),
                          `1998` = col_double(),
                          `1999` = col_double(),
                          `2000` = col_double(),
                          `2001` = col_double(),
                          `2002` = col_double(),
                          `2003` = col_double(),
                          `2004` = col_double(),
                          `2005` = col_double(),
                          `2006` = col_double(),
                          `2007` = col_double(),
                          `2008` = col_double(),
                          `2009` = col_double(),
                          `2010` = col_double(),
                          `2011` = col_double(),
                          `2012` = col_double(),
                          `2013` = col_double(),
                          `2014` = col_double(),
                          `2015` = col_double(),
                          `2016` = col_double(),
                          `2017` = col_double(),
                          `2018` = col_double(),
                          `2019` = col_double()
                        )
)

# I had trouble with the data until I converted it to a tibble. It now loads
# much faster.

internetraw <- as_tibble(internetraw)

# Takes only the recent 20 years of values.

internet20 <- internetraw %>%
  select(country, `2000`:`2019`) %>%
  rowwise() %>%
  mutate(tenyearinternet = mean(c(`2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`), na.rm = TRUE)) %>%
  mutate(fiveyearinternet = mean(c( `2015`, `2016`, `2017`, `2018`, `2019`), na.rm = TRUE)) %>%
  arrange(country)



# Countries & Continents labels data cleaning

clabel <- read_csv(file = "raw_data/countries-continents.csv",
                   col_types = cols(
                     Continent = col_character(),
                     Country = col_character()
                   ))

# Could have used the janitor package, but this was easy enough to rename.

continentlabel <- as_tibble(clabel) %>%
  rename("country" = Country)

# Adding the value of internet access to the continent labels, so I can organize
# by continent in the future. 

newinternet <- left_join(internet20, continentlabel, by = "country") 

# PC Ownership data cleaning


# % of people with personal computers

# This data is problematic. I have two sources, one from the United Nations that
# measures personal computer ownership per 100 people from 1990 to 2006, and
# another from the World bank that covers 2009-2019. I ended up not the United 
# Nations data, but am keeping it here in case I need it.

pcraw1 <- read_csv(file = "raw_data/personal_computers_per_100_people.csv",
                   col_types = cols(
                     country = col_character(),
                     `1990` = col_double(),
                     `1991` = col_double(),
                     `1992` = col_double(),
                     `1993` = col_double(),
                     `1994` = col_double(),
                     `1995` = col_double(),
                     `1996` = col_double(),
                     `1997` = col_double(),
                     `1998` = col_double(),
                     `1999` = col_double(),
                     `2000` = col_double(),
                     `2001` = col_double(),
                     `2002` = col_double(),
                     `2003` = col_double(),
                     `2004` = col_double(),
                     `2005` = col_double(),
                     `2006` = col_double()
                   )
)

# Again, defining as tibble for faster deployments.

pcraw <- as_tibble(pcraw1)

# This dataset has all the indicators included, hence why the table has 19870
# rows. Need to filter for the indicator we want, which is % of PC ownership.

pctotalraw <- read_csv(file = "raw_data/data.csv",
                       col_types = cols(
                         `Country ISO3` = col_character(),
                         `Country Name` = col_character(),
                         `Indicator Id` = col_double(),
                         Indicator = col_character(),
                         `Subindicator Type` = col_character(),
                         `2012` = col_double(),
                         `2013` = col_double(),
                         `2014` = col_double(),
                         `2015` = col_double(),
                         `2016` = col_double()
                       )
)



# Percent ownership of households with PCs.
# As mentioned above, I filtered for % of households.

pcpercent <- pctotalraw %>%
  filter(`Indicator` == "Households w/ personal computer, %") %>%
  filter(`Subindicator Type` == "% households") %>%
  rowwise() %>%
  mutate(fiveyearpc = mean(c( `2015`, `2016`), na.rm = TRUE))






# Filtered out Burkina as the World Bank data did not include Burkina
# Should go through and make sure other problematic countries are filtered out too.

newcell100 <- left_join(continentlabel, cell100, by = "country") %>%
  filter(country != "Burkina") %>%
  arrange(country)

# % of people with Cellphone over 20 years

cell20 <- newcell100 %>%
  rowwise() %>%
  mutate(tenyearcell = mean(c(`2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`), na.rm = TRUE)) %>%
  mutate(fiveyearcell = mean(c( `2015`, `2016`, `2017`, `2018`, `2019`), na.rm = TRUE)) %>%
  arrange(country)

# Long tibble of country, year, and cellphone ownership

celllong <- cell20 %>%
  pivot_longer(names_to = "Year", 
               values_to = "Percentage", 
               cols = `2000`:`2019` ) %>%
  mutate(Year = as.numeric(Year)) %>%
  drop_na(Percentage)



# Graphic of change in cell ownership by year
# Somehow I was unable to change the values to plot a geom_line graph, 
# but it worked for geom_point(). 

allcountriescell <-  as_tibble(celllong) %>%
  ggplot(aes(x = Year, 
             y = Percentage, 
             fill = Continent, 
             colour = Continent)) +
  facet_wrap(~Continent, nrow = 2) +
  geom_point( alpha = 0.7)  +
  transition_time(Year) +
  labs(title = "Years: {round(frame_time,0)}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

# GDP Data cleaning
# Takes away NaN values where there is no data on the country using na.rm = TRUE
# I used na.rm so that, when calculating averages etc, it ignores the value

GDP <- GDPraw %>%
  rename("country" = `Country Name`) %>%
  select(country, `2000`:`2019`) %>%
  rowwise() %>%
  mutate(tenyearGDP = mean(c(`2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`), 
                           na.rm = TRUE)) %>%
  mutate(fiveyearGDP = mean(c( `2015`, `2016`, `2017`, `2018`, `2019`), 
                            na.rm = TRUE)) %>%
  filter(fiveyearGDP != "NaN") 

GDPlonger <- GDP %>%
  pivot_longer(names_to = "Year", 
               values_to = "GDP", 
               cols = `2000`:`2019`) %>%
  mutate(Year = as.numeric(Year)) %>%
  drop_na(GDP)

# GDP and Cellphone values put together. This drops blank values.

GDPandCell <- left_join(celllong, GDPlonger, 
                        by = c("country", "Year")) %>%
  drop_na(GDP)

# Moving graph of GDP to cellphone ownership. This uses plotly and not ggplot,
# so be careful with outputs.

GDPandCellfig <- GDPandCell %>%
  plot_ly(
    x = ~GDP, 
    y = ~Percentage, 
    #size = ~pop, 
    color = ~Continent, 
    frame = ~Year, 
    text = ~country, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>% 
  layout(title = "GDP to Cell Phone Access per 100 people, 2000-2019",
         xaxis = list(
    title = "GDP (international $, PPP-adjusted)"
  ),
  yaxis = list(
    title = "Cell Phone Use (%)"
  )
  ) 

# This is the long internet data tibble, with countries names included in a
# column for easy plotting.

internetlong <- newinternet %>%
  select(-fiveyearinternet, -tenyearinternet) %>%
  pivot_longer(names_to = "Year", values_to = "internet", cols = `2000`:`2019`) %>%
  mutate(Year = as.numeric(Year)) %>%
  drop_na(internet)

# Moving graph of GDP and internet users and ownership. Again, this takes a long
# tibble and plots it into plotly.

GDPandInternet <- left_join(internetlong, GDPlonger, by = c("country", "Year")) %>%
  drop_na()

GDPandInternetfig <- GDPandInternet %>%
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
  layout(title = "GDP to Internet Access per 100 people, 2000-2019",
         xaxis = list(
    title = "GDP (international $, PPP-adjusted)"
  ),
  yaxis = list(
    title = "Internet Use (%)"
  )
  ) 


# Average, creating the large tibble where I can access all my information about
# various averages. It is concerning and I need to come back and make sure no
# information was lost with the left_joins.

# I dropped all information besides the most pertinent values, including five
# year and ten year averages of each type of ownership, using the select
# function, Then I joined all the individual tibbles together.

internetavg <- newinternet %>%
  select(Continent, country, fiveyearinternet, tenyearinternet)

cellavg <- cell20 %>%
  select(Continent, country, fiveyearcell, tenyearcell)

pcavg <- pcpercent %>%
  rename("country" = `Country Name`) %>%
  select(country, fiveyearpc) %>%
  filter(fiveyearpc != "NaN")

GDPavg <- GDP %>%
  select(country, fiveyearGDP, tenyearGDP)


average1 <- left_join(cellavg, internetavg, by = c("country", "Continent")) %>%
  left_join(y = pcavg, by = "country") %>%
  left_join(y = GDPavg, by = "country")

average <- as_tibble(average1)

# Somehow was unable to plot this on the Shiny app, and while others had the
# same problem I did on Stack Overflow, I implemented all their suggestions in
# the plot commented below and it did not work.

# The original plots work well locally.

# internetcellplot5 <- average %>%
#   drop_na() %>%
#   plot_ly(
#   x = ~fiveyearinternet,
#   y = ~fiveyearcell,
#   size = ~fiveyearGDP*100,
#   color = ~Continent,
#   #frame = ~Year,
#   text = ~country,
#   hoverinfo = "text",
#   type = 'scatter',
#   mode = 'markers'
# ) %>%
#   layout(title = "Average Internet Access and Cellphone Ownership, 2015-2019",
#          xaxis = list(showgrid = TRUE,
#                       title = "Internet Access (%)",
#                       yaxis = list(showgrid = TRUE,
#                                    title = "Cellphone Ownership (%)"
#                       )
#          ))
# 
# internetcellplot10 <- average %>% plot_ly(
#   x = ~tenyearinternet,
#   y = ~tenyearcell,
#   span = ~tenyearGDP,
#   color = ~Continent,
#   #frame = ~Year,
#   text = ~country,
#   hoverinfo = "text",
#   type = 'scatter',
#   mode = 'markers',
#   marker = list(sizemode = 'diameter'),
#   fill = ~""
# ) %>%
#   layout(title = "Average Internet Access and Cellphone Ownership, 2009-2019",
#          xaxis = list(showgrid = TRUE,
#                       title = "Internet Access (%)"),
#          yaxis = list(showgrid = TRUE,
#                       title = "Cellphone Ownership (%)",
#                       range = c(0, 200))
#   )
# 
# 
# 
# 
# pccell5 <- average %>% plot_ly(
#   x = ~fiveyearpc,
#   y = ~fiveyearcell,
#   size = ~tenyearGDP,
#   color = ~Continent,
#   #frame = ~Year,
#   text = ~country,
#   hoverinfo = "text",
#   type = 'scatter',
#   mode = 'markers',
#   fill = ""
# ) %>%
#   layout(title = "Average Internet Access and Cellphone Ownership, 2009-2019",
#          xaxis = list(showgrid = TRUE,
#                       title = "Internet Access (%)"),
#          yaxis = list(showgrid = TRUE,
#                       title = "PC Ownership (%)",
#                       range = c(0, 200))
#   )
# 
# 
