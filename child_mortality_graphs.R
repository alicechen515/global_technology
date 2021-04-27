
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



child_mortality <- read_csv(file = "raw_data/child_mortality_0_5_year_olds_dying_per_1000_born.csv",
                            col_types = cols(
                              .default = col_double(),
                              country = col_character()
                            )
                            )

child_mortality_tbl <- as_tibble(child_mortality) %>%
  select("country", "2000", "2020") %>%
  slice(1:10) %>%
  ggplot(mapping = aes(x = country, 
                       y = `2020`)) +
  geom_point()

child_mortality_tbl

mean_child_mortality <- as_tibble(child_mortality) %>%
  rowwise() %>%
  mutate(mean = mean(c_across(`1800`:`2020`))) %>%
  drop_na() %>%
  select("country", "mean") %>%
  slice(1:10) %>%
  ggplot(mapping = aes(x = country, 
                       y = mean)) +
  geom_point()

mean_child_mortality




# Data cleaning for cellphone data

cellphones_100 <- read_csv(file = "cell_phones_per_100_people.csv",
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

cell100 <- as_tibble(cellphones_100) %>%
  select(country, `2000`:`2019`)

# GDP Data cleaning

# "An international dollar would buy in the cited country a comparable amount of goods and services a U.S. dollar would buy in the United States. This term is often used in conjunction with Purchasing Power Parity (PPP) data." (World Bank)

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

# Delete the X66 column, since we don't need it.

GDPraw <- as_tibble(GDPraw1) %>%
  select(-`2020`, -`X66`)

# Internet data cleaning

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

internetraw <- as_tibble(internetraw)

internet20 <- internetraw %>%
  select(country, `2000`:`2019`) %>%
  rowwise() %>%
  mutate(tenyearinternet = mean(c(`2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`), na.rm = TRUE)) %>%
  mutate(fiveyearinternet = mean(c( `2015`, `2016`, `2017`, `2018`, `2019`), na.rm = TRUE))

# Countries & Continents labels data cleaning

clabel <- read_csv(file = "countries-continents.csv",
                   col_types = cols(
                     Continent = col_character(),
                     Country = col_character()
                   ))


continentlabel <- as_tibble(clabel) %>%
  rename("country" = Country)

newinternet <- left_join(internet20, continentlabel, by = "country") 

# PC Ownership data cleaning


# % of people with personal computers

# This data is problematic. I have two sources, one from the United Nations that measures personal computer ownership per 100 people from 1990 to 2006, and another from the World bank 

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

pcraw <- as_tibble(pcraw1)

# This dataset has all the indicators included, hence why the table has 19870 rows. Need to filter for the indicator we want, which is % of PC ownership.
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



# Percent ownership of households with PCs
pcpercent <- pctotalraw %>%
  filter(`Indicator` == "Households w/ personal computer, %") %>%
  filter(`Subindicator Type` == "% households") %>%
  rowwise() %>%
  mutate(fiveyearpc = mean(c( `2015`, `2016`), na.rm = TRUE))






# Filtered out Burkina as the World Bank data did not include Burkina

newcell100 <- left_join(continentlabel, cell100, by = "country") %>%
  filter(country != "Burkina")


#% of people with Cellphone over 20 years
cell20 <- newcell100 %>%
  rowwise() %>%
  mutate(tenyearcell = mean(c(`2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`), na.rm = TRUE)) %>%
  mutate(fiveyearcell = mean(c( `2015`, `2016`, `2017`, `2018`, `2019`), na.rm = TRUE))

countrycellpercent <- cell20 %>%
  filter(country == "China") %>%
  select( `2000`:`2019`) %>%
  pivot_longer(names_to = "Year", values_to = "Percentage", cols = everything() ) %>%
  mutate(Year = as.numeric(Year)) %>%
  drop_na() %>%
  ggplot(aes(x = Year, y = Percentage)) +
  geom_point( alpha = 0.7) +
  theme_bw() +
  transition_reveal(Year) +
  labs(title = "Year: {round(frame_time,0)}") +
  shadow_wake(wake_length = 1, alpha = FALSE)


# Long tibble of country, year, and cellphone ownership

celllong <- cell20 %>%
  pivot_longer(names_to = "Year", values_to = "Percentage", cols = `2000`:`2019` ) %>%
  mutate(Year = as.numeric(Year)) %>%
  drop_na(Percentage)



# Graphic of change in cell ownership by year

allcountriescell <-  as_tibble(celllong) %>%
  ggplot(aes(x = Year, y = Percentage, fill = Continent,colour = Continent, na.rm = TRUE )) +
  facet_wrap(~Continent, nrow = 2) +
  geom_point( alpha = 0.7)  +
  transition_time(Year) +
  labs(title = "Years: {round(frame_time,0)}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

# GDP Data

GDP <- GDPraw %>%
  rename("country" = `Country Name`) %>%
  select(country, `2000`:`2019`) %>%
  rowwise() %>%
  mutate(tenyearGDP = mean(c(`2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`), na.rm = TRUE)) %>%
  mutate(fiveyearGDP = mean(c( `2015`, `2016`, `2017`, `2018`, `2019`), na.rm = TRUE)) %>%
  filter(fiveyearGDP != "NaN") 

GDPlonger <- GDP %>%
  pivot_longer(names_to = "Year", values_to = "GDP", cols = `2000`:`2019`) %>%
  mutate(Year = as.numeric(Year)) %>%
  drop_na(GDP)

#GDP and Cellphone 

GDPandCell <- left_join(celllong, GDPlonger, by = c("country", "Year")) %>%
  drop_na(GDP)
# Moving graph of GDP to cellphone ownership
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
  layout(xaxis = list(
    type = "log",
    title = "GDP (international $, PPP-adjusted)"
  ),
  yaxis = list(
    title = "Cell Phone Use (%)"
  )
  ) 

GDPandCellfig

internetlong <- newinternet %>%
  select(-fiveyearinternet, -tenyearinternet) %>%
  pivot_longer(names_to = "Year", values_to = "internet", cols = `2000`:`2019`) %>%
  mutate(Year = as.numeric(Year)) %>%
  drop_na(internet)

# Moving graph of GDP and internet usership

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
  layout(xaxis = list(
    type = "log",
    title = "GDP (international $, PPP-adjusted)"
  ),
  yaxis = list(
    title = "Internet Use (%)"
  )
  ) 

# Average of many things

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

# if fill does not exist, `size` controls line.width

#average <- average[order(average$fiveyearGDP),]

# internetcellplot5a <- average %>% 
#   drop_na() %>%
#   plot_ly(
#   x = ~fiveyearinternet, 
#   y = ~fiveyearcell, 
#   span = ~fiveyearGDP ,
#   color = ~Continent, 
#   #frame = ~Year, 
#   text = ~country, 
#   hoverinfo = "text",
#   type = 'scatter',
#   mode = 'markers',
#   marker = list(size = ~fiveyearGDP),
#   fill = ~""
# ) %>% 
#   layout(title = "Average Internet Access and Cellphone Ownership, 2015-2019",
#          xaxis = list(showgrid = TRUE, 
#                       title = "Internet Access (%)",
#                       yaxis = list(showgrid = TRUE,
#                                    title = "Cellphone Ownership (%)"
#                       )
#          )) 
# internetcellplot5a

internetcellplot5 <- average %>% 
  drop_na() %>%
  plot_ly(
  x = ~fiveyearinternet, 
  y = ~fiveyearcell, 
  size = ~fiveyearGDP*100, 
  color = ~Continent, 
  #frame = ~Year, 
  text = ~country, 
  hoverinfo = "text",
  type = 'scatter',
  mode = 'markers'
) %>% 
  layout(title = "Average Internet Access and Cellphone Ownership, 2015-2019",
         xaxis = list(showgrid = TRUE, 
                      title = "Internet Access (%)",
                      yaxis = list(showgrid = TRUE,
                                   title = "Cellphone Ownership (%)"
                      )
         )) 

internetcellplot10 <- average %>% plot_ly(
  x = ~tenyearinternet, 
  y = ~tenyearcell, 
  span = ~tenyearGDP, 
  color = ~Continent, 
  #frame = ~Year, 
  text = ~country, 
  hoverinfo = "text",
  type = 'scatter',
  mode = 'markers',
  marker = list(sizemode = 'diameter'),
  fill = ~""
) %>% 
  layout(title = "Average Internet Access and Cellphone Ownership, 2009-2019",
         xaxis = list(showgrid = TRUE, 
                      title = "Internet Access (%)"),
         yaxis = list(showgrid = TRUE,
                      title = "Cellphone Ownership (%)",
                      range = c(0, 200))
  )




pccell5 <- average %>% plot_ly(
  x = ~fiveyearpc, 
  y = ~fiveyearcell, 
  size = ~tenyearGDP, 
  color = ~Continent, 
  #frame = ~Year, 
  text = ~country, 
  hoverinfo = "text",
  type = 'scatter',
  mode = 'markers',
  fill = ""
) %>% 
  layout(title = "Average Internet Access and Cellphone Ownership, 2009-2019",
         xaxis = list(showgrid = TRUE, 
                      title = "Internet Access (%)"),
         yaxis = list(showgrid = TRUE,
                      title = "PC Ownership (%)",
                      range = c(0, 200))
  )

