
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
library(rstanarm)
library(gt)
library(gtsummary)
library(ggdist)

source(file = "child_mortality_graphs.R")


# While the World Bank uses GNI per capita, this analysis uses GDP per capita.
# Furthermore, the World Bank uses US$, converted from local currencies using
# the World Bank-specific Atlas method, whereas this representation uses
# International Dollars. Each international dollar would buy, in the respective
# country, a comparable amount of goods and services a U.S. dollar would buy in
# the United States. This is to account for Purchasing Power Parity.

# As of July 1, 2020, the bounds are: low (<$1036), lower-middle (1036-4045),
# upper-middle (4046-12535), and high (> 12535)

# Finding out the median income of low, lower-middle, upper-middle, and
# high-income countries, using a similar method used by the World Bank.

# Low-income

lowtibble <- average %>%
  select(fiveyearGDP) %>%
  filter(fiveyearGDP < 1036) 

lowincome <- median(lowtibble$fiveyearGDP)

# Lower-middle-income

lowmidtibble <- average %>%
  select(fiveyearGDP) %>%
  filter(fiveyearGDP > 1036) %>%
  filter(fiveyearGDP < 4046)

lowmidincome <- median(lowmidtibble$fiveyearGDP)

# Upper-middle-income

uppermidtibble <- average %>%
  select(fiveyearGDP) %>%
  filter(fiveyearGDP <= 12535) %>%
  filter(fiveyearGDP >= 4046)

uppermidincome <- median(uppermidtibble$fiveyearGDP)

# High-income

hightibble <- average %>%
  select(fiveyearGDP) %>%
  filter(fiveyearGDP > 12535) 

highincome <- median(hightibble$fiveyearGDP)


# Using previously created tibble average, which consists of of five and yeah
# year averages of internet, cell phone, pc, and GDP.

# Created models for each of them, where GDP is the only parameter used to
# predict the outcome

cellGDPmodel <- stan_glm(data = average,
         formula = fiveyearcell ~ fiveyearGDP,
         family = gaussian,
         seed = 15,
         refresh = 0)


cellGDPmodeltbl <- tbl_regression(cellGDPmodel, 
                                  intercept = TRUE, 
                                  estimate_fun = function(x) style_sigfig(x, digits = 8)) %>%
  as_gt() %>%
  tab_header(title = md("**Proportion of Population with Cell Phone Subscription**"),
             subtitle = "How GDP predicts Percentage of Mobile Cellular Subscriptions per 100 People") %>%
  tab_source_note(md("Source: World Bank (2020) and UN (2020)")) %>% 
  cols_label(estimate = md("**Parameter**"))

# Creating a posterior distribution for cell phone subscriptions

newobscell <- tibble(fiveyearGDP = 
                       c(lowincome, lowmidincome, uppermidincome, highincome),
                     names = c("Low Income", "Low-Middle Income", "Upper Middle Income", "High Income")
                     )

pecell <- posterior_epred(cellGDPmodel,
                newdata = newobscell) %>%
  as_tibble %>%
  set_names(newobscell$names)

# Creating the plot for posterior of cell phone and GDP. Is stat_slab the best
# way to visualize this data? Would histograms that are facet_wrapped be better?

pecelllong <- pecell %>%
  pivot_longer(cols = everything(), 
               names_to = "Income Classification", 
               values_to = "Cell Phone Ownership")

pecellplot <- pecelllong %>%
  ggplot(aes(x = `Cell Phone Ownership`, 
             y = `Income Classification`)) +
  stat_slab(alpha = 0.3) +
  labs(title = "Proportion of Population with Cell Phone Subscription",
       subtitle = "How GDP predicts Percentage of Mobile Cellular Subscriptions per 100 People",
       x = "Cell Phone Subscription per 100 people",
       y = "Income Classification",
       caption = "Source: World Bank (2020) and UN (2020)") +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_bw()

# Creating stan_glm model for internet to GDP

internetGDPmodel <- stan_glm(data = average,
                             formula = fiveyearinternet ~ fiveyearGDP,
                             family = gaussian,
                             seed = 15,
                             refresh = 0)

internetGDPmodeltbl <- tbl_regression(internetGDPmodel, 
                                  intercept = TRUE, 
                                  estimate_fun = function(x) style_sigfig(x, digits = 8)) %>%
  as_gt() %>%
  tab_header(title = md("**Proportion of Population with Internet Subscriptions**"),
             subtitle = "How GDP predicts Percentage of Internet Subscriptions per 100 People") %>%
  tab_source_note(md("Source: World Bank (2020) and UN (2020)")) %>% 
  cols_label(estimate = md("**Parameter**"))

# Creating a posterior distribution for internet subscriptions

newobsinternet <- tibble(fiveyearGDP = 
                       c(lowincome, lowmidincome, uppermidincome, highincome),
                     names = c("Low Income", "Low-Middle Income", "Upper Middle Income", "High Income")
)

peinternet <- posterior_epred(internetGDPmodel,
                          newdata = newobsinternet) %>%
  as_tibble %>%
  set_names(newobscell$names)


# Creating the plot for posterior of cell phone and GDP. Is stat_slab the best
# way to visualize this data? Would histograms that are facet_wrapped be better?

peinternetlong <- peinternet %>%
  pivot_longer(cols = everything(), 
               names_to = "Income Classification", 
               values_to = "Internet Subscription")

peinternetplot <- peinternetlong %>%
  ggplot(aes(x = `Internet Subscription`, 
             y = `Income Classification`)) +
  stat_slab(alpha = 0.3) +
  labs(title = "Proportion of Population with Internet Subscription",
       subtitle = "How GDP predicts Percentage of Internet Subscriptions per 100 People",
       x = "Internet Subscription per 100 people",
       y = "Income Classification",
       caption = "Source: World Bank (2020) and UN (2020)") +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_bw()




