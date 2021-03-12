library(tidyverse)
#library(tidycensus)
library(shinythemes)
library(shiny)

# racevars <- c(White = "B02001_002", 
#               Black = "B02001_003", 
#               Asian = "B02001_005",
#               Hispanic = "B03003_003")
# harris <- get_acs(geography = "tract",
#                   variables = racevars, 
#                   year = 2018,
#                   state = "TX",
#                   county = "Harris County",
#                   geometry = TRUE,
#                   summary_var = "B02001_001") 
# 
# map <- harris %>%
#   mutate(Percent = 100 * (estimate / summary_est)) %>%
#   ggplot(aes(fill = Percent, color = Percent)) +
#   facet_wrap(~ variable) +
#   geom_sf() +
#   scale_fill_viridis_c(direction = -1) +
#   scale_color_viridis_c(direction = -1) +
#   labs(title = "Racial geography of Harris County, Texas",
#        caption = "Source: American Community Survey 2014-2018") +
#   theme_void()
# 
# ggsave("map.png", map)
# 
# fairfax <- get_acs(geography = "tract",
#                    variables = racevars, 
#                    year = 2018,
#                    state = "VA",
#                    county = "Fairfax County",
#                    geometry = TRUE,
#                    summary_var = "B02001_001") 
# 
# map_2 <- fairfax %>%
#   mutate(Percent = 100 * (estimate / summary_est)) %>%
#   ggplot(aes(fill = Percent, color = Percent)) +
#   facet_wrap(~ variable) +
#   geom_sf() +
#   scale_fill_viridis_c(direction = -1) +
#   scale_color_viridis_c(direction = -1) +
#   labs(title = "Racial geography of Fairfax County, Virginia",
#        caption = "Source: American Community Survey 2014-2018") +
#   theme_void()
# 
# ggsave("map_2.png", map_2)

child_mortality <- read_csv(file = "child_mortality_0_5_year_olds_dying_per_1000_born.csv",
                            col_types = cols(
                              .default = col_double(),
                              country = col_character()
                            )
                            )

child_mortality_tbl <- tibble(child_mortality) %>%
  select("country", "2000", "2020") %>%
  slice(1:10) %>%
  ggplot(mapping = aes(x = country, 
                       y = `2020`)) +
  geom_point()

child_mortality_tbl
ggsave("child_mortality.png", child_mortality_tbl)


health_spending <- read_csv(file = "total_health_spending_per_person_us.csv",
                            col_types = cols(
                              country = col_character(),
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
                              `2010` = col_double()
                            ))

