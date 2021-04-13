library(tidyverse)
#library(tidycensus)
library(shinythemes)
library(shiny)



child_mortality <- read_csv(file = "child_mortality_0_5_year_olds_dying_per_1000_born.csv",
                            col_types = cols(
                              .default = col_double(),
                              country = col_character()
                            )
                            )

child_mortality_tbl <- as.tibble(child_mortality) %>%
  select("country", "2000", "2020") %>%
  slice(1:10) %>%
  ggplot(mapping = aes(x = country, 
                       y = `2020`)) +
  geom_point()

child_mortality_tbl

mean_child_mortality <- as_tibble(child_mortality) %>%
  rowwise() %>%
  mutate(mean = mean(c_across(`1800`:`2020`))) %>%
  select("country", "mean") %>%
  slice(1:10) %>%
  ggplot(mapping = aes(x = country, 
                       y = `2020`)) +
  geom_point()

ggsave("child_mortality.png", child_mortality_tbl)


# health_spending <- read_csv(file = "final_project/child_mortality_0_5_year_olds_dying_per_1000_born.csv",
#                             col_types = cols(
#                               country = col_character(),
#                               `1995` = col_double(),
#                               `1996` = col_double(),
#                               `1997` = col_double(),
#                               `1998` = col_double(),
#                               `1999` = col_double(),
#                               `2000` = col_double(),
#                               `2001` = col_double(),
#                               `2002` = col_double(),
#                               `2003` = col_double(),
#                               `2004` = col_double(),
#                               `2005` = col_double(),
#                               `2006` = col_double(),
#                               `2007` = col_double(),
#                               `2008` = col_double(),
#                               `2009` = col_double(),
#                               `2010` = col_double()
#                             ))
# 
# health_spending

