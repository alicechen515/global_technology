library(tidyverse)
#library(tidycensus)
library(shinythemes)
library(shiny)



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

read_csv(file = "raw_data/six_vacc_rate.csv")


cellphones_100 <- read_csv(file = "raw_data/cell_phones_per_100_people.csv"
                            )

as_tibble(cell_phones_100)




