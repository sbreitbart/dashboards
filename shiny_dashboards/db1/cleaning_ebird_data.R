# load libraries

library(here)
library(tidyverse)
library(janitor)

# import data
ebird <- read.csv(here::here("./shiny_dashboards/db1/MyEBirdData.csv"),
                 header=T, na.strings=c("","NA")) %>%
  
# clean it
  # get rid of spaces, slashes, other bad stuff in colnames
  janitor::clean_names() %>%
  # convert "X"s to NAs in bird counts
  dplyr::mutate(count = na_if(count, "X"))

