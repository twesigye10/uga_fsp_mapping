library(tidyverse)
library(janitor)
library(glue)

source("R/composite_indicators.R")


# load data ---------------------------------------------------------------

df_cleaned <- read_csv("outputs/20210820_clean_data.csv")

dap <- read_csv("inputs/r_dap.csv") %>% 
  janitor::clean_names()