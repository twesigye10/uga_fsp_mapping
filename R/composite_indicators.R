# create composite indicators

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

# read data
df_tool_data <- read_csv("inputs/UGA2103_FSP_Assessment_Raw_data_aug_final.xlsx")

df_tool_data %>% 
  mutate(
    i.number_agents_cat = case_when(
      number_agents == 0 ~ "0",
      number_agents >= 1 &  number_agents <= 10 ~ "1 - 10",
      number_agents >= 11 &  number_agents <= 25 ~ "11 - 25",
      number_agents >= 25 &  number_agents <= 50 ~ "25 - 50",
      number_agents >= 51 &  number_agents <= 100 ~ "51 - 100",
      number_agents >= 101 &  number_agents <= 200 ~ "101 - 200",
      number_agents >= 201 &  number_agents <= 500 ~ "201 - 500",
      number_agents >= 501 &  number_agents <= 998 ~ "500+",
      number_agents == 999 ~ "999",
      number_agents >= 1000  ~ "500+",
    )
  )