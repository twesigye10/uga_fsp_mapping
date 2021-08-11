library(tidyverse)
library(readxl)
library(janitor)

# read in data
df_cleaning_log <- read_excel("inputs/FSP_KII_cleaning_log_final10082021.xlsx", sheet = "log") %>% 
  remove_empty("rows")

df_survey <- read_excel("inputs/UGA2103_FSPA_KI_Tool_June.xlsx", sheet = "survey") 
df_choices <- read_excel("inputs/UGA2103_FSPA_KI_Tool_June.xlsx", sheet = "choices") 

# check if the name exists in the survey questionnaire

# check if all types of change are the expected ones

# check if the type of change and values are consistent with the type of original question
# select_one, select_multiple



# check if the value is consistent with other values in that question


# create final cleaning log

