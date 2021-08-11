library(tidyverse)
library(readxl)
library(janitor)

# read the log file
df_cleaning_log <- read_excel("inputs/FSP_KII_cleaning_log_final10082021.xlsx", sheet = "log") %>% 
  remove_empty("rows")

# check if the name exists in the survey questionnaire

# check if all types of change are the expected ones

# check if the type of change and values are consistent with the type of original question
# select_one, select_multiple



# check if the value is consistent with other values in that question


# create final cleaning log

