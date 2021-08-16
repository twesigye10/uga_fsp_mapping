# implement cleaning log

library(tidyverse)
library(lubridate)

# read the data
df_cleaning_log <- readxl::read_excel("inputs/FSP_KII_cleaning_log_final13082021.xlsx")
df_raw_data <- readxl::read_excel("inputs/UGA2103_FSP_Assessment_Raw_data_aug_final.xlsx")
df_survey <- readxl::read_excel("inputs/UGA2103_FSP_Tool_June2021_Final_2021_08_12.xlsx", sheet = "survey") 
df_choices <- readxl::read_excel("inputs/UGA2103_FSP_Tool_June2021_Final_2021_08_12.xlsx", sheet = "choices") 

# find all new choices to add to choices sheet ----------------------------


# add new choices to the survey tool --------------------------------------


# modify the data using the cleaning log ----------------------------------


# make some cleanup -------------------------------------------------------


# write modified data -----------------------------------------------------


