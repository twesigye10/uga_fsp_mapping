# implement cleaning log

library(tidyverse)
library(lubridate)

# read the data
df_cleaning_log <- readxl::read_excel("inputs/FSP_KII_cleaning_log_final13082021.xlsx")
df_raw_data <- readxl::read_excel("inputs/UGA2103_FSP_Assessment_Raw_data_aug_final.xlsx")
df_survey <- readxl::read_excel("inputs/UGA2103_FSP_Tool_June2021_Final_2021_08_12.xlsx", sheet = "survey") 
df_choices <- readxl::read_excel("inputs/UGA2103_FSP_Tool_June2021_Final_2021_08_12.xlsx", sheet = "choices") 

# find all new choices to add to choices sheet ----------------------------

# gather choice options based on unique choices list
df_grouped_choices<- df_choices %>% 
  group_by(list_name) %>% 
  summarise(choice_options = paste(name, collapse = " : "))
# get new name and ad_option pairs to add to the choices sheet
new_vars <- df_cleaning_log %>% 
  filter(type == "add_option") %>% 
  left_join(df_survey, by = "name") %>% 
  separate(col = type.y, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>% 
  left_join(df_grouped_choices, by = "list_name") %>%
  filter(!str_detect(string = choice_options, pattern = value ) ) %>% 
  rename(choice = value ) %>% 
  select(name, choice) %>% 
  distinct() %>% # to make sure there are no duplicates
  arrange(name)

# add new choices to the survey tool --------------------------------------


# modify the data using the cleaning log ----------------------------------


# make some cleanup -------------------------------------------------------


# write modified data -----------------------------------------------------


