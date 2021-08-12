library(tidyverse)
library(readxl)
library(janitor)

# read in data
df_cleaning_log <- read_excel("inputs/FSP_KII_cleaning_log_final10082021.xlsx", sheet = "log") %>% 
  remove_empty("rows")

df_cl_survey <- read_excel("inputs/UGA2103_FSPA_KI_Tool_June.xlsx", sheet = "survey") 
df_cl_choices <- read_excel("inputs/UGA2103_FSPA_KI_Tool_June.xlsx", sheet = "choices") 

# check if the name exists in the survey questionnaire

questionnaire_names <- df_cl_survey %>% 
  filter(str_length(name) > 2) %>% 
  pull(name) %>% 
  unique()

cl_name_not_in_svyr <- df_cleaning_log %>% 
  filter(!name %in% questionnaire_names)


# check if all types of change are the expected ones ----------------------

df_cl_check_type <- df_cleaning_log %>%
  left_join(df_cl_survey, by = "name") %>% 
  mutate(qn_type = str_extract(type.y, pattern = "^[a-zA-Z\\_]*"))
# none select_multiple and not having "change_response" as the type of change
type_check_none_sm <- df_cl_check_type %>% 
  filter(qn_type != "select_multiple", `type.x` != "change_response") 
# select_multiple and having "change_response" as the type of change
type_check_sm <- df_cl_check_type %>% 
  filter(qn_type == "select_multiple", `type.x` == "change_response")

# check if the type of change and values are consistent with the type of original question
# select_one, select_multiple



# check if the value is consistent with other values in that question


# create final cleaning log

