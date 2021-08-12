library(tidyverse)
library(readxl)
library(janitor)

# read in data
df_cleaning_log <- read_excel("inputs/FSP_KII_cleaning_log_final12082021.xlsx", sheet = "log") %>% 
  remove_empty("rows")

df_cl_survey <- read_excel("inputs/UGA2103_FSP_Tool_June2021_Final_2021_08_12.xlsx", sheet = "survey") 
df_cl_choices <- read_excel("inputs/UGA2103_FSP_Tool_June2021_Final_2021_08_12.xlsx", sheet = "choices") 


# gather choice options based on unique choices list ----------------------

df_grouped_choices<- df_cl_choices %>% 
  group_by(list_name) %>% 
  summarise(choice_options = paste(name, collapse = " : "))

# check if the name exists in the survey questionnaire --------------------

questionnaire_names <- df_cl_survey %>% 
  filter(str_length(name) > 2) %>% 
  pull(name) %>% 
  unique()

df_cl_name_not_in_svyr <- df_cleaning_log %>% 
  filter(!name %in% questionnaire_names) %>% 
  mutate(
    identified_issue_for_final_log = "name not_in_survey"
  )


# check if all types of change are the expected ones ----------------------

df_cl_check_type <- df_cleaning_log %>%
  left_join(df_cl_survey, by = "name") %>% 
  mutate(qn_type = str_extract(type.y, pattern = "^[a-zA-Z\\_]*"))
# none select_multiple and not having "change_response" as the type of change
df_cl_type_check_none_sm <- df_cl_check_type %>% 
  filter(qn_type != "select_multiple", `type.x` != "change_response") %>% 
  mutate(
    identified_issue_for_final_log = "type not for this type of question "
  )
# select_multiple and having "change_response" as the type of change
df_cl_type_check_sm <- df_cl_check_type %>% 
  filter(qn_type == "select_multiple", `type.x` == "change_response") %>% 
  mutate(
    identified_issue_for_final_log = "type not for this type of question "
  )


# check if the values are consistent --------------------------------------

# option exists, check provided value in the log
df_cl_value_check_option_exists_so_sm <- df_cl_check_type %>% 
  filter(qn_type %in% c("select_one", "select_multiple"), 
         Issue %in% c("option_exists", "wrong name", "Not filled", "recode other",
                      "Recode other", "wrong name",
                      "wrong entry", "Respondent not based in any district?", "not clear", "information needed")) %>% 
  separate(col = type.y, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" ) %>% 
  left_join(df_grouped_choices, by = "list_name") %>% 
  filter(!str_detect(string = choice_options, pattern = value)) %>% 
  mutate(
    identified_issue_for_final_log = "suggested option not in the tool"
  )

# select_one, select_multiple
df_cl_value_check_so_sm <- df_cl_check_type %>% 
  filter(qn_type %in% c("select_one", "select_multiple"), str_detect(string = value, pattern = "[ ]|[:upper:]"))

# integer
df_cl_value_check_int <- df_cl_check_type %>% 
  filter(qn_type == "integer", str_detect(string = value, pattern = "\\D") )

# create final cleaning log

