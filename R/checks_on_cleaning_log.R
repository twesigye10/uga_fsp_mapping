library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

# read in data
df_cleaning_log <- read_excel("inputs/FSP_KII_cleaning_log_final12082021.xlsx", sheet = "log") %>% 
  remove_empty("rows") %>% 
  # mutate(Date = lubridate::as_date(Date) )
  rename_with(.fn = ~paste0("i.", .x))

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
  filter(!i.name %in% questionnaire_names) %>% 
  mutate(
    i.identified_issue_for_final_log = "name not_in_survey"
  ) %>% 
  select(starts_with("i."))


# check if all types of change are the expected ones ----------------------

df_cl_check_type <- df_cleaning_log %>%
  left_join(df_cl_survey, by = c("i.name" = "name")) %>% 
  mutate(int.qn_type = str_extract(type, pattern = "^[a-zA-Z\\_]*"))
# none select_multiple and not having "change_response" as the type of change
df_cl_type_check_none_sm <- df_cl_check_type %>% 
  filter(int.qn_type != "select_multiple", i.type != "change_response") %>% 
  mutate(
    i.identified_issue_for_final_log = "type not for this type of question "
  )%>% 
  select(starts_with("i."))
# select_multiple and having "change_response" as the type of change
df_cl_type_check_sm <- df_cl_check_type %>% 
  filter(int.qn_type == "select_multiple", i.type == "change_response") %>% 
  mutate(
    i.identified_issue_for_final_log = "type not for this type of question "
  )%>% 
  select(starts_with("i."))


# check if the values are consistent --------------------------------------

# option exists, check provided value in the log
df_cl_value_check_option_exists_so_sm <- df_cl_check_type %>% 
  filter(int.qn_type %in% c("select_one", "select_multiple"), 
         i.Issue %in% c("option_exists", "wrong name", "Not filled", "recode other",
                      "Recode other", "wrong name",
                      "wrong entry", "Respondent not based in any district?", "not clear", "information needed")) %>% 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" ) %>% 
  left_join(df_grouped_choices, by = "list_name") %>%
  filter(!str_detect(string = choice_options, pattern = i.value ) ) %>%
  mutate(
    i.identified_issue_for_final_log = "suggested option not in the tool or in wrong case"
  )%>%
  select(starts_with("i."))

# # select_one, select_multiple
# df_cl_value_check_so_sm <- df_cl_check_type %>% 
#   filter(int.qn_type %in% c("select_one", "select_multiple"), str_detect(string = value, pattern = "[ ]|[:upper:]"))

# integer
df_cl_value_check_int <- df_cl_check_type %>% 
  filter(int.qn_type == "integer", str_detect(string = i.value, pattern = "\\D") ) %>% 
  mutate(
    i.identified_issue_for_final_log = "value does not correspond to integer qn type"
  )%>% 
  select(starts_with("i."))

# create final cleaning log

df_cl_issues_combined <- bind_rows(
  df_cl_name_not_in_svyr, df_cl_type_check_none_sm, df_cl_type_check_sm, 
  df_cl_value_check_option_exists_so_sm, df_cl_value_check_int
) %>% 
  rename_with(~str_replace(.x, pattern = "i.", replacement = ""))

write_csv(df_cl_issues_combined, file = paste0("outputs/cleaning_log_issues_to_be_addressed_",as_date(today()),"_", hour(now()) ,".csv"))
