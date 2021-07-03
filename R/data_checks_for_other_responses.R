# data checks
library(tidyverse)
library(glue)
# library(koboAPI)
library(butteR)
# library(koboloadeR)


# read data ---------------------------------------------------------------
df_tool_data <- read_csv("inputs/UGA2103_Financial_Service_Providers_Assessment_KI_Tool_June2021_2021-06-24.csv")

df_survey <- read_csv("inputs/survey.csv")

df_choices <- read_csv("inputs/choices.csv")

# All “other” responses should be flagged ---------------------------------
# - identify them
# - extract them and their responses
# - add column for re-categorization
# - Keep important columns to help you take them back
# - have some kind of identifier

# get questions with other
others_colnames <-  df_tool_data %>% 
  select(
    ends_with("_other")
  ) %>% colnames()

# data.frame for holding _other response data
df_other_response_data <- data.frame()

for (cln in others_colnames) {
  df_filtered_data <- df_tool_data %>% 
    select("_uuid", "today", "enumerator_id", current_value = cln) %>% 
    filter(!is.na(current_value)) %>% 
    mutate( name = cln, appropriate_choice = NA)
  df_other_response_data <- rbind(df_other_response_data, df_filtered_data)
}
# arrange the data
df_data_arranged <- df_other_response_data %>% 
  arrange(today, `_uuid`)


write_csv(x = df_data_arranged, file = paste0("outputs/others_responses_",as_date(today()),"_", hour(now()) ,".csv"), na = "")


# add choices to the data -------------------------------------------------

# attach choices to the survey
df_unique_choices <- df_choices %>% 
  pull(list_name) %>% unique()

df_grouped_choices <- data.frame()

for (vl in df_unique_choices) {
  current_data <- df_choices %>% 
    filter(list_name == vl) %>% pull(name) %>% str_c(collapse = " : ")
  df_grouped_choices <- rbind(df_grouped_choices, data.frame(list_name=vl, choice_options = current_data))
}

# extract parent question

df_data_parent_qns <- df_data_arranged %>% 
  mutate(
    parent_qn = str_replace_all(name, "/.*", ""),
    parent_qn = str_replace_all(parent_qn, "_other", "")
  ) %>% 
  left_join(df_survey %>% select(name, type), by = c("parent_qn"="name")) %>% 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# make a join or do a lookup

df_join_other_response_with_choices <- df_data_parent_qns %>% 
  left_join(df_grouped_choices, by = "list_name")


# butteR::auto_detect_sm_parents(df_tool_data)
# ?auto_detect_sm_parents
# butteR::check_others(df = df_tool_data, suffix = "_other", report_cols = c("_uuid") )
# butteR::read_all_csvs_in_folder()
# 
# extract_sm_other_responses <- function(.x) {
#   .x[!is.na(.x)]
# }
# 
# df_check_other_responses <-  df_tool_data %>% 
#   select(
#     ends_with("_other")
#   )
# 
# map(.x = df_check_other_responses, .f = extract_sm_other_responses)
# 
# map_dfr(.x = df_check_other_responses, .f = extract_sm_other_responses)

