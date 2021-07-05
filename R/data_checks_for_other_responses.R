# data checks
library(tidyverse)
library(glue)
# library(koboAPI)
library(butteR)
# library(koboloadeR)


# read data ---------------------------------------------------------------
df_tool_data <- read_csv("inputs/UGA2103_Financial_Service_Providers_Assessment_KI_Tool_June2021.csv")

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


# add choices to the data -------------------------------------------------

# gather choice options based on unique choices list
df_unique_choices <- df_choices %>% 
  pull(list_name) %>% unique()

df_grouped_choices <- data.frame()

for (vl in df_unique_choices) {
  current_data <- df_choices %>% 
    filter(list_name == vl) %>% pull(name) %>% str_c(collapse = " : ")
  df_grouped_choices <- rbind(df_grouped_choices, data.frame(list_name=vl, choice_options = current_data))
}

# Option 2:
df_grouped_choices_2 <- df_choices %>% 
  group_by(list_name) %>% 
  summarise(choice_options = paste(name, collapse = " : "))

# extract parent question and join survey for extracting list_name
df_data_parent_qns <- df_data_arranged %>% 
  mutate(
    parent_qn = str_replace_all(name, "/.*", ""),
    parent_qn = str_replace_all(parent_qn, "_other", "")
  ) %>% 
  left_join(df_survey %>% select(name, type), by = c("parent_qn"="name")) %>% 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# join other responses with choice options based on list_name

df_join_other_response_with_choices <- df_data_parent_qns %>% 
  left_join(df_grouped_choices, by = "list_name")

# output the resulting data frame
write_csv(x = df_join_other_response_with_choices, file = paste0("outputs/others_responses_",as_date(today()),"_", hour(now()) ,".csv"), na = "")