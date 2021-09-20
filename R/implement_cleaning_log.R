# implement cleaning log

library(tidyverse)
library(lubridate)

source("R/composite_indicators.R")

# read the data
df_cleaning_log <- readxl::read_excel("inputs/FSP_KII_cleaning_log_final13082021.xlsx") %>% 
  rename(issue_id = Issue, issue = comment) %>% 
  mutate(issue_id = str_replace_all(string = issue_id, pattern = " ", replacement = "_"), sheet = NA, index = NA, relevant = NA) %>% 
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)
df_raw_data <- readxl::read_excel("inputs/UGA2103_FSP_Assessment_Raw_data_aug_final.xlsx") %>% 
  select(-contains("cash_transfer_limitation")) %>% 
  mutate(fsps_contact_inform = ifelse(fsps_contact_inform == "0", "999", fsps_contact_inform), across(.cols = everything(), .fns = ~ifelse(. == "N/A", "NA", .)))

df_survey <- readxl::read_excel("inputs/UGA2103_FSP_Tool_June2021_Final_2021_08_20.xlsx", sheet = "survey") 
df_choices <- readxl::read_excel("inputs/UGA2103_FSP_Tool_June2021_Final_2021_08_20.xlsx", sheet = "choices") 

# find all new choices to add to choices sheet ----------------------------

# gather choice options based on unique choices list
df_grouped_choices<- df_choices %>% 
  group_by(list_name) %>% 
  summarise(choice_options = paste(name, collapse = " : "))
# get new name and ad_option pairs to add to the choices sheet
new_vars <- df_cleaning_log %>% 
  filter(type %in% c("change_response", "add_option")) %>% 
  left_join(df_survey, by = "name") %>% 
  filter(str_detect(string = type.y, pattern = "select_one|select one|select_multiple|select multiple")) %>% 
  separate(col = type.y, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>% 
  left_join(df_grouped_choices, by = "list_name") %>%
  filter(!str_detect(string = choice_options, pattern = value ) ) %>%
  rename(choice = value ) %>%
  select(name, choice) %>%
  distinct() %>% # to make sure there are no duplicates
  arrange(name)


# create kobold object ----------------------------------------------------

kbo <- kobold::kobold(survey = df_survey, 
                      choices = df_choices, 
                      data = df_raw_data, 
                      cleaning = df_cleaning_log)

# modified choices for the survey tool --------------------------------------
df_choises_modified <- butteR:::xlsform_add_choices(kobold = kbo, new_choices = new_vars)

# special treat for variables for select_multiple, we need to add the columns to the data itself
df_survey_sm <- df_survey %>% 
  mutate(q_type = case_when(str_detect(string = type, pattern = "select_multiple|select multiple") ~ "sm",
                            str_detect(string = type, pattern = "select_one|select one") ~ "so",
                            TRUE ~ type)) %>% 
  select(name, q_type)
# construct new columns for select multiple
new_vars_sm <- new_vars %>% 
  left_join(df_survey_sm, by = "name") %>% 
  filter(q_type=="sm") %>% 
  mutate(new_cols=paste0(name,"/",choice))


# add new columns to the raw data -----------------------------------------

df_raw_data_modified <- df_raw_data %>% 
  butteR:::mutate_batch(nm = new_vars_sm$new_cols, value = F )

# make some cleanup -------------------------------------------------------
kbo_modified <- kobold::kobold(survey = df_survey %>% filter(name %in% colnames(df_raw_data_modified)), 
                               choices = df_choises_modified, 
                               data = df_raw_data_modified, 
                               cleaning = df_cleaning_log )
kbo_cleaned <- kobold::kobold_cleaner(kbo_modified)


# handling added responses after starting data collection -----------------

df_final_cleaned_data <- kbo_cleaned$data %>% 
  mutate(across(.cols = contains("/"), .fns = ~ifelse(is.na(.), FALSE, .)))

# write final modified data -----------------------------------------------------

write_csv(df_final_cleaned_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_data.csv"))


df_with_composites <- create_composite_indicators_fsp(input_df = df_final_cleaned_data)

write_csv(df_with_composites, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_data_with_composite_indicators.csv"))
