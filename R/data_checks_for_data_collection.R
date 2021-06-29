# data checks
library(tidyverse)
library(glue)
library(lubridate)
library(koboAPI)
# library(koboloadeR)


source("support_data/credentials.R")

u<-account
pw<-password
form_id<-"816981"

# use kobo api to get data ------------------------------------------------
# dataset
my_data <- koboAPI::download_data(formid = form_id, user=u, pwd = pw)
v# tool itself
tool <- download_form(formid = form_id, user=u, pwd = pw)
# create kobold object
kbo <- kobold::kobold(survey = tool$survey, choices = tool$choices, data = my_data)
kbo_labelled <- kobold::choices = choice_names_to_labels(object = kbo)
# 
# 
# 
# 
# # koboloadeR has notb worked --------------------------------------------------------------
# # auth_str <- paste0(u, ":", pw)
# # data_with_koboloadeR <- kobo_data_downloader(formid = 816981, user = auth_str, api = "kobohr")
# 
# 
# # New function ------------------------------------------------------------
# 
# # Downlaoding data from kobo
# 
# kobohr_getdata_csv<-function(url,u,pw){
#   #supply url for the data
#   rawdata<-GET(url,authenticate(u,pw),progress())
#   d_content <- read_csv(content(rawdata,"raw",encoding = "UTF-8"))
# }
# 
# download_data <- function(dataurl,kobo_user,kobo_pw) {
#   d_raw <-  kobohr_getdata_csv(dataurl,kobo_user,kobo_pw) 
#   return( as.data.frame(d_raw))
# }
# 
# kobo_user <-  u
# kobo_pw <- pw
# form_id <-  form_id
# 
# dataurl<- paste0("https://kc.humanitarianresponse.info/api/v1/data/",form_id,".csv")
# 
# df <- download_data(dataurl,kobo_user,kobo_pw)
# colnames(df) <- gsub('^(?:[^/]*/)*(.*)', '\\1', colnames(df))
# 
# my_kobo_data <- df

# read data ---------------------------------------------------------------
df_tool_data <- read_csv("inputs/UGA2103_Financial_Service_Providers_Assessment_KI_Tool_June2021_2021-06-24.csv")


# Time interval for the survey --------------------------------------------

min_time_of_survey <- 60
max_time_of_survey <- 180

df_check_survey_time <-  df_tool_data %>% 
  mutate(
    i.check.survey_time_interval = difftime(end,start, units = "mins"),
    i.check.survey_time_interval = round(i.check.survey_time_interval,2),
    i.check.found_issue = case_when(
      i.check.survey_time_interval < min_time_of_survey ~ "less_survey_time",
      i.check.survey_time_interval > max_time_of_survey ~ "more_survey_time",
      TRUE ~ "normal_survey_time"
    )
)

df_check_survey_time %>% select(starts_with("i.check"))


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
    mutate( name = cln)
  df_other_response_data <- rbind(df_other_response_data, df_filtered_data)
}
# arrange the data
df_data_arranged <- df_other_response_data %>% 
  arrange(today, `_uuid`)

write_csv(x = df_data_arranged, file = paste0("outputs/others_responses_",as_date(today()),"_", hour(now()) ,".csv"))


# time_verify_new_agents --------------------------------------------------

# •	time_verify_new_agents should be flagged IF no response is recorded but skip logic was not activated. AND IF response = >20


# •	perc_value_delivered should be flagged IF type_FSP = banking institution AND decimal =  >2 
# 
# •	charge_each_transfer should be flagged IF response = >10,000,000  OR “999”  
# 
# •	fixed_fee should be flagged IF response = > 10,000  OR “999”
# 
# •	withdraw_fixed_fee_amount should be flagged IF response = > 10,000 OR “999”
# 
# •	perc_value_delivered should be flagged IF response = > 10    OR “999”
# 
# •	perc_value_withdraw should be flagged IF response = > 10 OR “999” 
# 
# •	number_agents should be flagged IF response = “999”
# 
# •	yes_operate_presence cannot be larger number than number_agents
# 
# •	records_kept response should be changed to “all_above” IF “withdrawal” AND “deposit” AND “cash_transfer” are all selected. 
# 
# •	monitoring_agent_transparency should be flagged IF response “not_applicable”  Does the organization use agents? Then not applicable should not be answered here. 


