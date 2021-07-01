# data checks
library(tidyverse)
library(glue)
library(lubridate)

# rm(list = ls())

# read data ---------------------------------------------------------------
df_tool_data <- read_csv("inputs/UGA2103_Financial_Service_Providers_Assessment_KI_Tool_June2021_2021-06-24.csv")

# Time interval for the survey --------------------------------------------

min_time_of_survey <- 60
max_time_of_survey <- 180

df_check_survey_time <-  df_tool_data %>% 
  mutate(
    i.check.uuid = `_uuid`,
    i.check.today = today,
    i.check.enumerator_id = enumerator_id,
    int.survey_time_interval = difftime(end,start, units = "mins"),
    int.survey_time_interval = round(int.survey_time_interval,2),
    i.check.identified_issue = case_when(
      int.survey_time_interval < min_time_of_survey ~ "less_survey_time",
      int.survey_time_interval > max_time_of_survey ~ "more_survey_time",
      TRUE ~ "normal_survey_time" ),
    i.check.type = NA,
    i.check.name = NA,
    i.check.value = NA,
    i.check.checked_by = "Mathias",
    i.check.checked_date = as_date(today()),
    i.check.comment = NA
  )%>% 
  filter(i.check.identified_issue %in% c("less_survey_time", "more_survey_time"))



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


# time_verify_new_agents --------------------------------------------------

# •	time_verify_new_agents should be flagged IF no response is recorded but skip logic was not activated. AND IF response = >20

df_time_verify_new_agents <- df_tool_data %>% 
  filter(
    time_verify_new_agents >= 20
  ) %>% 
  mutate(
    i.check.uuid = `_uuid`,
    i.check.today = today,
    i.check.enumerator_id = enumerator_id,
    i.check.identified_issue = "value_outside_limits",
    i.check.type = NA,
    i.check.name = "time_verify_new_agents",
    i.check.value = time_verify_new_agents,
    i.check.checked_by = "Mathias",
    i.check.checked_date = as_date(today()),
    i.check.comment = NA
  )

# 
# •	charge_each_transfer should be flagged IF response = >10,000,000  OR “999”  
df_charge_each_transfer <- df_tool_data %>% 
  filter(
    charge_each_transfer == 999 | charge_each_transfer >= 10000000
  ) %>% 
  mutate(
    i.check.uuid = `_uuid`,
    i.check.today = today,
    i.check.enumerator_id = enumerator_id,
    i.check.identified_issue = "value_outside_limits",
    i.check.type = NA,
    i.check.name = "charge_each_transfer",
    i.check.value = charge_each_transfer,
    i.check.checked_by = "Mathias",
    i.check.checked_date = as_date(today()),
    i.check.comment = NA
  )

# •	fixed_fee should be flagged IF response = > 10,000  OR “999”
df_fixed_fee <- df_tool_data %>% 
  filter(
    fixed_fee == 999 | fixed_fee >= 10000
  ) %>% 
  mutate(
    i.check.uuid = `_uuid`,
    i.check.today = today,
    i.check.enumerator_id = enumerator_id,
    i.check.identified_issue = "value_outside_limits",
    i.check.type = NA,
    i.check.name = "fixed_fee",
    i.check.value = fixed_fee,
    i.check.checked_by = "Mathias",
    i.check.checked_date = as_date(today()),
    i.check.comment = NA
  )
# •	withdraw_fixed_fee_amount should be flagged IF response = > 10,000 OR “999”
df_withdraw_fixed_fee_amount <- df_tool_data %>% 
  filter(
    withdraw_fixed_fee_amount == 999 | withdraw_fixed_fee_amount >= 10000
  ) %>% 
  mutate(
    i.check.uuid = `_uuid`,
    i.check.today = today,
    i.check.enumerator_id = enumerator_id,
    i.check.identified_issue = "value_outside_limits",
    i.check.type = NA,
    i.check.name = "withdraw_fixed_fee_amount",
    i.check.value = withdraw_fixed_fee_amount,
    i.check.checked_by = "Mathias",
    i.check.checked_date = as_date(today()),
    i.check.comment = NA
  )
# •	perc_value_delivered should be flagged IF type_FSP = banking institution AND decimal =  >2 
# •	perc_value_delivered should be flagged IF response = > 10    OR “999”
df_perc_value_delivered <- df_tool_data %>% 
  filter(
    (type_FSP == "banking institution" & decimal >= 2) | (perc_value_delivered == 999 | perc_value_delivered >= 10)
  ) %>% 
  mutate(
    i.check.uuid = `_uuid`,
    i.check.today = today,
    i.check.enumerator_id = enumerator_id,
    i.check.identified_issue = "value_outside_limits",
    i.check.type = NA,
    i.check.name = "perc_value_delivered",
    i.check.value = perc_value_delivered,
    i.check.checked_by = "Mathias",
    i.check.checked_date = as_date(today()),
    i.check.comment = NA
  )

# •	perc_value_withdraw should be flagged IF response = > 10 OR “999” 
df_perc_value_withdraw <- df_tool_data %>% 
  filter(
    perc_value_withdraw == 999 | perc_value_withdraw >= 10
  ) %>% 
  mutate(
    i.check.uuid = `_uuid`,
    i.check.today = today,
    i.check.enumerator_id = enumerator_id,
    i.check.identified_issue = "value_outside_limits",
    i.check.type = NA,
    i.check.name = "perc_value_withdraw",
    i.check.value = perc_value_withdraw,
    i.check.checked_by = "Mathias",
    i.check.checked_date = as_date(today()),
    i.check.comment = NA
  )

# •	number_agents should be flagged IF response = “999”
df_number_agents <- df_tool_data %>% 
  filter(
    number_agents == 999
  ) %>% 
  mutate(
    i.check.uuid = `_uuid`,
    i.check.today = today,
    i.check.enumerator_id = enumerator_id,
    i.check.identified_issue = "value_outside_limits",
    i.check.type = NA,
    i.check.name = "number_agents",
    i.check.value = number_agents,
    i.check.checked_by = "Mathias",
    i.check.checked_date = as_date(today()),
    i.check.comment = NA
  )
# •	yes_operate_presence cannot be larger number than number_agents
df_yes_operate_presence <- df_tool_data %>% 
  filter(
    yes_operate_presence > number_agents
  ) %>% 
  mutate(
    i.check.uuid = `_uuid`,
    i.check.today = today,
    i.check.enumerator_id = enumerator_id,
    i.check.identified_issue = "value_outside_limits",
    i.check.type = NA,
    i.check.name = "yes_operate_presence",
    i.check.value = yes_operate_presence,
    i.check.checked_by = "Mathias",
    i.check.checked_date = as_date(today()),
    i.check.comment = NA
  )
# •	records_kept response should be changed to “all_above” IF “withdrawal” AND “deposit” AND “cash_transfer” are all selected. 
df_records_kept <- df_tool_data %>% 
  filter(
    grepl("withdrawal", records_kept, ignore.case=TRUE) & grepl("deposit", records_kept, ignore.case=TRUE) & grepl("cash_transfer", records_kept, ignore.case=TRUE) 
  ) %>% 
  mutate(
    i.check.uuid = `_uuid`,
    i.check.today = today,
    i.check.enumerator_id = enumerator_id,
    i.check.identified_issue = "response to be changed to all_above",
    i.check.type = NA,
    i.check.name = "records_kept",
    i.check.value = records_kept,
    i.check.checked_by = "Mathias",
    i.check.checked_date = as_date(today()),
    i.check.comment = NA
  )
# •	monitoring_agent_transparency should be flagged IF response “not_applicable”  Does the organization use agents? Then not applicable should not be answered here. 
df_monitoring_agent_transparency <- df_tool_data %>% 
  filter(
    monitoring_agent_transparency == "not_applicable" & number_agents > 0
  ) %>% 
  mutate(
    i.check.uuid = `_uuid`,
    i.check.today = today,
    i.check.enumerator_id = enumerator_id,
    i.check.identified_issue = "questionable response",
    i.check.type = NA,
    i.check.name = "monitoring_agent_transparency",
    i.check.value = monitoring_agent_transparency,
    i.check.checked_by = "Mathias",
    i.check.checked_date = as_date(today()),
    i.check.comment = NA
  )

