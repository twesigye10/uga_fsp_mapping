# create composite indicators

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

# read data
df_tool_data <- read_excel("inputs/UGA2103_FSP_Assessment_Raw_data_aug_final.xlsx")

create_composite_indicators_fsp <- function(input_df) {
  settlement_regroup <- tibble::tribble(
    ~settlement,      ~region,
    "adjumani",  "west nile",
    "bidibidi",  "west nile",
    "imvepi",  "west nile",
    "kiryandongo",  "west nile",
    "kyaka II", "south west",
    "kyangwali", "south west",
    "lobule",  "west nile",
    "nakivale", "south west",
    "oruchinga", "south west",
    "palabek",  "west nile",
    "palorinya",  "west nile",
    "rhino",  "west nile",
    "rwamwanja", "south west"
  )
  
  region_lookup <- setNames(object = settlement_regroup$region, nm = settlement_regroup$settlement)
  
  
  df_data_ci_updated <- input_df %>% 
    mutate(
      i.number_agents_cat = case_when(number_agents == 0 ~ "0",
                                      number_agents >= 1 &  number_agents <= 10 ~ "1_10",
                                      number_agents >= 11 &  number_agents <= 25 ~ "11_25",
                                      number_agents >= 25 &  number_agents <= 50 ~ "25_50",
                                      number_agents >= 51 &  number_agents <= 100 ~ "51_100",
                                      number_agents >= 101 &  number_agents <= 200 ~ "101_200",
                                      number_agents >= 201 &  number_agents <= 500 ~ "201_500",
                                      number_agents >= 501 &  number_agents <= 998 ~ "500+",
                                      number_agents == 999 ~ "999",
                                      number_agents >= 1000  ~ "500 +",
                                      TRUE ~ "NA"
      ),
      i.region = recode(refugee_settlelement, !!!region_lookup),
      i.settlement_presence = case_when(refugee_settle_operate == "no_presence" ~ "no",
                                        refugee_settle_operate %in% c("yes_thr_agent_network",
                                                                      "yes_brach_and_agent",
                                                                      "yes_branch_location",
                                                                      "other") ~ "yes",
                                        TRUE ~ refugee_settle_operate
      ),
      i.operate_presence_cat = case_when(yes_operate_presence == 0 ~ "0",
                                         yes_operate_presence >= 1 & yes_operate_presence <= 10 ~   "1_10",
                                         yes_operate_presence >= 11 & yes_operate_presence <= 20 ~  "11_20",
                                         yes_operate_presence >= 21 & yes_operate_presence <= 30 ~  "21_30",
                                         yes_operate_presence >= 31 & yes_operate_presence <= 40 ~  "31_40",
                                         yes_operate_presence >= 41 & yes_operate_presence <= 50 ~  "41_50",
                                         yes_operate_presence >= 51 & yes_operate_presence <= 75 ~  "51_75",
                                         yes_operate_presence >= 76 & yes_operate_presence <= 100 ~ "76_100",
                                         yes_operate_presence >= 101 & yes_operate_presence <= 998 ~    "100 +",
                                         yes_operate_presence == 999 ~      "999",
                                         yes_operate_presence >= 1000 ~      "100 +",
                                         TRUE ~ "NA"
      ),
      i.transfer_value_cat = case_when(large_prog_transfer_value == 0 ~ "0",
                                       large_prog_transfer_value >= 1 & large_prog_transfer_value <= 998 ~   "1_10000000",
                                       large_prog_transfer_value == 999 ~      "999",
                                       large_prog_transfer_value >= 1000 & large_prog_transfer_value <= 10000000 ~   "1_10000000",
                                       large_prog_transfer_value >= 10000001 & large_prog_transfer_value <= 20000000 ~  "10000001_20000000",
                                       large_prog_transfer_value >= 20000001 & large_prog_transfer_value <= 50000000 ~  "20000001_50000000",
                                       large_prog_transfer_value >= 50000001 & large_prog_transfer_value <= 100000000 ~  "50000001_100000000",
                                       large_prog_transfer_value >= 100000001 & large_prog_transfer_value <= 200000000 ~  "100000001_200000000",
                                       large_prog_transfer_value >= 200000001 & large_prog_transfer_value <= 500000000 ~  "200000001_500000000",
                                       large_prog_transfer_value >= 500000001 & large_prog_transfer_value <= 1000000000 ~ "500000001_1000000000",
                                       large_prog_transfer_value > 1000000000  ~    "1000000000 +",
                                       TRUE ~ "NA"
                                       
      ),
      i.agent_increment_cat = case_when(agent_increment_ability_yes == 0 ~ "0",
                                        agent_increment_ability_yes >= 1 &  agent_increment_ability_yes <= 5 ~ "1_5",
                                        agent_increment_ability_yes >= 6 &  agent_increment_ability_yes <= 10 ~ "6_10",
                                        agent_increment_ability_yes >= 11 &  agent_increment_ability_yes <= 20 ~ "11_20",
                                        agent_increment_ability_yes >= 21 &  agent_increment_ability_yes <= 30 ~ "21_30",
                                        agent_increment_ability_yes >= 31 &  agent_increment_ability_yes <= 50 ~ "31_50",
                                        agent_increment_ability_yes >= 51 &  agent_increment_ability_yes <= 75 ~ "51_75",
                                        agent_increment_ability_yes >= 76 &  agent_increment_ability_yes <= 100 ~ "76_100",
                                        agent_increment_ability_yes >= 101 &  agent_increment_ability_yes <= 998 ~ "100 +",
                                        agent_increment_ability_yes == 999 ~ "999",
                                        agent_increment_ability_yes >= 1000  ~ "100 +",
                                        TRUE ~ "NA"
      )
      
    )
  
  df_data_ci_updated
}



df_updated_data <- create_composite_indicators_fsp(df_tool_data)

df_updated_data %>% 
  tabyl(i.number_agents_cat) 

df_updated_data %>% 
  tabyl(i.region)

df_updated_data %>% 
  tabyl(i.settlement_presence) 

df_updated_data %>% 
  tabyl(i.operate_presence_cat)  

df_updated_data %>% 
  tabyl(i.transfer_value_cat)

df_updated_data %>% 
  tabyl(i.transfer_value_cat)


