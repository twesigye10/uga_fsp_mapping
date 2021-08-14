# create composite indicators

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

# read data
df_tool_data <- read_excel("inputs/UGA2103_FSP_Assessment_Raw_data_aug_final.xlsx")

settlement_regroup <- tibble::tribble(
  ~settlement,      ~region,
  "adjumani",  "west nile",
  "bidibidi",  "west nile",
  "imvepi",  "west nile",
  "kiryandongo",  "west nile",
  "kyaka", "south west",
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


df_tool_data <- df_tool_data %>% 
  mutate(
    i.number_agents_cat = case_when(number_agents == 0 ~ "0",
                                    number_agents >= 1 &  number_agents <= 10 ~ "1 - 10",
                                    number_agents >= 11 &  number_agents <= 25 ~ "11 - 25",
                                    number_agents >= 25 &  number_agents <= 50 ~ "25 - 50",
                                    number_agents >= 51 &  number_agents <= 100 ~ "51 - 100",
                                    number_agents >= 101 &  number_agents <= 200 ~ "101 - 200",
                                    number_agents >= 201 &  number_agents <= 500 ~ "201 - 500",
                                    number_agents >= 501 &  number_agents <= 998 ~ "500+",
                                    number_agents == 999 ~ "999",
                                    number_agents >= 1000  ~ "500+",
                                    TRUE ~ "999"
    ),
    i.region = recode(refugee_settlelement, !!!region_lookup),
    i.operate_presence_cat = case_when(yes_operate_presence == 0 ~ "0",
                                       yes_operate_presence >= 1 & yes_operate_presence <= 10 ~   "1 - 10",
                                       yes_operate_presence >= 11 & yes_operate_presence <= 20 ~  "11 - 20",
                                       yes_operate_presence >= 21 & yes_operate_presence <= 30 ~  "21 - 30",
                                       yes_operate_presence >= 31 & yes_operate_presence <= 40 ~  "31 - 40",
                                       yes_operate_presence >= 41 & yes_operate_presence <= 50 ~  "41 - 50",
                                       yes_operate_presence >= 51 & yes_operate_presence <= 75 ~  "51 - 75",
                                       yes_operate_presence >= 76 & yes_operate_presence <= 100 ~ "76 - 100",
                                       yes_operate_presence >= 101 & yes_operate_presence <= 998 ~    "100 +",
                                       yes_operate_presence == 999 ~      "999",
                                       yes_operate_presence >= 1000 ~      "100 +",
                                       TRUE ~ "999"
    ),
    i.transfer_value_cat = case_when(large_prog_transfer_value == 0 ~ "0",
                                     large_prog_transfer_value >= 1 & large_prog_transfer_value <= 998 ~   "1 - 10000000",
                                     large_prog_transfer_value == 999 ~      "999",
                                     large_prog_transfer_value >= 1000 & large_prog_transfer_value <= 10000000 ~   "1 - 10000000",
                                     large_prog_transfer_value >= 10000001 & large_prog_transfer_value <= 20000000 ~  "10000001 - 20000000",
                                     large_prog_transfer_value >= 20000001 & large_prog_transfer_value <= 50000000 ~  "20000001 - 50000000",
                                     large_prog_transfer_value >= 50000001 & large_prog_transfer_value <= 100000000 ~  "50000001 - 100000000",
                                     large_prog_transfer_value >= 100000001 & large_prog_transfer_value <= 200000000 ~  "100000001 - 200000000",
                                     large_prog_transfer_value >= 200000001 & large_prog_transfer_value <= 500000000 ~  "200000001 - 500000000",
                                     large_prog_transfer_value >= 500000001 & large_prog_transfer_value <= 1000000000 ~ "500000001 - 1000000000",
                                     large_prog_transfer_value > 1000000000  ~    "1000000000 +",
                                     TRUE ~ "999"
                                     
    )
    
  )

df_tool_data %>% 
  tabyl(i.region)

