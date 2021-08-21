library(tidyverse)
library(srvyr)
library(janitor)
library(glue)

source("R/composite_indicators.R")


# load data ---------------------------------------------------------------

df_cleaned <- read_csv("outputs/20210820_clean_data.csv")

dap <- read_csv("inputs/r_dap.csv") %>% 
  janitor::clean_names()

# make composite indicator ------------------------------------------------

df_with_composites <- create_composite_indicators_fsp(input_df = df_cleaned)

# analysis ----------------------------------------------------------------

#  select variables from dap that are in the dataset
variables_to_analyse <- dap$variable[dap$variable %in% colnames(df_with_composites)]

# convert df to a survey using the srvyr package
df_svy <- as_survey(df_with_composites)

