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

# outputs list
outputs <- list()

# overall analysis
outputs$over_all_analysis <- butteR::survey_collapse(df = df_svy,
                                             vars_to_analyze = variables_to_analyse) %>% 
  mutate(analysis_level = "overall")

# split analysis by subset_1
dap_all_subset <- dap %>% 
  filter(split %in% c("All", "ref_only"), !is.na(subset_1))

# overall, subset_1

dap_all_subset_split <- dap_all_subset %>% 
  split(.$subset_1)

overall_subset1<-list()

for (i in seq_along(dap_all_subset_split)) {
  print(i)
  subset_temp <- dap_all_subset_split[[i]]
  subset_value <- unique(subset_temp$subset_1)
  vars_temp <- subset_temp %>% pull(variable)
  overall_subset1[[subset_value]] <- butteR::survey_collapse(df = df_svy,
                                                             vars_to_analyze = vars_temp, 
                                                             disag = c(subset_value)) %>% 
    mutate(analysis_level = subset_value)
}

outputs$overall_subset1 <- bind_rows(overall_subset1)