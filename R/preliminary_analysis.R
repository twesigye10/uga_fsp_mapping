library(tidyverse)
library(srvyr)
library(janitor)
library(glue)

source("R/composite_indicators.R")


# load data ---------------------------------------------------------------

df_cleaned <- read_csv("outputs/20210913_clean_data.csv")

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
                                                             disag = c(subset_value)) 
}

outputs$overall_subset1 <- bind_rows(overall_subset1) %>% 
  mutate(analysis_level = "by_subset_1")

# merge all analysis
full_analysis_long<- bind_rows(outputs)
# write the output analysis
full_analysis_long %>%
  write_csv(paste0("outputs/", butteR::date_file_prefix(),"_full_analysis_long_format.csv"),na="")


# plots -------------------------------------------------------------------

# some graphs for the analysis
analysis_levels <- c("overall", "by_subset_1")

analysis_level_plots <- list()

for (i in seq_along(analysis_levels)) {
  analysis_level_plots[[analysis_levels[i]]] <- full_analysis_long %>% 
    filter(analysis_level %in% analysis_levels, !is.na(variable_val)) %>% 
    mutate(question_val = paste0(variable, ".", variable_val)) %>% 
    ggplot(aes(x = question_val, y = `mean/pct`, color = subset_1_val))+
    geom_point(stat="identity", position = position_dodge(width = 0.3))+
    geom_errorbar(aes(ymin= `mean/pct_low`, ymax= `mean/pct_upp`), 
                  width=0.2,position = position_dodge(width = 0.3))+
    geom_text(aes(x=question_val,y=`mean/pct`, label=n_unweighted), nudge_x = 0.3)+
    scale_y_continuous(labels = scales::percent,breaks = seq(0,1,by=0.1))+
    labs(color= "i.region")+
    ggtitle(label = glue::glue("{analysis_levels[i]} level"))+
    coord_flip()+
    theme_bw()+
    theme(
      axis.title = element_blank(),
      legend.title= element_blank()
    ) 
  # save the plots
  ggsave(glue::glue("outputs/graphs/{analysis_levels[i]}_level.pdf"),
         height = 36,
         width = 11,
         units = "in",
         device = "pdf")
}


# Boxplot for organisation_cash_capability --------------------------------

# organisation_cash_capability 
# by refugee_settlement 
# By FSP_type 
# By region

df_with_composites %>% 
  filter(!is.na(refugee_settlelement), organisation_cash_capability != 999 ) %>% 
  ggplot(aes(x = refugee_settlelement , y = organisation_cash_capability ))+
  geom_boxplot()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "organisation cash capability by refugee settlelement",
    x= "",
    y= "organisation cash capability"
  )+
  theme_bw()+
  coord_flip()  
ggsave("outputs/graphs/organisation cash capability by refugee settlelement.pdf",
       height = 12,
       width = 9,
       units = "in",
       device = "pdf")

df_with_composites %>% 
  filter(!is.na(organisation_type), organisation_cash_capability != 999 ) %>% 
  ggplot(aes(x = organisation_type , y = organisation_cash_capability ))+
  geom_boxplot()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "organisation cash capability by organisation type",
    x= "",
    y= "organisation cash capability"
  )+
  theme_bw()+
  coord_flip()
ggsave("outputs/graphs/organisation cash capability by organisation type.pdf",
       height = 12,
       width = 9,
       units = "in",
       device = "pdf")

df_with_composites %>% 
  filter(!is.na(i.region), organisation_cash_capability != 999 ) %>% 
  ggplot(aes(x = i.region , y = organisation_cash_capability ))+
  geom_boxplot()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "organisation cash capability by region",
    x= "",
    y= "organisation cash capability"
  )+
  theme_bw()+
  coord_flip()
ggsave("outputs/graphs/organisation cash capability by region.pdf",
       height = 12,
       width = 9,
       units = "in",
       device = "pdf")