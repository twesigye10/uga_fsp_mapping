# data checks
library(tidyverse)
library(glue)
library(koboAPI)
# library(kobold)

source("support_data/credentials.R")

# 816981

u<-account
pw<-password

# dataset
form_id<-"816981"
my_data <- download_data(formid = form_id, user=u, pwd = pw, seperator = ".")

# tool itself
tool <- download_form(formid = form_id, user=u, pwd = pw)

# create kobold object
kbo <- kobold::kobold(survey = tool$survey, choices = tool$choices, data = my_data)
kbo_labelled <- kobold::choices = choice_names_to_labels(object = kbo)