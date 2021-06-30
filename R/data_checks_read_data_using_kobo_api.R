# data checks
library(tidyverse)
library(koboAPI)
# library(koboloadeR)

# rm(list = ls())

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