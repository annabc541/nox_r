library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

setwd("~/Cape Verde/nox/data_submission/downloaded_data")

# Reading in NO ebas data -------------------------------------------------

#reading in NO data from first group on ebas
files = list.files(full.names = TRUE,pattern = "_1")
skip_values = c(71,71,73,75,73)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],skip = skip_values[index],header = TRUE) %>%
    mutate(date = as.POSIXct(round(starttime / 0.0417) *3600,
                             origin = sprintf("20%s-01-01",str_extract(files[index],"\\d+")))) %>% 
    tibble()
  
}

ebas_no_1 = bind_rows(datList) %>% 
  arrange(date) %>% 
  select(date,no_ppb = NO.1,no_flag = flag) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_1")},
               .cols=-date)

#reading in NO data from second group on ebas
files = list.files(full.names = TRUE,pattern = "_2.nas")
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],skip = 74,header = TRUE) %>%
    mutate(date = as.POSIXct(round(starttime / 0.0417) *3600,
                             origin = sprintf("20%s-01-01",str_extract(files[index],"\\d+")))) %>% 
    tibble()
  
}

ebas_no_2 = bind_rows(datList) %>% 
  arrange(date) %>% 
  select(date,no = NO,no_flag = flag_NO) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_1")},
               .cols=-date)

#reading in NO data from third group on ebas - appears to be just lod and uncertainty
files = list.files(full.names = TRUE,pattern = "_3")
skip_values = c(74,74,74,74,74,74,74,76,74)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],skip = skip_values[index],header = TRUE) %>%
    mutate(date = as.POSIXct(round(starttime / 0.0417) *3600,
                             origin = sprintf("20%s-01-01",str_extract(files[index],"\\d+")))) %>% 
    tibble()
  
}

ebas_no_3 = bind_rows(datList) %>% 
  arrange(date) %>% 
  select(date,no_lod = NO,no_lod_ppb = NO.1,
         no_uncertainty = NO.2,no_uncertainty_ppb = NO.3) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_1")},
               .cols=-date)

ebas_no = left_join(ebas_no_2,ebas_no_3,by = "date") %>%
  mutate(no_ppb_1 = no_1 / 0.6249228119) %>% 
  bind_rows(ebas_no_1) %>% 
  filter(no_flag_1 != 0.999) %>%
  arrange(date) %>% 
  mutate(no_ppt = no_ppb_1 * 10^3) %>% 
  select(date,no_ppt,no_flag = no_flag_1)

remove(ebas_no_1,ebas_no_2,ebas_no_3)


# Reading in NO earlier data ----------------------------------------------

setwd("~/Cape Verde/nox/processing/ebas_ceda_data")

nox_filtered = read.table("cv-noxy_capeverde_20061001_60min_filtered.na",skip = 48) %>% 
  rename(date = V1,
         no_ppt_filtered = V2,
         no_error_flag_filtered = V3,
         no2_ppt_filtered = V4,
         no2_error_flag_filtered = V5,
         noy_ppt_filtered = V6,
         noy_error_flag_filtered = V7) %>% 
  mutate(date = as.POSIXct(round(date/0.0417)* 3600,
                           origin = "2006-01-01 00:00")) %>% 
  select(c(date,no_ppt = no_ppt_filtered,no_flag = no_error_flag_filtered))

# Reading NO2 ebas data ---------------------------------------------------

#reading in NO2 data from ebas
setwd("~/Cape Verde/nox/processing/ebas_ceda_data/ebas_no2")

files = list.files(full.names = TRUE,pattern = "_no2")
skip_values = c(76,76,76,76,78,76)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],skip = skip_values[index],header = TRUE) %>%
    mutate(date = as.POSIXct(round(starttime / 0.0417) *3600,
                             origin = sprintf("20%s-01-01",str_extract(files[index],"\\d+")))) %>% 
    tibble()
  
}

ebas_no2 = bind_rows(datList) %>% 
  arrange(date) %>% 
  select(date,no2_ppb_ebas = NO2.1,no2_flag = flag) %>% 
  mutate(no2_ppt = no2_ppb_ebas * 10^3) %>% 
  select(date,no2_ppt,no2_flag)

# Creating complete nox df-------------------------------------------------

nox_full_data = left_join(ebas_no,ebas_no2,by = "date") %>% 
  bind_rows(nox_filtered) %>% 
  arrange(date) %>% 
  mutate(no_flag = case_when(no_flag == 3 ~ 0.999,
                             no_flag == 2 ~ 0.147,
                             TRUE ~ no_flag))


unique_years = unique(year(nox_full_data$date))
path = "~/Cape Verde/nox/processing/ebas_ceda_data/yearly_data"

for (year in unique_years) {
  
  df_year = nox_full_data %>% filter(year(date) == year)
  filename = paste0("cvao_nox_",year,".csv")
  full_path = file.path(path,filename)
  write.csv(df_year,file = full_path,row.names = F)
}
