library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

setwd("~/Cape Verde/nox/processing/ebas_ceda_data")

# Reading in Simone's data ------------------------------------------------

#reading in NO-only df from simone
files = list.files(full.names = TRUE,pattern = "simone_no1")
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],skip = 69,header = TRUE)%>%
    mutate(date = as.POSIXct(round(start_time / 0.0417) *3600,
                             origin = sprintf("20%s-01-01",str_extract(files[index],"\\d+")))) %>% 
    tibble()
  
}

simone_no_dat = bind_rows(datList) %>% 
  arrange(date) %>% 
  select(date,no_ppb = nitrogen_monoxide,no_lod_ppb = nitrogen_monoxide_LOD,
         no_uncertainty_ppb = nitrogen_monoxide_uncertainty,no_flag = nitrogen_monoxide_numflag)

#reading in NOx df from simone
files = list.files(full.names = TRUE,pattern = "simone_nox")
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],skip = 73,header = TRUE)%>%
    mutate(date = as.POSIXct(round(start_time / 0.0417) *3600,
                             origin = sprintf("20%s-01-01",str_extract(files[index],"\\d+")))) %>% 
  tibble()
  
}

simone_nox_dat = bind_rows(datList) %>% 
  arrange(date) %>% 
  select(date,no_ppb = nitrogen_monoxide,no_lod_ppb = nitrogen_monoxide_LOD,
         no_uncertainty_ppb = nitrogen_monoxide_uncertainty,no_flag = nitrogen_monoxide_numflag,
         no2_ppb = nitrogen_dioxide,no2_lod_ppb = nitrogen_dioxide_LOD,
         no2_uncertainty_ppb = nitrogen_dioxide_uncertainty,no2_flag = nitrogen_dioxide_numflag)

simone_dat = bind_rows(simone_no_dat,simone_nox_dat)

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
  select(date,no = NO,no_ppb = NO.1,no_flag = flag) %>% 
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
  mutate(no_ppt_ebas = no_ppb_1 * 10^3) %>% 
  select(date,no_ppt_ebas,no_flag_ebas = no_flag_1)

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
  select(date,no2_ppb_ebas = NO2.1,no2_flag_ebas = flag) %>% 
  mutate(no2_ppt_ebas = no2_ppb_ebas * 10^3) %>% 
  select(date,no2_ppt_ebas,no2_flag_ebas)

# Reading CEDA data -------------------------------------------------------

setwd("~/Cape Verde/nox/processing/ebas_ceda_data")

no_ceda = read.table("ceda_no16.txt",skip = 44) %>% 
  mutate(date = as.POSIXct(round(V1 / 0.0417) *3600,origin = "2006-01-01")) %>% 
  select(date,no_ppt = V2,no_flag = V3)

no_ceda %>% 
  filter(no_ppt < 40) %>% 
  mutate(no_ppt = ifelse(no_flag == 1,no_ppt,NA_real_)) %>% 
  ggplot(aes(date,no_ppt)) +
  geom_path()

dat %>% 
  filter(date > "2015-12-29 01:00" & date < "2016-12-28 19:00") %>% 
  mutate(no_ppb = ifelse(no_flag > 0.149, NA_real_,no_ppb * 1000)) %>% 
  ggplot(aes(date,no_ppb)) +
    geom_path()

# Reading in nox data 2006-2012 -------------------------------------------

setwd("~/Cape Verde/nox/processing/ebas_ceda_data")

nox_raw = read.table("cv-noxy_capeverde_20061001_60min_raw.na",skip = 47) %>% 
  rename(date = V1,
         no_ppt_raw = V2,
         no_error_flag_raw = V3,
         no2_ppt_raw = V4,
         no2_error_flag_raw = V5,
         noy_ppt_raw = V6,
         noy_error_flag_raw = V7) %>% 
  mutate(date = as.POSIXct(round(date/0.0417)* 3600,
                           origin = "2006-01-01 00:00"))

nox_filtered = read.table("cv-noxy_capeverde_20061001_60min_filtered.na",skip = 48) %>% 
  rename(date = V1,
         no_ppt_filtered = V2,
         no_error_flag_filtered = V3,
         no2_ppt_filtered = V4,
         no2_error_flag_filtered = V5,
         noy_ppt_filtered = V6,
         noy_error_flag_filtered = V7) %>% 
  mutate(date = as.POSIXct(round(date/0.0417)* 3600,
                           origin = "2006-01-01 00:00"))

# Reading in merge data ---------------------------------------------------

setwd("~/Cape Verde")

cv_merge = read.csv("20230827_CV_merge.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  clean_names() %>% 
  remove_empty() %>% 
  select(date,year,no_ppt_merge = no_ppt_v,no2_ppt_merge = no2_ppt_v)

# Plotting and comparing data ---------------------------------------------

#ebas/ceda data and simone's data
df_list = list(simone_dat,ebas_no,ebas_no2)
dat = df_list %>% reduce(full_join,by = "date") %>% 
  arrange(date)

dat %>% 
  mutate(no_ppt_simone = ifelse(no_flag > 0.149, NA_real_,no_ppb * 1000),
         no_ppt_ebas = ifelse(no_flag_ebas > 0.149,NA_real_,no_ppt_ebas),
         diff = abs(no_ppt_simone - no_ppt_ebas)) %>%
  # pivot_longer(c(no_ppt_simone,no_ppt_ebas)) %>% 
  filter(date > "2014-01-01" & date < "2022-01-01") %>%
  ggplot(aes(no_ppt_ebas,no_ppt_simone)) +
  geom_point() +
  NULL

# Making nox dataframe and checking everything ----------------------------

df_list = list(cv_merge,nox_raw,nox_filtered,ebas_no,ebas_no2,simone_dat)
dat = df_list %>% reduce(full_join,by = "date")

dat %>% 
  mutate(no_ppt_filtered = case_when(no_error_flag_filtered > 2 ~ NA_real_,
                                     TRUE ~ no_ppt_filtered),
         no_ppt_ebas = case_when(no_flag_ebas > 0.149 ~ NA_real_,
                                 TRUE ~ no_ppt_ebas),
         no_ppt_simone = case_when(no_flag > 0.149 ~ NA_real_,
                                   TRUE ~ no_ppb *10^3)) %>% 
  pivot_longer(c(no_ppt_ebas,no_ppt_simone,no_ppt_merge)) %>%
  filter(date > "2014-01-01" & date < "2022-01-01",
         value < 500) %>%
  ggplot(aes(date,value,col = name)) +
  geom_point() +
  facet_grid(rows = vars(name)) +
  NULL

# Reading in 2022 and 2023 data -------------------------------------------

setwd("~/Cape Verde/nox/processing/data/processed_data_new_jan24")

nox22 = read.csv("NOx_2022_calc_df.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  select(date,no_ppt = NO_Conc_art_corrected,no2_ppt = NO2_Conc_diode) %>% 
  filter(date > "2021-12-31 23:59:59" & date < "2023-01-01") %>% 
  timeAverage("1 hour")
  
nox23 = read.csv("NOx_2023_calc_df.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  select(date,no_ppt = NO_Conc_art_corrected,no2_ppt = NO2_Conc_diode) %>% 
  filter(date > "2022-12-31 23:59" & date < "2024-01-01") %>% 
  timeAverage("1 hour")

dat = simone_dat %>% 
  mutate(no_ppt = ifelse(no_flag > 0.149, NA_real_,no_ppb * 1000),
         no2_ppt = ifelse(no2_flag > 0.149, NA_real_,no2_ppb * 1000)) %>% 
  bind_rows(nox23) %>% 
  arrange(date)

dat %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(date,no_ppt)) +
  facet_wrap(~year,ncol = 1,scales = "free_x") +
  geom_path()

setwd("~/Cape Verde/nox/processing/nox_r")

diurnal = nox23 %>% 
  rename(NO = no_ppt,NO2 = no2_ppt) %>%
  mutate(month = month(date),
         nox = NO + NO2,
         NO2 = ifelse(NO2 < 200,NO2,NA_real_)) %>% 
  pivot_wider(names_from = month,values_from = NO2) %>% 
  timeVariation(pollutant = c("2":"12"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  mutate(variable = case_when(variable == 2 ~ "February",
                              variable == 3 ~ "March",
                              variable == 4 ~ "April",
                              variable == 5 ~ "May",
                              variable == 6 ~ "June",
                              variable == 7 ~ "July",
                              variable == 8 ~ "August",
                              variable == 9 ~ "September",
                              variable == 10 ~ "October",
                              variable == 11 ~ "November",
                              variable == 12 ~ "December")) %>% 
  ggplot(aes(hour,Mean)) +
  geom_line(linewidth = 1) +
  facet_wrap(~factor(variable,levels = c("February","March","April","May","June","July","August",
                                         "September","October","November","December"))) +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = expression(NO[2]~(ppt))) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  theme(legend.position = "top")

ggsave("hourly_no2_23.png",
       path = "output/plots/no2_baseline",
       width = 30,
       height = 12,
       units = 'cm')
