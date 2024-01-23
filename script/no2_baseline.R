library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

#investigating reasons for NO2 baseline increase at the end of 2023
#and looking into any related issues that come up


# Reading in raw data ---------------------------------------------------------

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

#read in raw dataset for 2023
raw_dat23 = read.csv("output/data/raw_dat23.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(date))

#read in 1 second raw cal dataset for 2023
raw_dat23_1s = read.csv("output/data/raw_dat23_cals_1s.csv") %>%
  tibble() %>% 
  mutate(date = ymd_hms(date))

#updating 2024 raw dataset
setwd('E:/Cape Verde/data/nox_raw_data')

files = list.files(pattern = "z_24", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    timeAverage("5 min") %>%
    tibble()
  
}

raw_dat24 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant()


# Reading in processed data -----------------------------------------------

setwd("~/Cape Verde/nox/processing/data")

processed_dat23 = read.csv("processed_data_new_jan24/NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") %>% 
  filter(date > "2023-01-01")

processed_dat24 = read.csv("processed_data_new_jan24/NOx_2024_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") %>% 
  filter(date > "2024-01-01")

processed_dat = bind_rows(processed_dat23,processed_dat24) %>% 
  arrange(date)


# Reading in artefact data - PAG ------------------------------------------

#changed digit as necessary to read in data from 2022,2023,2024 etc.
art24 = read.csv("processed_data_new_jan24/NOx_2024_art_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  # timeAverage("5 min") %>% 
  filter(date > "2024-01-01")

art = bind_rows(art22,art23,art24)


# Plotting PAG artefacts --------------------------------------------------

art %>% 
  filter(PAG_Zero_NO2_Conc_diode_mean < 200) %>%
  ggplot(aes(date,PAG_Zero_NO2_Conc_diode_mean)) +
  geom_point()


# Plotting processed data -------------------------------------------------

processed_dat24 %>% 
  # mutate(NO2_Conc_art_corrected = ifelse(NO2_Conc_art_corrected < 200 & NO2_Conc_art_corrected > 0,
  #                                        NO2_Conc_art_corrected,NA_real_),
  #        NO2_Conc_diode = ifelse(NO2_Conc_diode < 200 & NO2_Conc_diode > 0,
  #                                NO2_Conc_diode,NA_real_)) %>%
  pivot_longer(c(NO2_Conc_diode,NO2_Conc)) %>% 
  ggplot(aes(date,value)) +
  geom_point() +
  facet_grid(rows = vars(name))

ggsave('no2_processed_baseline.png',
       path = "output/plots/no2_baseline",
       width = 30,
       height = 12,
       units = 'cm')
