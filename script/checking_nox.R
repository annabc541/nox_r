library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

#for checking NOx data and its parameters

# Read in processed data ------------------------------------------------------------

#no filtering based on pmt temperature applied
setwd("~/Cape Verde/nox/processing/data/no_pmt_filter")

processed_dat23_pmt = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") 

#no filtering based on night NO applied
setwd("~/Cape Verde/nox/processing/data/no_night_filter")

processed_dat23_no_night = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") 


# Reading in raw data -----------------------------------------------------

#updating 2023 raw data
setwd('D:/Cape Verde/data/nox_raw_data')

files = list.files(pattern = "z_2309", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble()
  
}

raw_dat2309 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant() %>% 
  timeAverage("5 min")

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

initial_raw_dat23 = read.csv("output/raw_dat23.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("5 min")

raw_dat23 = bind_rows(initial_raw_dat23,raw_dat2309) %>% 
  select(-c(NO_Conc,NO2_CE,NO2_CE_diodes,NO2_conc_diodes)) %>% 
  remove_constant() %>% 
  remove_empty()

all_dat23_pmt = left_join(processed_dat23_pmt,raw_dat23,by = "date")
all_dat23_no_night = left_join(processed_dat23_no_night,raw_dat23,by = "date")

write.csv(all_dat23_pmt,"output/processed_and_raw_data.csv",row.names =  FALSE)

# Plotting for temperature issues summer 2023 -----------------------------

#look at PMT and lab temperatures
all_dat23_pmt %>% 
  filter(date > "2023-02-07") %>% 
  mutate(sensitivity = na.approx(SENS,na.rm = F)) %>% 
  filter(CH1_zero > 0,
         CH1_Hz > 0,
         NOx_cal == 0) %>%
  pivot_longer(c(CH1_Hz,CH1_zero)) %>% 
  ggplot(aes(date,value,col = sensitivity)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c() +
  NULL

ggsave('pmt_lab_temp.png',
       path = "output/plots/temperature_summer23",
       width = 30,
       height = 12,
       units = 'cm')