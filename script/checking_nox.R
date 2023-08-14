library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

Sys.setenv(TZ = 'UTC')

#for checking NOx data and its parameters

# Read in processed data ------------------------------------------------------------

#no filtering based on pmt temperature applied
setwd("~/Cape Verde/nox/processing/data/no_pmt_filter")

processed_dat23 = read.csv("NOx_2023_calc_df.csv") %>% 
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

files = list.files(pattern = "z_2308", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble()
  
}

raw_dat2308 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant() %>% 
  timeAverage("5 min")

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

initial_raw_dat23 = read.csv("output/raw_dat23.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("5 min") %>% 
  filter(date < "2023-08-01 16:28")

raw_dat23 = bind_rows(initial_raw_dat23,raw_dat2308) %>% 
  select(-c(NO_Conc,NO2_CE,NO2_CE_diodes,NO2_conc_diodes)) %>% 
  remove_constant() %>% 
  remove_empty()

all_dat23 = left_join(processed_dat23,raw_dat23,by = "date")

# Plotting for temperature issues summer 2023 -----------------------------

#look at PMT and lab temperatures
all_dat23 %>% 
  filter(date > "2023-06-01", #to remove initial high PMT temperatures due to instrument being turned on
         PMT_Temp < -22.5) %>% 
  pivot_longer(c(PMT_Temp,Rxn_Vessel_Pressure)) %>% 
  ggplot(aes(date,value,col = Control_Temp)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c() +
  NULL

#checking processed data
all_dat23 %>% 
  filter(date > "2023-08-01",
         CH1_Hz > 0,
         CH1_zero > 0,
         NOx_cal == 0,
         NO2_Conc_diode < 1500) %>% 
  pivot_longer(c(NO_Conc_art_corrected,NO2_Conc_art_corrected,NO2_Conc_diode,CH1_Hz,CH1_zero)) %>% 
  ggplot(aes(date,value,col = Control_Temp)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c() +
  NULL
