library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

#for checking NOx data and its parameters

# Read in processed data ------------------------------------------------------------

setwd("~/Cape Verde/nox/processing/data/processed_data_sep23")

processed_dat = read.csv("NOx_2023_calc_df.csv") %>% 
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
    timeAverage("5 min") %>% 
    tibble()
  
}

raw_dat2309 = bind_rows(datList) %>%
  # mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant() 
  

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

initial_raw_dat23 = read.csv("output/raw_dat23.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("5 min")

raw_dat23 = bind_rows(initial_raw_dat23,raw_dat2309) %>% 
  select(-c(NO_Conc,NO2_CE,NO2_CE_diodes,NO2_conc_diodes)) %>% 
  remove_constant() %>% 
  remove_empty()

all_dat = left_join(processed_dat,raw_dat23,by = "date")

write.csv(all_dat23_pmt,"output/processed_and_raw_data.csv",row.names =  FALSE)

# Checking noxy parameters after pump tip seals were changed --------------

#removed data between 18/09/23 13:30 to 19/09/23 19:00 when pump fan wasn't working

all_dat %>%
  # timeAverage("1 hour") %>% 
  filter(Rxn_Vessel_Pressure < 3) %>%
  # pivot_longer(c(NO_night_mean,Rxn_Vessel_Pressure)) %>% 
  ggplot(aes(PAG_,Rxn_Vessel_Pressure)) +
  geom_point() +
  # scale_x_datetime(date_breaks = "2 weeks",date_labels = "%d/%m") +
  # facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c() +
  NULL

#haven't done this for PAG data because it's in the artefact df and I couldn't be bothered - sorry!
ggsave('rxn_cell_pressure_vs_no_night_artefact.png',
       path = "output/plots/artefact",
       width = 30,
       height = 12,
       units = 'cm')

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
