library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

#for finding and solving problems with the NOxy instrument

# Reading in raw data -----------------------------------------------------

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


#creating 1 second raw dataset for looking at cals
# files = list.files(pattern = "z_23", full.names=TRUE)
# datList = list()
# for(index in 1:length(files)) {
#   
#   datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
#     mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
#     rename(date = TheTime) %>%
#     filter(NOx_cal == 1 | zero_air_valve == 1) %>% 
#     tibble()
#   
# }
# 
# raw_dat23_1s = bind_rows(datList) %>%
#   mutate(date = round_date(date, "1 sec")) %>%
#   remove_empty() %>%
#   remove_constant()

#updating/creating datasets
   
# setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")
# 
# initial_raw_dat23 = read.csv("output/data/raw_dat23.csv") %>% 
#   tibble() %>% 
#   mutate(date = ymd_hms(date))
# 
# raw_dat23 = bind_rows(initial_raw_dat23,raw_dat2311,raw_dat2312) %>% 
#   select(-c(NO_Conc,NO2_CE,NO2_CE_diodes,NO2_conc_diodes)) %>% 
#   remove_constant() %>% 
#   remove_empty() %>% 
#   arrange(date) %>% 
#   filter(date > "2023-01-01" & date < "2024-01-01")

# write.csv(raw_dat23_1s,"output/data/raw_dat23_cals_1s.csv",row.names =  FALSE)

# Read in processed data ------------------------------------------------------------

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

#creating df with processed and raw data
# all_dat = left_join(processed_dat,raw_dat23,by = "date")
# write.csv(all_dat23_pmt,"output/processed_and_raw_data.csv",row.names =  FALSE)

# PAG problems ------------------------------------------------------------

raw_dat2312 %>% 
  mutate(cal = ifelse(NOx_cal == 1 | zero_air_valve == 1, 1, 0)) %>% 
  filter(CH1_Hz < 8000,
         CH1_Hz > 0,
         # cal == 1,
         date < "2023-12-04 02:40" & date > "2023-12-04") %>% 
  # timeAverage("1 hour") %>% 
  # pivot_longer(c(Control_Temp,PMT_Temp,Rxn_Vessel_Pressure,CH1_Hz)) %>% 
  ggplot(aes(date,CH1_Hz,col = zero_air_valve)) +
  geom_point() +
  # facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c()

# ggsave('data_parameters_nov23.png',
#        path = "output/plots/pag_problems_plots",
#        width = 30,
#        height = 12,
#        units = 'cm')

all_dat %>%
  mutate(NO_Conc_art_corrected = ifelse(NO_Conc_art_corrected < 20,NO_Conc_art_corrected,NA_real_),
         NO2_Conc_diode = ifelse(NO2_Conc_diode < 100,NO2_Conc_diode,NA_real_)) %>%
  # timeAverage("1 hour") %>% 
  filter(date > "2023-10-07",
         NOx_cal == 0,
         CH1_Hz > 0,
         CH1_zero > 0) %>%
  pivot_longer(c(CH1_Hz,NO_Conc_art_corrected,NO2_Conc_diode)) %>%
  ggplot(aes(date,value,col = PMT_Temp)) +
  geom_path() +
  scale_x_datetime(date_breaks = "4 days",date_labels = "%d/%m") +
  facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c() +
  NULL

processed_dat23 %>% 
  filter(date > "2023-09-11" & date < "2023-09-25") %>%
  mutate(NO_Conc_art_corrected = ifelse(NO_Conc_art_corrected < 20,NO_Conc_art_corrected,NA_real_),
         NO2_Conc_diode = ifelse(NO2_Conc_diode < 200,NO2_Conc_diode,NA_real_)) %>%
  pivot_longer(c(NO_Conc_art_corrected,NO2_Conc_diode)) %>% 
  ggplot(aes(date,value)) +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free")

ggsave('all_dat_nov23.png',
       path = "output/plots/pag_problems_plots",
       width = 30,
       height = 12,
       units = 'cm')

# Checking CE and SENS ----------------------------------------------------

#looking at CE and SENS
processed_dat %>% 
  # filter(date > "2023-11-01") %>% 
  rename(CE_blc = CE) %>% 
  pivot_longer(c(CE_blc,CE_diode)) %>%
  rename(CE = value,ce_name = name) %>% 
  pivot_longer(c(CE,SENS)) %>% 
  mutate(ce_name = ifelse(name == "SENS",NA_real_,ce_name)) %>% 
  ggplot(aes(date,value,col = ce_name)) +
  theme_bw() +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free") +
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b") +
  labs(x = "Datetime (UTC)",
       y = NULL,
       col = NULL) +
  theme(legend.position = "top") +
  NULL

# Checking NO2 diodes PAG -------------------------------------------------

raw_dat23_1s %>% 
  filter(zero_air_valve == 1 & diodes == 1,
         date > "2023-09-17" & date < "2023-09-19") %>% 
  ggplot(aes(date,CH1_Hz)) +
  geom_point()

processed_dat23 %>% 
  ggplot(aes(date,NO2_Conc_diode)) +
  geom_point()

# setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

# write.csv(raw_dat23_1s,"output/data/raw_dat23_cals_1s.csv",row.names =  FALSE)