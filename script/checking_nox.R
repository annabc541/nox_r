library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

#for finding and solving problems with the NOxy instrument

# Reading in raw data -----------------------------------------------------

setwd("~/Cape Verde/nox/processing/nox_r")

#read in saved raw datasets
raw_dat23_cal = read.csv("output/data/raw_data/cal/raw_dat23_cals_1s.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("1 day")

#creating/updating raw datasets
setwd('C:/Users/anna_/Desktop/harddrive_backup/Cape Verde/nox_raw_data')
 
files = list.files(pattern = "z_2304", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {

  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    # timeAverage("5 min") %>%
    tibble()

}

raw_dat2304 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant()


#creating 1 second raw dataset for looking at cals
files = list.files(pattern = "z_20", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {

  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble() %>% 
    timeAverage("5 min")

}

raw_dat20 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant()

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

write.csv(raw_dat20,"output/data/raw_data/raw_dat20.csv",row.names =  FALSE)

# Read in processed data ------------------------------------------------------------

setwd("~/Cape Verde/nox/processing/processed_data")

files = list.files(pattern = "calc_", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    tibble() %>% 
    rename(date = X) %>% 
    mutate(date = ymd_hms(date),
           date = round_date(date, "1 sec")) %>% 
    remove_empty() %>% 
    remove_constant() %>%
    arrange(date) %>% 
    timeAverage("5 min")
  
}

calc_dat = bind_rows(datList)

files = list.files(pattern = "cal_", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    tibble() %>% 
    rename(date = X) %>% 
    mutate(date = ymd_hms(date),
           date = round_date(date, "1 sec")) %>% 
    remove_empty() %>% 
    remove_constant()
  
}

cal_dat = bind_rows(datList)

files = list.files(pattern = "art_", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    tibble() %>% 
    rename(date = X) %>% 
    mutate(date = ymd_hms(date),
           date = round_date(date, "1 sec")) %>% 
    remove_empty() %>% 
    remove_constant()
  
}

art_dat = bind_rows(datList)

#creating df with processed and raw data
# all_dat = left_join(processed_dat,raw_dat23,by = "date")
# write.csv(all_dat23_pmt,"output/processed_and_raw_data.csv",row.names =  FALSE)


# Assessment of instrument parameters -------------------------------------

setwd("~/Cape Verde/nox/processing/nox_r")

#temperatures and flows
bind_rows(raw_dat20,raw_dat21,raw_dat22,raw_dat23,raw_dat24) %>% 
  arrange(date) %>%
  # mutate(NO_valve == 1) %>% 
  timeAverage("1 hour") %>%
  mutate(year = year(date),
         month = month(date),
         doy = yday(date),
         Rxn_Vessel_Pressure = case_when(Rxn_Vessel_Pressure < 2.5 & Rxn_Vessel_Pressure > 0 ~ Rxn_Vessel_Pressure,
                              TRUE ~ NA_real_)
         ) %>%
  filter(date > "2023-02-01") %>%
  ggplot(aes(date,Rxn_Vessel_Pressure)) +
  geom_path() +
  # facet_wrap(~year,ncol = 1, scales = "free") +
  theme_bw() +
  labs(x = NULL,
       y = "Reaction cell temperature") +
  scale_x_datetime(breaks = "1 month",date_labels = "%b %y")

ggsave('rxn_cell_pressure23to24.png',
       path = "output/plots/nox_checks/jan24",
       width = 30,
       height = 15.04,
       units = 'cm')

#no cal gas flow
cal_dat %>% 
  mutate(year = year(date)) %>% 
  # filter(year < 2024 & year > 2019) %>% 
  ggplot(aes(date,NO_cal_flow_mean)) +
  geom_point() +
  # facet_wrap(~year,ncol = 1, scales = "free_x") +
  theme_bw() +
  labs(x = NULL,
       y = "NO calibration gas flow / sccm") +
  scale_x_datetime(breaks = "3 month",date_labels = "%b %y")

ggsave('no_cal_gas_flow.png',
       path = "output/plots/nox_checks/jan24",
       width = 30,
       height = 15.04,
       units = 'cm')

# CE, sens and artefacts --------------------------------------------------

setwd("~/Cape Verde/nox/processing/nox_r")

#looking at CE and SENS
cal_dat %>% 
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
  scale_x_datetime(date_breaks = "3 month",date_labels = "%b %y") +
  labs(x = NULL,
       y = NULL,
       col = NULL) +
  theme(legend.position = "top") +
  NULL

ggsave('ce_sens.png',
       path = "output/plots/nox_checks/jan24",
       width = 30,
       height = 15.04,
       units = 'cm')

#looking at pag artefacts
art_dat %>% 
  mutate(year = year(date)) %>% 
  filter(PAG_Zero_NO2_Conc_last < 200) %>%
  ggplot(aes(date,PAG_Zero_NO2_BLC_art)) +
  geom_point() +
  # facet_wrap(~year,ncol = 1, scales = "free_x") +
  theme_bw() +
  labs(x = NULL,
       y = expression(NO[2]~(ppt))) +
  scale_x_datetime(breaks = "4 month",date_labels = "%b %y")

ggsave('no2_blc_pag_art.png',
       path = "output/plots/nox_checks/jan24",
       width = 30,
       height = 15.04,
       units = 'cm')

#no nighttime artefact
calc_dat %>% 
  mutate(year = year(date)) %>% 
  filter(NO_night_mean > -50,
         NO_night_mean < 15) %>%
  ggplot(aes(date,NO_night_mean)) +
  geom_point() +
  # facet_wrap(~year,ncol = 1, scales = "free_x") +
  theme_bw() +
  labs(x = NULL,
       y = "NO nighttime artefact / ppt") +
  scale_x_datetime(breaks = "4 month",date_labels = "%b %y")

ggsave('no_nighttime_art.png',
       path = "output/plots/nox_checks/jan24",
       width = 30,
       height = 15.04,
       units = 'cm')

# Processed data ----------------------------------------------------------

setwd("~/Cape Verde/nox/processing/nox_r")

#timeseries
calc_dat %>% 
  timeAverage("1 hour") %>% 
  mutate(year = year(date),
         doy = yday(date),
         NO2 = case_when(NO2_Conc_diode > 200 ~ NA_real_,
                         NO2_Conc_diode < -25 ~ NA_real_,
                         TRUE ~ NO2_Conc_diode)) %>% 
  filter(date > "2023-01-31") %>%
  ggplot(aes(date,NO2)) +
  geom_path() +
  theme_bw() +
  # facet_wrap(~year,ncol = 1,scales = "free_x") +
  scale_x_datetime(date_breaks = "2 month",date_labels = "%b %y") +
  labs(x = NULL,
       y = expression(NO[2]~(ppt))) 

ggsave('no2_2324.png',
       path = "output/plots/nox_checks/jan24",
       width = 30,
       height = 15.04,
       units = 'cm')

#seasonal diurnals
outupt = calc_dat %>% 
  mutate(year = year(date),
         NO2 = case_when(NO2_Conc_diode > 200 ~ NA_real_,
                         NO2_Conc_diode < -25 ~ NA_real_,
                         TRUE ~ NO2_Conc_diode)) %>% 
  filter(year > 2019) %>% 
  # rename(NO2 = NO2_Conc_diode) %>% 
  timeVariation(pollutant = "NO2",type = "season",group = "year")

no2_diurnals = outupt$data$hour

no2_diurnals %>% 
  filter(variable != 2024) %>% 
  ggplot(aes(hour,Mean,col = as.character(variable))) +
  geom_path(linewidth = 0.8) +
  theme_bw() +
  facet_wrap(~season) +
  scale_colour_viridis_d() +
  labs(x = "Hour of day (UTC)",
       y = expression(NO[2]~(ppt)),
       col = NULL) +
  theme(legend.position = "top")

ggsave('no2_seasonal_diurnals.png',
       path = "output/plots/nox_checks/jan24",
       width = 30,
       height = 15.04,
       units = 'cm')

# PAG problems ------------------------------------------------------------

setwd("~/Cape Verde/nox/processing/nox_r")

raw_dat2304 %>% 
  filter(NOx_cal == 0,
         zero_air_valve == 1) %>% 
  filter(date > "2023-04-20" & date < "2023-04-21") %>% 
  mutate(measurement_type = case_when(diodes == 1 ~ "NO2 diodes",
                                      NO2_converter == 1 ~ "NO2 BLC",
                                      zero_valve_1 == 1 ~ "Zero",
                                      TRUE ~ "NO")) %>% 
  ggplot(aes(date,CH1_Hz,col = measurement_type)) +
  theme_bw() +
  geom_point() +
  labs(col = NULL,
       x = NULL) +
  theme(legend.position = "top")

ggsave('pag_20-04-23.png',
       path = "output/plots/nox_checks/jan24",
       width = 10,
       height = 12,
       units = 'cm')
