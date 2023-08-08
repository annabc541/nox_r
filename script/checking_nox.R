library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

Sys.setenv(TZ = 'UTC')

#used most recently on 04/08 to check NOxy parameters and measurements after restart on 21/07
#temp and cell pressure high, thought to be due to temperature in the lab


# Read in data ------------------------------------------------------------

#reading in processed data
setwd("~/Cape Verde/nox/processing/data/pmt_27")

processed_dat = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") %>% 
  filter(date > "2023-08-01")


#reading in raw data -> parameters for that period
setwd('D:/Cape Verde/data/nox_raw_data')

files = list.files(pattern = "z_2308", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble()
  
}

raw_dat = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  timeAverage("5 min") %>%
  remove_empty() %>%
  remove_constant()

# raw_dat_june = bind_rows(datList) %>%
#   mutate(date = round_date(date, "1 sec")) %>% 
#   timeAverage("5 min") %>%
#   remove_empty() %>%
#   remove_constant()

# raw_dat = bind_rows(raw_dat_june,raw_dat_july) %>% 
#   arrange(date)

processed_dat = processed_dat %>% 
  select(date,sens = SENS,ce_blc = CE,ce_diode = CE_diode,no = NO_Conc_art_corrected,no2_blc = NO2_Conc_art_corrected,no2_diode = NO2_Conc_diode)

raw_dat = raw_dat %>% 
  select(-c(CH1_Raw,CH1_sens,CH1_sens,CH2_Hz,CH2_Raw,Bypass_prereactor,NO2_CE,no2_conc_avg,no_conc_avg,no2_ch1_5min,NO2_CE_diodes,no2_conc_diodes_5min,M3_zero,NOy_conc_cyclone,NOy_conc_nocyclone))

#joining raw and processed data
dat_aug = left_join(processed_dat,raw_dat, by = "date") %>% 
  arrange(date)

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

#data saved until 4th August 2023
write.csv(dat,"output/dat23.csv",row.names = FALSE)


# Read in simplified ------------------------------------------------------

#reading in processed and raw data from the whole year (5 min average) then removing august to add more
#up to date august data

dat = read.csv("output/dat23.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  filter(date < "2023-08-01")

all_dat = bind_rows(dat,dat_aug) %>% 
  arrange(date)

# Plotting and selecting data required ------------------------------------

all_dat %>% 
  filter(date > "2023-07-23",
         NOx_cal == 0,
         CH1_Hz > 0) %>% 
  # timeAverage("1 hour") %>%
  pivot_longer(c(CH1_Hz,no2_blc,no2_diode,no)) %>%
  ggplot(aes(date,value,col = PMT_Temp)) +
  facet_grid(rows = vars(name),scales = "free_y") +
  geom_point(size = 0.8) +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  scale_color_viridis_c() +
  NULL

dat_of_interest = dat %>% 
  filter(date > "2023-06-01",
         NOx_cal == 0,
         CH1_Hz > 0) %>% 
  select(date,
         ch1_hz = CH1_Hz,lab_temp = Control_Temp,hv2 = HV_2, pmt_temp = PMT_Temp,
         rxn_cell_temp = Rxn_Cell_Temp,rxn_cell_pressure = Rxn_Vessel_Pressure,
         no = NO_Conc_art_corrected,no2_blc = NO2_Conc_art_corrected,no2_diode = NO2_Conc_diode)

dat_of_interest %>% 
  filter(date > "2023-07-01") %>%
  timeAverage("1 hour") %>%
  pivot_longer(c(pmt_temp,rxn_cell_pressure,ch1_hz)) %>%
  ggplot(aes(date,value,col = lab_temp)) +
  facet_grid(rows = vars(name),scales = "free_y") +
  geom_point(size = 0.8) +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  scale_color_viridis_c()
  NULL

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")  
  
ggsave('july_raw_dat_parameters.svg',
         path = "output",
         width = 30,
         height = 12,
         units = 'cm')



# Processed data without PMT temperature filter ---------------------------

#reading in processed data
setwd("~/Cape Verde/nox/processing/data/no_pmt_filter")

processed_dat_unfiltered = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min")
  # filter(date > "2023-06-01")

processed_dat_unfiltered = processed_dat_unfiltered %>% 
  select(date,no_unfiltered = NO_Conc_art_corrected,no2_blc_unfiltered = NO2_Conc_art_corrected,no2_diode_unfiltered = NO2_Conc_diode)

dat_unfiltered_check = left_join(all_dat,processed_dat_unfiltered)

dat_unfiltered_check %>% 
  filter(NOx_cal == 0,
         CH1_Hz > 0,
         no2_diode_unfiltered < 100 & no2_diode_unfiltered > 0,
         no2_diode > 0
         # PMT_Temp < -20
         ) %>% 
  # timeAverage("1 hour") %>%
  pivot_longer(c(no2_diode,no2_diode_unfiltered)) %>%
  ggplot(aes(date,value,col = PMT_Temp)) +
  facet_grid(rows = vars(name),scales = "free_y") +
  geom_point(size = 0.8) +
  # scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  scale_color_viridis_c() +
  NULL
