library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

Sys.setenv(TZ = 'UTC')

#used most recently on 01/08 to check NOxy parameters and measurements after restart on 21/07
#temp and cell pressure high, thought to be due to temperature in the lab


# Read in data ------------------------------------------------------------

#reading in processed data
setwd("~/Cape Verde/nox/processing/data/pmt_27")

processed_dat = read.csv("NOx_2023_calc_df.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") %>% 
  filter(date > "2023-06-01")


#reading in raw data -> parameters for that period
setwd('D:/Cape Verde/data/nox_raw_data')

files = list.files(pattern = "z_2307", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble()
  
}

raw_dat_june = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>% 
  timeAverage("5 min") %>%
  remove_empty() %>%
  remove_constant()

raw_dat_july = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>% 
  timeAverage("5 min") %>%
  remove_empty() %>%
  remove_constant()

raw_dat = bind_rows(raw_dat_june,raw_dat_july) %>% 
  arrange(date)

#joining raw and processed data
dat = left_join(processed_dat,raw_dat, by = "date") %>% 
  arrange(date)

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

#data saved for the whole of June and almost all of July (just missing last day of July)
write.csv(dat,"output/dat_june_july.csv",row.names = FALSE)


# Read in simplified ------------------------------------------------------

#data saved from start of the year to 1am on 18/07/23 ->update and save in future as required
dat = read.csv("output/all_dat.csv") %>% 
  mutate(date = ymd_hms(date))

# Plotting and selecting data required ------------------------------------

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

