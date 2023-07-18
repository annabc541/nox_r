library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

Sys.setenv(TZ = 'UTC')


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
  timeAverage("5 min")


#reading in raw data -> parameters for that period
setwd('D:/Cape Verde/data/nox_raw_data')

files = list.files(pattern = "z_23", full.names=TRUE)
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

#joining raw and processed data
dat = left_join(processed_dat,raw_dat, by = "date") %>% 
  arrange(date)

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

#data saved from start of the year to 1am on 18/07/23 ->update and save in future as required
write.csv(dat,"output/all_dat.csv",row.names = FALSE)

# Plotting and selecting data required ------------------------------------

dat_of_interest = dat %>% 
  filter(date > "2023-06-01",
         NOx_cal == 0,
         CH1_Hz > 0) %>% 
  select(date,
         ch1_hz = CH1_Hz,lab_temp = Control_Temp,hv2 = HV_2, pmt_temp = PMT_Temp,rxn_cell_temp = Rxn_Cell_Temp,rxn_cell_pressure = Rxn_Vessel_Pressure,
         no = NO_Conc_art_corrected,no2_blc = NO2_Conc_art_corrected,no2_diode = NO2_Conc_diode)

dat_of_interest %>% 
  filter(date > "2023-07-10") %>% 
  pivot_longer(c(no,no2_blc,no2_diode,ch1_hz)) %>%
  ggplot(aes(date,value,col = rxn_cell_pressure)) +
  facet_grid(rows = vars(name),scales = "free_y") +
  geom_point() +
  scale_x_datetime(date_breaks = "12 hour",date_labels = "%d %H:%M") +
  scale_color_viridis_c()
  NULL

