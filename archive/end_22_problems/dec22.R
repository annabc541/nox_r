library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(janitor)
library(openair)


setwd('D:/Cape Verde/Data/Raw data/NOx/22')

# Importing November --------------------------------------------------

files = list.files('November', full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime)
  tibble()
  
}

dat_nov1 = bind_rows(datList)

dat_nov = dat_nov1 %>% 
  filter(date > "2022-11-10") %>%
  # select(date,CH1_Hz,CH1_sens,Rxn_Vessel_Pressure,NO2_CE,NO2_CE_diodes,NOx_cal,zero_valve_1,Control_Temp,PMT_Temp,Rxn_Cell_Temp,Zero_Vol_Temp) %>%
  remove_empty() %>%
  remove_constant()
# timeAverage(avg.time = "5 min")


# Importing December --------------------------------------------------

files = list.files('December', full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime)
  tibble()
  
}

dat_dec1 = bind_rows(datList)

dat_dec = dat_dec1 %>% 
  # filter(date > "2022-11-10") %>%
  # select(date,CH1_Hz,CH1_sens,Rxn_Vessel_Pressure,NO2_CE,NO2_CE_diodes,NOx_cal,zero_valve_1,Control_Temp,PMT_Temp,Rxn_Cell_Temp,Zero_Vol_Temp) %>%
  remove_empty() %>%
  remove_constant() %>% 
timeAverage(avg.time = "1 min")

# Plotting ----------------------------------------------------------------

dat_dec %>% 
  filter(
    # NOx_cal == 1,
    CH1_Hz < 15000,
    date > "2022-12-12 11:20"
    ) %>%
  mutate(hv = ifelse(date < "2022-12-12 16:40","hv_1","hv_2")) %>% 
  pivot_longer(c(CH1_Hz,CH2_Hz,HV_1,HV_2)) %>%
  ggplot(aes(date,value,col = hv)) +
  facet_grid(rows = vars(name),scales = "free_y") +
  geom_point() +
  # scale_color_gradientn(colours = viridis(6))
  # scale_x_datetime(breaks = "1 day") +
  NULL
