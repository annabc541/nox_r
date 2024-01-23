library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(zoo)
library(viridis)
library(openair)
library(waclr)
library(janitor)

setwd('D:/Cape Verde/Data/Raw data/NOx/22')

# Importing october data --------------------------------------------------

files = list.files('October', full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime)
  tibble()
  
}

dat_oct1 = bind_rows(datList)

dat_oct = dat_oct1 %>% 
  filter(NOx_cal == 0,
         zero_valve_1 == 0) %>% 
  select(date,CH1_Hz,CH1_sens,Rxn_Vessel_Pressure,NO2_CE,NO2_CE_diodes,Control_Temp,PMT_Temp,Rxn_Cell_Temp,Zero_Vol_Temp) %>% 
  remove_empty() %>%
  remove_constant() %>% 
  timeAverage(avg.time = "5 min")

# Importing november --------------------------------------------------

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
  filter(NOx_cal == 0,
         zero_valve_1 == 0) %>% 
  select(date,CH1_Hz,CH1_sens,Rxn_Vessel_Pressure,NO2_CE,NO2_CE_diodes,NOx_cal,zero_valve_1,Control_Temp,PMT_Temp,Rxn_Cell_Temp,Zero_Vol_Temp) %>%
  remove_empty() %>%
  remove_constant() %>% 
  timeAverage(avg.time = "5 min")

# Plotting ----------------------------------------------------------------

bind_rows(dat_oct,dat_nov) %>% 
  filter(CH1_Hz > -100,
         date > "2022-10-10" & date < "2022-11-07") %>%
  pivot_longer(c(CH1_Hz,Rxn_Vessel_Pressure,PMT_Temp)) %>%
  ggplot(aes(date,value,col = Rxn_Cell_Temp)) +
  geom_point() +
  labs(color = "Temp",
       x = NULL,
       y = NULL) +
  facet_grid(rows = vars(name),scales = "free_y") +
  scale_color_gradientn(colours = viridis(6))
  # scale_x_datetime(breaks = "1 day") +
  NULL

ggsave("Problem_period.svg",
       path = "C:/Users/anna_/Documents/PhD work/CV/Problems",
       width = 32,
       height = 15,
       units = 'cm')
