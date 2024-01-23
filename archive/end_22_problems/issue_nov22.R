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
  filter(date > "2022-11-10") %>%
  # select(date,CH1_Hz,CH1_sens,Rxn_Vessel_Pressure,NO2_CE,NO2_CE_diodes,NOx_cal,zero_valve_1,Control_Temp,PMT_Temp,Rxn_Cell_Temp,Zero_Vol_Temp) %>%
  remove_empty() %>%
  remove_constant()
  # timeAverage(avg.time = "5 min")

# Plotting ----------------------------------------------------------------
dat_nov1 %>% 
  filter(
    # NOx_cal == 1,
          date > "2022-11-10 18:00",
          CH1_Hz > 0,
          CH1_Hz < 5000) %>%
  mutate(z = ifelse(CH1_Hz == 0,"yes","no")) %>% 
  # pivot_longer(c(CH1_Hz,CH1_Raw)) %>% 
  ggplot(aes(date,CH1_Hz)) +
  # facet_grid(rows = vars(name),scales = "free_y") +
  geom_point() +
  # scale_color_gradientn(colours = viridis(6))
# scale_x_datetime(breaks = "1 day") +
NULL

ggsave("Problem_period.svg",
       path = "C:/Users/anna_/Documents/PhD work/CV/Problems",
       width = 32,
       height = 15,
       units = 'cm')
