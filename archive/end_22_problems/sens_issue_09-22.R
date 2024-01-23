library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(zoo)
library(viridis)
library(openair)
library(waclr)
library(janitor)

setwd('D:/CV/Data/Raw data/NOx/22')

# Importing october data --------------------------------------------------

files = list.files('October', full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime)
  tibble()
  
}

dato = bind_rows(datList)

dato = dato %>% 
  select(date,CH1_Hz,CH1_sens,Rxn_Vessel_Pressure,NO2_CE,NO2_CE_diodes) %>% 
  remove_empty() %>%
  remove_constant() %>% 
  timeAverage(avg.time = "5 min")

  


# Importing problem data --------------------------------------------------


files = list.files('September', full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime)
  tibble()
  
}

datp = bind_rows(datList)

datp = datp %>% 
  select(date,CH1_Hz,CH1_sens,Rxn_Vessel_Pressure,NO2_CE,NO2_CE_diodes) %>% 
  remove_empty() %>%
  remove_constant() %>% 
  timeAverage(avg.time = "5 min")

    


# Importing normal data ---------------------------------------------------

files = list.files('May to July', full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime)
  tibble()
  
}

datn = bind_rows(datList)

datn = datn %>% 
  select(date,CH1_Hz,CH1_sens,Rxn_Vessel_Pressure,NO2_CE,NO2_CE_diodes) %>% 
  remove_empty() %>%
  remove_constant() %>% 
  timeAverage(avg.time = "5 min")


datn %>% 
  # mutate(NOx_cal = ifelse(NOx_cal == 0,"no","yes")) %>% 
  # filter(date < "2022-08-30",
  #        CH1_Hz > -5,
  #        NOx_cal == 1) %>% 
  pivot_longer(cols = c(CH1_Hz,CH1_sens,NO2_CE,NO2_CE_diodes)) %>%
  ggplot(aes(date,value,col = NOx_cal)) +
  geom_line() +
  facet_grid(rows = vars(name),scales = "free_y") +
  # scale_x_datetime(breaks = "3 days") +
  NULL


# Importing in between data -----------------------------------------------

files = list.files('August', full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime)
  tibble()
  
}

datib = bind_rows(datList) %>% 
  remove_empty() %>% 
  remove_constant() %>% 
  select(date,CH1_Hz,CH1_sens,Rxn_Vessel_Pressure,NO2_CE,NO2_CE_diodes) %>% 
  timeAverage(avg.time = "5 min")

dat = bind_rows(datib,datn,datp) %>% 
  arrange(date)

# Plotting ----------------------------------------------------------------

dato %>% 
  filter(
    # date > "2022-09-26",
    # Rxn_Vessel_Pressure < 15
  ) %>%
  pivot_longer(cols = c(
    # CH1_sens,
                        # Rxn_Vessel_Pressure,
                        CH1_Hz,
                        CH1_sens,
                        NO2_CE,
                        NO2_CE_diodes
                        )) %>%
  # drop_na(NOx_cal) %>% 
  # mutate(NOx_cal = ifelse(NOx_cal == 0.5,1,NOx_cal)) %>% 
  ggplot(aes(date,value)) +
  geom_line() +
  facet_grid(rows = vars(name),scales = "free_y") +
  # scale_x_datetime(breaks = "7 days",date_minor_breaks = "1 day") +
  # scale_color_gradientn(colors = viridis(5)) +
  NULL

ggsave("Problem_period.svg",
       path = "C:/Users/anna_/Documents/PhD work/CV/Problems",
       width = 32,
       height = 15,
       units = 'cm')
