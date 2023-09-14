library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

# Processed NOx timeseries ---------------------------------------------------------

setwd("~/Cape Verde/nox/processing/data/processed_data_sep23")

processed_dat23 = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  filter(date < "2023-09-01") %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("1 hour") 

nox23 = processed_dat23 %>% 
  select(date,no = NO_Conc_art_corrected,no2 = NO2_Conc_diode)

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")
write.csv(nox23,"output/data/nox23.csv",row.names = F)

nox23 %>% 
  pivot_longer(c(no,no2)) %>% 
  ggplot(aes(date,value)) +
  facet_grid(rows = vars(name),scales = "free_y") +
  geom_path()


# Lab temperature timeseries ----------------------------------------------

raw_dat23 = read.csv("output/raw_dat23.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("1 hour")

temp = raw_dat23 %>% 
  select(date,lab_temp = Control_Temp,pmt_temp = PMT_Temp)

write.csv(temp,"output/data/temp.csv",row.names = F)

temp %>% 
  filter(date > "2023-02-01",
         pmt_temp < -20) %>% 
  pivot_longer(c(lab_temp,pmt_temp)) %>% 
  ggplot(aes(date,value)) +
  facet_grid(rows = vars(name),scales = "free_y") +
  geom_path()


# Lab temperature over the years ------------------------------------------

setwd('D:/Cape Verde/data/nox_raw_data')

files = list.files(pattern = "z_22", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble() %>% 
    timeAverage("1 hour")
  
}

raw_dat22 = bind_rows(datList) %>%
  remove_empty() %>%
  remove_constant()

write.csv(raw_dat22,"hourly_year/raw_dat22.csv",row.names = F)

files = list.files(pattern = "z_21", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble() %>% 
    timeAverage("1 hour")
  
}

raw_dat21 = bind_rows(datList) %>%
  remove_empty() %>%
  remove_constant()

write.csv(raw_dat21,"hourly_year/raw_dat21.csv",row.names = F)

ggplot(raw_dat21,aes(date,Control_Temp)) +
  geom_point()


# Plotting lab temp (2021 to 2023) ----------------------------------------

bind_rows(raw_dat21,raw_dat22,raw_dat23) %>% 
  mutate(year = year(date),
         month = month(date),
         Control_Temp = ifelse(date > "2021-06-01" & date < "2021-06-02",NA_real_,Control_Temp),
         PMT_Temp = ifelse(date > "2022-10-28" & date < "2023-02-01",NA_real_,PMT_Temp)) %>% 
  filter(month > 5 & month < 9,
         Control_Temp < 28) %>% 
  rename(lab_temp = Control_Temp) %>% 
  # pivot_longer(c(lab_temp,PMT_Temp)) %>% 
  ggplot(aes(date,lab_temp)) +
  geom_path() +
  facet_grid(cols = vars(year),scales = "free") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %y")

ggsave("summer_temp.svg",
       path = "output/plots_18sept_meeting/",
       width = 30,
       height = 12,
       units = 'cm')