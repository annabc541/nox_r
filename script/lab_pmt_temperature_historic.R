library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

Sys.setenv(TZ = 'UTC')


# Reading in raw 2023 data ------------------------------------------------

dat23 = read.csv("output/raw_dat23.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("5 min")


# Reading in raw dat from past summers ------------------------------------

setwd('D:/Cape Verde/data/nox_raw_data')

#2022
files = list.files(pattern = "z_2208", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble()
  
}

raw_dat2206 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  timeAverage("1 hour") %>%
  remove_empty() %>%
  remove_constant()

raw_dat2207 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  timeAverage("1 hour") %>%
  remove_empty() %>%
  remove_constant()

raw_dat2208 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  timeAverage("1 hour") %>%
  remove_empty() %>%
  remove_constant()

raw_dat22 = bind_rows(raw_dat2206,raw_dat2207,raw_dat2208)
setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")
write.csv(raw_dat22,"output/summer22.csv")


#2021
setwd('D:/Cape Verde/data/nox_raw_data')
files = list.files(pattern = "z_2108", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble()
  
}

raw_dat2106 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  timeAverage("1 hour") %>%
  remove_empty() %>%
  remove_constant()

raw_dat2107 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  timeAverage("1 hour") %>%
  remove_empty() %>%
  remove_constant()

raw_dat2108 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  timeAverage("5 min") %>%
  remove_empty() %>%
  remove_constant()

raw_dat21 = bind_rows(raw_dat2106,raw_dat2107,raw_dat2108)
setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")
write.csv(raw_dat22,"output/summer21.csv")

# Plotting ----------------------------------------------------------------

#looking at hourly averages for summer 2021, 2022, 2023
#raw datasets of hourly summer months have been saved

raw_dat = bind_rows(raw_dat21,raw_dat22,dat23) %>% 
  arrange(date)

raw_dat %>% 
  mutate(year = year(date),
         month = month(date),
         hour = hour(date),
         doy = yday(date),
         temp_settings = case_when(date < "2023-06-09" ~ "PMT -30, lab 20",
                                   date > "2023-06-09" & date < "2023-06-15" ~ "PMT -28, lab 20",
                                   date > "2023-06-15" & date < "2023-08-08" ~ "PMT -28, lab 22",
                                   date > "2023-08-07" ~ "PMT -24,lab 22")) %>% 
  filter(month >= 6 & month <= 8,
         PMT_Temp > -40) %>%
  # filter(year == 2023) %>% 
  # pivot_longer(c(PMT_Temp,Control_Temp)) %>%
  ggplot(aes(PMT_Temp,Control_Temp,col = doy)) +
  geom_point() +
  facet_grid(cols = vars(year),scales = "free") +
  # labs(y = "Degrees Celsius") +
  # facet_wrap(~year,scales = "free",ncol = 1) +
  # geom_vline(xintercept = as.numeric(as.POSIXct("2023-06-16")),
  #            color = "red") +
  # geom_vline(xintercept = as.numeric(as.POSIXct("2023-06-09")),
  #            color = "blue") +
  scale_colour_viridis_c()

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

ggsave('lab_vs_pmt_21to23_doy.png',
       path = "output/plots/temp_different_years",
       width = 30,
       height = 12,
       units = 'cm')


# Looking at raw counts and temperature -----------------------------------

#plotting the zero counts and the pmt temperature to see what the relationship is between the two
#can be done with 5 min averaged data because zero count is taken as an average for each cycle

#looking at 2023 5 min averaged
dat23 %>% 
  mutate(doy = yday(date)) %>% 
  filter(PMT_Temp > -50 & PMT_Temp < -20,
         CH1_zero > 1000,
         date > "2023-02-14") %>% 
  ggplot(aes(CH1_zero,PMT_Temp,col = doy)) +
  geom_point() +
  scale_colour_viridis_c()

#looking at August 2021 5 min averaged
raw_dat2108 %>% 
  ggplot(aes(CH1_zero,PMT_Temp)) +
  geom_point()

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

ggsave('zero_vs_pmt_temp.png',
       path = "output/plots/temp_different_years",
       width = 30,
       height = 12,
       units = 'cm')
