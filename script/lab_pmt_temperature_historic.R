library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

Sys.setenv(TZ = 'UTC')

#temperature readout from logger computer
lab_met = read.csv("output/data/Met_30m_2023_hourly.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date,Temp,insidetemp)

temp = raw_dat23 %>% 
  timeAverage("1 hour") %>% 
  select(date,Control_Temp,PMT_Temp) %>% 
  left_join(lab_met,by = "date")

temp %>% 
  filter(date > "2023-09-01") %>% 
  pivot_longer(c(Control_Temp,Temp,insidetemp)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_path()

# Reading in hourly raw data ------------------------------------------------

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

raw_dat23 = read.csv("output/data/raw_dat23.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))


raw_dat22 = read.csv("output/data/raw_dat22.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

setwd("D:/Cape Verde/data")

raw_dat21 = read.csv("D:/Cape Verde/data/no_raw_hourly/raw_dat21.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

dat = bind_rows(raw_dat21,raw_dat22,raw_dat23) %>% 
  timeAverage("1 hour") %>% 
  arrange(date)


# Plotting ----------------------------------------------------------------

#comparing 2021, 2022 and 2023

dat %>% 
  timeAverage("6 hour") %>% 
  mutate(PMT_Temp = ifelse(PMT_Temp < -23 & PMT_Temp > -30,PMT_Temp,NA_real_),
         year = year(date),
         doy = yday(date)) %>% 
  filter(doy < 300 & doy > 31) %>% 
  pivot_longer(c(PMT_Temp,Control_Temp)) %>%
  ggplot(aes(date,value)) + 
  geom_path() +
  labs(x = NULL, y = NULL) +
  facet_grid(cols = vars(year),rows = vars(name),scales = "free") +
  scale_x_datetime(breaks = "1 month",date_labels = "%b") +
  scale_colour_viridis_c()

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

ggsave('temp_21to23.png',
       path = "output/plots/nox_overview_plots_oct23",
       width = 30,
       height = 12,
       units = 'cm')


dat %>% 
  # timeAverage("6 hour") %>% 
  mutate(PMT_Temp = ifelse(PMT_Temp < -23 & PMT_Temp > -30,PMT_Temp,NA_real_),
         year = year(date),
         doy = yday(date)) %>% 
  filter(doy < 300 & doy > 31,
         CH1_zero > 2500,
         CH1_zero < 7000) %>% 
  # pivot_longer(c(PMT_Temp,Control_Temp)) %>%
  ggplot(aes(CH1_zero,PMT_Temp,col = Control_Temp)) + 
  geom_point() +
  labs(x = NULL, y = NULL) +
  facet_grid(cols = vars(year)) +
  # scale_x_datetime(breaks = "1 month",date_labels = "%b") +
  scale_colour_viridis_c()

# Looking at raw counts and temperature -----------------------------------

#plotting the zero counts and the pmt temperature to see what the relationship is between the two
#can be done with 5 min averaged data because zero count is taken as an average for each cycle

dat23 = bind_rows(dat23,raw_dat2308)

#looking at 2023 5 min averaged
dat23 %>% 
  filter(date > "2023-02-14",
         NOx_cal == 0,
         CH1_zero > 0 & CH1_zero < 6000) %>% 
  mutate(month = month(date)) %>%
  ggplot(aes(Rxn_Cell_Temp,CH1_zero,col = PMT_Temp)) +
  # facet_grid(rows = vars(name),scales = "free_y") +
  geom_point() +
  scale_colour_viridis_c()

#looking at August 2021 5 min averaged
raw_dat2108 %>% 
  ggplot(aes(CH1_zero,PMT_Temp)) +
  geom_point()

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

ggsave('lab_pmt_raw_summer23_rxn_col.png',
       path = "output/plots/2023",
       width = 30,
       height = 12,
       units = 'cm')


# Reading in completely raw data from past summers ------------------------------------

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
  timeAverage("1 hour") %>%
  remove_empty() %>%
  remove_constant()

dat21 = bind_rows(raw_dat2106,raw_dat2107,raw_dat2108)
setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")
write.csv(dat21,"output/summer21.csv",row.names = FALSE)
