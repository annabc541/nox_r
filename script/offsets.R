library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

setwd("~/Cape Verde/nox/processing/processed_data")

# Zero air offsets --------------------------------------------------------

files = list.files(pattern = "art_", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    tibble() %>% 
    mutate(date = ymd_hms(X)) %>% 
    select(date,everything(),-X)
  
}

art_dat = bind_rows(datList)

art_dat %>% 
  mutate(year = year(date)) %>% 
  rename(BLC = PAG_Zero_NO2_Conc_mean,
         Diode = PAG_Zero_NO2_Conc_diode_mean) %>% 
  pivot_longer(c(BLC,Diode)) %>% 
  filter(year > 2019 & year < 2024,
         value < 200) %>% 
  ggplot(aes(date,value,col = name)) +
  labs(x = NULL,
       y = expression(NO[2]~ZA~offset~(ppt)),
       col = expression(NO[2]~measurement)) +
  theme_bw() +
  scale_x_datetime(breaks = "1 month",date_labels = "%b %y") +
  theme(legend.position = "top") +
  geom_point() +
  facet_wrap(vars(year),scales = "free",ncol = 1) +
  NULL

ggsave('no2_za_offset.png',
       path = "~/Cape Verde/nox/processing/nox_r/output/plots/offsets",
       width = 30,
       height = 15.04,
       units = 'cm')


# Nighttime NO offsets ----------------------------------------------------

files = list.files(pattern = "calc_", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    tibble() %>% 
    rename(date = X) %>% 
    mutate(date = ymd_hms(date)) %>% 
    remove_empty() %>% 
    remove_constant() %>%
    arrange(date) %>% 
    timeAverage("5 min")
  
}

calc_dat = bind_rows(datList)

calc_dat %>% 
  timeAverage("1 hour") %>% 
  # pivot_longer(c(NO_Conc,NO_Conc_art_corrected)) %>% 
  mutate(year = year(date)) %>% 
  filter(year > 2019 & year < 2024) %>% 
  ggplot(aes(date,NO_night_mean)) +
  labs(x = NULL,
       y = "NO nighttime offset (ppt)") +
  theme_bw() +
  scale_x_datetime(breaks = "1 month",date_labels = "%b %y") +
  geom_point() +
  facet_wrap(vars(year),scales = "free",ncol = 1) +
  NULL

ggsave('no_nighttime_offset.png',
       path = "~/Cape Verde/nox/processing/nox_r/output/plots/offsets",
       width = 30,
       height = 15.04,
       units = 'cm')
