library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

#hectic plots thrown together for the Australia meeting 25/09/23

setwd("~/Cape Verde/nox/processing/data/no_pmt_filter")

dat = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("1 hour") 

art = read.csv("NOx_2023_art_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("1 hour") 


cal = read.csv("NOx_2023_cal_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("1 hour") 

cal %>% 
  filter(date > "2023-02-02") %>% 
  ggplot(aes(date,PAG_Zero_NO2_Conc_mean)) +
  geom_point() +
  scale_x_datetime(breaks = "2 weeks",date_labels = "%y/%m/%d")

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

ggsave('hourly_no2_blc_pag_art.svg',
       path = "output/aussie_meeting",
       width = 30,
       height = 12,
       units = 'cm')







