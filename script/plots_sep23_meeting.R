library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')


# Reading in data ---------------------------------------------------------

setwd("~/Cape Verde/nox/processing/data/processed_data_sep23")

processed_dat23_pmt = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  filter(date < "2023-09-01") %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("1 hour") 


# Timeseries plot ---------------------------------------------------------

processed_dat23_pmt %>% 
  # timeAverage("1 hour") %>% 
  # rename(NO = NO_Conc_art_corrected,
  #        NO2_blc = NO2_Conc_art_corrected,
  #        NO2_diode = NO2_Conc_diode) %>% 
  # pivot_longer(c(NO,NO2_blc,NO2_diode)) %>% 
  # pivot_longer(c(SENS,CE,CE_diode)) %>% 
  ggplot(aes(date,NO_night_mean)) +
  theme_bw() +
    # facet_grid(rows = vars(name),scale = "free_y") +
  geom_point() +
  labs(y = NULL,
       x = NULL) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")
ggsave("no_night.svg",
       path = "output/plots_18sept_meeting/",
       width = 30,
       height = 12,
       units = 'cm')


# PAG plots ---------------------------------------------------------------

art = read.csv("NOx_2023_art_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec"))

art %>% 
  rename(NO2_blc = PAG_Zero_NO2_Conc_corr,
         NO2_blc_avg = PAG_Zero_NO2_Conc_mean,
         NO2_diode = PAG_Zero_NO2_Conc_diode_corr,
         NO2_diode_avg = PAG_Zero_NO2_Conc_diode_mean) %>%
  pivot_longer(c(NO2_diode_avg,NO2_blc,NO2_diode,NO2_blc_avg)) %>% 
  ggplot(aes(date,value)) +
  theme_bw() +
  facet_grid(rows = vars(name),scale = "free_y") +
  geom_point() +
  labs(y = "ppt",
       x = NULL) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")

ggsave("conc_pag23.svg",
       path = "output/plots_18sept_meeting/",
       width = 30,
       height = 12,
       units = 'cm')


night = processed_dat23_pmt %>% 
  timeAverage("1 hour")

art1 = art %>% 
  timeAverage("1 hour")

night_art_no = left_join(night,art1)

night_art_no %>% 
  rename(Night = NO_night_mean,
         PAG = PAG_Zero_NO_Conc_mean) %>% 
  pivot_longer(c(Night,PAG)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point() +
  theme_bw() +
  geom_point() +
  labs(y = "ppt",
       x = NULL,
       col = NULL) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")

ggsave("night_pag.svg",
       path = "output/plots_18sept_meeting/",
       width = 30,
       height = 12,
       units = 'cm')

# Comparing with historical PAG measurements ------------------------------

art1 = art %>% 
  filter(date > "2023-04-01") %>% 
  select(date,no = PAG_Zero_NO_Conc_mean,no2_blc = PAG_Zero_NO2_Conc_corr,no2_diode = PAG_Zero_NO2_Conc_diode_corr)

files = list.files(pattern = "art",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  datList[[index]] = read.csv(files[index],header=TRUE,na.strings= c('NA','missing'))%>%
    tibble()
  
}

art_historic = bind_rows(datList) %>% 
  rename(date = DateTime) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec"))

art2 = art_historic %>% 
  select(date,no = PAG_Zero_NO_Conc_mean,no2_blc = PAG_Zero_NO2_Conc_corr,no2_diode =PAG_Zero_NO2_Conc_diode_corr)

art_comp = bind_rows(art2,art1) %>% 
  arrange(date)

art_comp %>% 
  pivot_longer(c(no2_blc,no2_diode)) %>% 
  mutate(year = year(date)) %>% 
  filter( year > 2016,
          value < 120) %>% 
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_point() +
  facet_wrap(ncol =1, vars(year),scales = "free") +
  labs(y = "ppt",
       x = NULL,
       col = NULL) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")

ggsave("no2_art_comparison_april.svg",
       path = "output/plots_18sept_meeting/",
       width = 33.87,
       height = 17,
       units = 'cm')


# Lab temperature ---------------------------------------------------------

raw_dat23 = read.csv("output/raw_dat23.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("5 min")

all_dat23 = left_join(processed_dat23_pmt,raw_dat23,by = "date")

all_dat23 %>%
  filter(date > "2023-06-01") %>% 
  timeAverage("1 hour") %>%
  pivot_longer(c(Control_Temp,PMT_Temp)) %>% 
  ggplot(aes(date,value)) +
  theme_bw() +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free") +
  labs(y = NULL,
       x = NULL) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  NULL

ggsave("temp_timeseries23.svg",
       path = "output/plots_18sept_meeting/",
       width = 30,
       height = 12,
       units = 'cm')

all_dat23 %>%
  filter(CH1_Hz > 0,
         CH1_zero > 0,
         NOx_cal == 0,
         date > "2023-02-01") %>%
  rename(NO = NO_Conc_art_corrected,
         NO2_blc = NO2_Conc_art_corrected,
         NO2_diode = NO2_Conc_diode) %>% 
  pivot_longer(c(NO,NO2_blc,NO2_diode,CH1_Hz,CH1_zero)) %>% 
  ggplot(aes(date,value)) +
  theme_bw() +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free") +
  labs(y = NULL,
       x = NULL) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  scale_colour_viridis_c() +
  NULL

ggsave("nox_timeseries23_raw.svg",
       path = "output/plots_18sept_meeting/",
       width = 30,
       height = 12,
       units = 'cm')


# Met data ----------------------------------------------------------------

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")
met = read.csv("data/met.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date,ws,wd,temp)

nox_with_met = left_join(met,all_dat23) 

nox_with_met %>% 
  mutate(ws_flag = ifelse(ws < 2,1,0),
         wd_flag = ifelse(wd > 100 & wd < 340,1,0),
         NO_Conc_art_corrected = ifelse(ws_flag == 1, NA_real_,NO_Conc_art_corrected),
         NO_Conc_art_corrected = ifelse(wd_flag == 1, NA_real_,NO_Conc_art_corrected),
         NO2_Conc_art_corrected = ifelse(ws_flag == 1, NA_real_,NO2_Conc_art_corrected),
         NO2_Conc_art_corrected = ifelse(wd_flag == 1, NA_real_,NO2_Conc_art_corrected),
         NO2_Conc_diode = ifelse(ws_flag == 1, NA_real_,NO2_Conc_diode),
         NO2_Conc_diode = ifelse(wd_flag == 1, NA_real_,NO2_Conc_diode)) %>%
  filter(date > "2023-02-01") %>%
  timeAverage("1 hour",vector.ws = T) %>%
  rename(NO = NO_Conc_art_corrected,
         NO2_blc = NO2_Conc_art_corrected,
         NO2_diode = NO2_Conc_diode) %>% 
  pivot_longer(c(NO,NO2_blc,NO2_diode,ws,wd)) %>% 
  ggplot(aes(date,value)) +
  theme_bw() +
  geom_path(size = 0.8) +
  facet_grid(rows = vars(name),scales = "free_y") +
  labs(y = NULL,
       x = NULL) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  scale_colour_viridis_c() +
  NULL

ggsave("nox_timeseries23_filtered.svg",
       path = "output/plots_18sept_meeting/",
       width = 30,
       height = 12,
       units = 'cm')


# Comparing with other years ----------------------------------------------

setwd("~/Cape Verde/nox/final_data")

nox14_22 = read.csv("NOx_2014-2022.csv") %>% 
  tibble() %>%  
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>%
  filter(date > "2018-01-01")

nox23 = nox_with_met %>% 
  mutate(no_flag = case_when(ws < 2 ~ 1,
                             wd > 100 & wd < 340 ~ 2,
                             TRUE ~ 0),
         no2_diode_flag = no_flag) %>%
  select(date,no_ppt = NO_Conc_art_corrected,no2_diode_ppt = NO2_Conc_diode,no_flag,no2_diode_flag) %>% 
  timeAverage("1 hour")

nox = bind_rows(nox14_22,nox23) %>% 
  arrange(date)

nox %>% 
  # filter(no_ppt < 50) %>% 
  mutate(year = year(date),
         no2_diode_ppt = ifelse(no2_diode_flag == 0 | no2_diode_flag == 0.147,no2_diode_ppt,NA_real_),
         no2_diode_ppt = ifelse(no2_diode_ppt < 200,no2_diode_ppt,NA_real_),
         no2_diode_ppt = ifelse(no2_diode_ppt > 0,no2_diode_ppt,NA_real_)
         ) %>%
  ggplot(aes(date,no2_diode_ppt)) +
  theme_bw() +
  geom_path() +
  facet_wrap(ncol =1, vars(year),scales = "free") +
  labs(y = "ppt",
       x = NULL,
       col = NULL) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")
ggsave("no218to23_lessthan200.svg",
       path = "output/plots_18sept_meeting/",
       width = 33.87,
       height = 17,
       units = 'cm')

no2_timevar_output = nox %>% 
  mutate(year = year(date),
         no2_diode_ppt = ifelse(no2_diode_flag == 0 | no2_diode_flag == 0.147,no2_diode_ppt,NA_real_),
         no2_diode_ppt = ifelse(no2_diode_ppt < 200,no2_diode_ppt,NA_real_),
         no2_diode_ppt = ifelse(no2_diode_ppt > 0,no2_diode_ppt,NA_real_)) %>%
  filter(year > 2019) %>%
  timeVariation(pollutant = "no2_diode_ppt",group = "year")

plot(no2_timevar_output,subset = "month")

ggsave("no2_monthly_var.svg",
       path = "output/plots_18sept_meeting/",
       width = 33.87,
       height = 17,
       units = 'cm')

no_timevar_output = nox %>% 
  mutate(year = year(date),
         no_ppt = ifelse(no_flag == 0 | no_flag == 0.147,no_ppt,NA_real_),
         no_ppt = ifelse(no_ppt < 200,no_ppt,NA_real_),
         no_ppt = ifelse(no_ppt > 0,no_ppt,NA_real_)) %>%
  filter(year > 2019) %>%
  timeVariation(pollutant = "no_ppt",group = "year")

plot(no_timevar_output,subset = "month")

ggsave("no2_monthly_var.svg",
       path = "output/plots_18sept_meeting/",
       width = 33.87,
       height = 17,
       units = 'cm')
