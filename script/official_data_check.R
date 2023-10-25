library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

#for officially plotting and looking at NOx data in a traceable and repeatable manner
#data recorded while pmt temp is above -23 has been removed

# Read in data ------------------------------------------------------------

setwd("~/Cape Verde/nox/processing/data/processed_data_oct23")

processed_dat = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") 

cal_dat = read.csv("NOx_2023_cal_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

art_dat = read.csv("NOx_2023_art_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

error_dat = read.csv("NOx_2023_error_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

zero_dat = read.csv("NOx_2023_zero_df.csv") %>% 
  tibble() %>% 
  rename(date = TheTime) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

raw_dat = read.csv("output/data/raw_dat23.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

all_dat = full_join(processed_dat,raw_dat,by = "date")

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")
met = read.csv("data/met.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date,ws,wd,temp)

nox_with_met = left_join(met,all_dat) 

# Plotting 2023 data ------------------------------------------------------


nox_with_met %>%
  mutate(flag = case_when(ws < 2 ~ "ws flag",
                          wd > 100 & wd < 340 ~ "wd flag",
                          TRUE ~ "no flag"),
         remove_dates = case_when(date > "2023-09-18" & date < "2023-09-20" ~ 1,
                                  date > "2023-02-28" & date < "2023-03-01" ~ 2,
                                  TRUE ~ 0),
         PMT_Temp = ifelse(remove_dates == 0, PMT_Temp,NA_real_),
         Rxn_Vessel_Pressure = ifelse(remove_dates == 0, Rxn_Vessel_Pressure,NA_real_),
         NO_Conc_art_corrected = case_when(NO_Conc_art_corrected > 50 ~ NA_real_,
                                           flag != "no flag" ~ NA_real_,
                                           TRUE ~ NO_Conc_art_corrected),
         NO2_Conc_diode = case_when(NO2_Conc_diode > 100 ~ NA_real_,
                                    flag != "no flag" ~ NA_real_,
                                    TRUE ~ NO2_Conc_diode),
         CH1_Hz = case_when(CH1_Hz < 0 ~ NA_real_,
                            CH1_Hz > 7000 ~ NA_real_,
                            NOx_cal == 1 ~ NA_real_,
                            NO_valve == 1 ~ NA_real_,
                            PMT_Temp > -23 ~ NA_real_,
                            PMT_Temp < -30 ~ NA_real_,
                            CH1_Hz > 15000 ~ NA_real_,
                            remove_dates != 0 ~ NA_real_,
                            TRUE ~ CH1_Hz),
         CH1_zero = case_when(CH1_zero < 0 ~ NA_real_,
                              PMT_Temp > -23 ~ NA_real_,
                              PMT_Temp < -30 ~ NA_real_,
                              remove_dates != 0 ~ NA_real_,
                              TRUE ~ CH1_zero)) %>% 
  # timeAverage("1 hour") %>%
  filter(date > "2023-09-02") %>%
  rename(NO = NO_Conc_art_corrected,"NO[2]" = NO2_Conc_diode) %>%
  pivot_longer(c(NO,"NO[2]",CH1_Hz,CH1_zero)) %>%
  ggplot(aes(date,value,col = Control_Temp)) +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free_y",labeller = label_parsed) +
  scale_x_datetime(date_breaks = "1 week",date_labels = "%d/%m/%y") +
  scale_colour_viridis_c() +
  labs(x = "Datetime (UTC)",
       y = NULL)

ggsave("nox_sep.png",
       path = "output/plots/nox_overview_plots_oct23",
       width = 30,
       height = 12,
       units = 'cm')


# Calibrations ------------------------------------------------------------

cal_dat %>% 
  pivot_longer(c(CE,CE_diode)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point() +
  scale_x_datetime(date_breaks = "1 month",date_labels = "%d/%m/%y")

ggsave("nox_ce.png",
       path = "output/plots/nox_overview_plots_oct23",
       width = 30,
       height = 12,
       units = 'cm')


# There's something wrong with NO2 ----------------------------------------

all_dat %>% 
  filter(date > "2023-09-04",
         NO2_Conc < 200) %>% 
  pivot_longer(c(Zero_mean,NO_Hz_mean,NOx_Hz_mean,NOx_Hz_diode_mean)) %>% 
  ggplot(aes(date,value)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free_y") +
  scale_x_datetime(date_breaks = "2 day",date_labels = "%d/%m/%y")

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
         no_ppt = case_when(no_flag != 0 & no_flag != 0.147 ~ NA_real_,
                            no_ppt > 50 ~ NA_real_,
                            TRUE ~ no_ppt),
         no2_diode_ppt = case_when(no2_diode_flag != 0 & no2_diode_flag != 0.147 ~ NA_real_,
                            no2_diode_ppt > 50 ~ NA_real_,
                            TRUE ~ no2_diode_ppt)
  ) %>%
  ggplot(aes(date,no_ppt)) +
  theme_bw() +
  geom_path() +
  facet_wrap(ncol =1, vars(year),scales = "free") +
  labs(y = "Mixing ratio (ppt)",
       x = NULL,
       col = NULL) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

ggsave("no_yearly_comparison.svg",
       path = "output/plots/nox_overview_plots_oct23",
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