library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')


# Reading in processed data 2023 ------------------------------------------

setwd("~/Cape Verde/nox/processing/data/processed_data_new_jan24")

processed_dat23 = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") 

cal_dat23 = read.csv("NOx_2023_cal_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "5 min")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

art_dat23 = read.csv("NOx_2023_art_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "5 min")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

error_dat23 = read.csv("NOx_2023_error_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "5 min")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

zero_dat23 = read.csv("NOx_2023_zero_df.csv") %>% 
  tibble() %>% 
  rename(date = TheTime) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

raw_dat23 = read.csv("output/data/raw_dat23.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(date),
         date = round_date(date, "5 min"))
  
# Checking 2023 data ------------------------------------------------------

processed_dat23 %>% 
  timeAverage("1 hour") %>% 
  # filter(date > "2023-11-01") %>% 
  mutate(NO_Conc_art_corrected = ifelse(NO_Conc_art_corrected > 20,NA_real_,NO_Conc_art_corrected),
         NO2_Conc_diode = ifelse(NO2_Conc_diode >200,NA_real_,NO2_Conc_diode),
         NO2_Conc_art_corrected = ifelse(NO2_Conc_art_corrected >200,NA_real_,NO2_Conc_art_corrected),
         NO2_Conc = ifelse(NO2_Conc > 200,NA_real_,NO2_Conc)) %>%
  rename(NO = NO_Conc_art_corrected,"NO[2]" = NO2_Conc_diode) %>% 
  pivot_longer(c("NO[2]",NO)) %>% 
  ggplot(aes(date,value)) +
  theme_bw() +
  geom_path(linewidth = 0.8) +
  facet_wrap(~name,scales = "free_y",labeller = label_parsed,ncol = 1) +
  labs(x = "Datetime (UTC)",
       y = expression(NO[x]~(ppt))) +
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b")

# Checking NO2 ------------------------------------------------------------

#looking for root of NO2 problem
processed_dat23 %>% 
  timeAverage("1 hour") %>% 
  filter(NO2_Conc_art_corrected < 200,
         NOx_Hz_mean < 12000) %>%
  pivot_longer(c(NOx_Hz_mean,NOx_Hz_spikes_removed,NO2_signal,NO2_Conc,NO2_Conc_art_corrected)) %>% 
  ggplot(aes(date,value)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~factor(name,levels = c("NOx_Hz_mean","NOx_Hz_spikes_removed","NO2_signal","NO2_Conc","NO2_Conc_art_corrected")),
             scales = "free_y",ncol = 1) +
  labs(x = "Datetime (UTC)",
       y = expression(NO[x]~(ppt))) +
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b")

processed_dat23 %>% 
  # filter(date > "2023-11-01") %>% 
  rename(CE_blc = CE) %>% 
  pivot_longer(c(CE_blc,CE_diode)) %>%
  rename(CE = value,ce_name = name) %>% 
  pivot_longer(c(CE,SENS)) %>% 
  mutate(ce_name = ifelse(name == "SENS",NA_real_,ce_name)) %>% 
  ggplot(aes(date,value,col = ce_name)) +
  theme_bw() +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free") +
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b") +
  labs(x = "Datetime (UTC)",
       y = NULL,
       col = NULL) +
  theme(legend.position = "top") +
  NULL

art_dat23 %>% 
  pivot_longer(c(PAG_Zero_NO2_diode_mean,PAG_Zero_NO2_diode_signal,PAG_Zero_NO2_Conc_diode_mean)) %>% 
  ggplot(aes(date,value)) +
  geom_point() +
  facet_wrap(~name,scales = "free_y",ncol = 1) +
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b")

ggsave('no2_diode_pag_artefact.png',
       path = "output/plots/no2_baseline",
       width = 30,
       height = 12,
       units = 'cm')

# Reading in processed data 2022 ------------------------------------------

setwd("~/Cape Verde/nox/processing/data/processed_data_new_jan24")

processed_dat22 = read.csv("NOx_2022_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") 

cal_dat22 = read.csv("NOx_2022_cal_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

art_dat22 = read.csv("NOx_2022_art_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

error_dat22 = read.csv("NOx_2022_error_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

zero_dat22 = read.csv("NOx_2022_zero_df.csv") %>% 
  tibble() %>% 
  rename(date = TheTime) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)
