library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

#checking that changes to nox processing code have the desired effects

# Reading in data ---------------------------------------------------------

setwd("~/Cape Verde/nox/processing/data")

processed_dat23_old = read.csv("processed_data_oct23/NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") %>% 
  filter(date > "2023-01-01")

processed_cal = read.csv("processed_data_new/NOx_2023_cal_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  filter(date > "2023-01-01")

# Checking that CE is replaced when titration was too high ----------------

processed_cal %>% 
  pivot_longer(c(CE,CE_diode)) %>% 
  mutate(month = month(date)) %>% 
  ggplot(aes(date,value,col = as.character(month),shape = name)) +
  geom_point()

#it is - missing accomplished


# Checking that cycles are dropped after PAG measurements -----------------

raw_dat23 =raw_dat23 %>% timeAverage("1 hour")

error_dat1 = read.csv("processed_data_oct23/NOx_2023_error_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  rename_with( .fn = function(.x){paste0(.x,"1")},
               .cols=-date)
               
error_dat2 = read.csv("processed_data_2023/NOx_2023_error_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  rename_with( .fn = function(.x){paste0(.x,"2")},
               .cols=-date)

error_dat3 = read.csv("processed_data_new/NOx_2023_error_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  rename_with( .fn = function(.x){paste0(.x,"3")},
               .cols=-date)

df_list = list(raw_dat23,error_dat1,error_dat2,error_dat3)

errors = df_list %>% reduce(full_join, by = "date")

errors %>% 
  pivot_longer(c(LOD_NO_pptv_1h1,LOD_NO_pptv_1h2,LOD_NO_pptv_1h3)) %>% 
  filter(value < 100,
         date > "2023-04-01" & date < "2023-05-01") %>% 
  ggplot(aes(date,value,col = zero_air_valve)) +
  geom_point() + 
  facet_grid(rows = vars(name))

#fixed python code, removing a generous amount of cycles - could maybe look into removing a few less?
#removing more was necessary to get rid of the spikes seen in hourly LOD col

# Checking that new code isn't affecting data -----------------------------

processed_dat23_old = read.csv("processed_data_oct23/NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") %>% 
  filter(date > "2023-04-01" & date < "2023-05-01") %>% 
  rename_with( .fn = function(.x){paste0(.x,"_old")},
               .cols=-date)

processed_dat23_old_fix = read.csv("processed_data_2023/NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") %>% 
  filter(date > "2023-04-01" & date < "2023-05-01") %>% 
  rename_with( .fn = function(.x){paste0(.x,"_old_fix")},
               .cols=-date)

df_list = list(processed_dat23,processed_dat23_old,processed_dat23_old_fix)

df = df_list %>% reduce(full_join, by = "date")

df %>% 
  pivot_longer(c(SENS,SENS_old,SENS_old_fix)) %>% 
  # filter(value < 100) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point() + 
  # facet_grid(rows = vars(name)) +
  NULL
