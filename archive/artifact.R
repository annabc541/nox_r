library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

Sys.setenv(TZ = 'UTC')

# Comparing new and old 2022 data -----------------------------------------

#I just don't know anymore

setwd("~/Cape Verde/nox/processing/data/processed_data")

artifact22_new = read.csv("NOx_2022_art_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_new")},
               .cols=-date)

setwd('D:/Cape Verde/data/processed_data_simone')

artifact22_old = read.csv("NOx_2022_NOx_art_df_new_altered_night(21-03)_v17.csv") %>% 
  tibble() %>% 
  rename(date = DateTime) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)%>% 
  rename_with( .fn = function(.x){paste0(.x,"_old")},
               .cols=-date)

artifact22 = left_join(artifact22_new,artifact22_old,by = "date") %>% 
  arrange(date)

artifact22 %>% 
  rename(new = PAG_Zero_NO2_diode_signal_new,
         old = PAG_Zero_NO2_diode_signal_old) %>% 
  mutate(diff = abs(old - new)) %>% 
  filter(date < "2022-07-01") %>%
  pivot_longer(c(new,old)) %>%
  ggplot(aes(date,value,col = diff)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free_y") +
  scale_colour_viridis_c()
NULL


# Reading in data ---------------------------------------------------------

setwd("~/Cape Verde/nox/processing/data/no_pmt_filter")

artifact23 = read.csv("NOx_2023_art_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

artifact21 = read.csv("NOx_2021_NOx_art_df_new_altered_night(21-03)_v17.csv") %>% 
  tibble() %>% 
  rename(date = DateTime) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

artifact20 = read.csv("NOx_2020_NOx_art_df_new_altered_night(21-03)_v17.csv") %>% 
  tibble() %>% 
  rename(date = DateTime) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)


# Dataframe prep ----------------------------------------------------------

artifact23_prep = artifact23 %>% 
  select(date,zero1 = Zero_artefact_mean,no1 = PAG_Zero_NO_mean,blc1 = PAG_Zero_NO2_mean,diode1 = PAG_Zero_NO2_mean)

dat = bind_rows(artifact20,artifact21,artifact22_new,artifact23) %>% 
  arrange(date)

