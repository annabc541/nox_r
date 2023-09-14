library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

Sys.setenv(TZ = 'UTC')


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

setwd("~/Cape Verde/nox/processing/data/processed_data")

artifact22 = read.csv("NOx_2022_art_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

setwd('D:/Cape Verde/data/processed_data_simone')

files = list.files(pattern = "art", full.names=TRUE)
datList = list()

artifact22 = read.csv("NOx_2022_NOx_art_df_new_altered_night(21-03)_v17.csv") %>% 
  tibble() %>% 
  rename(date = DateTime) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    tibble() %>% 
    rename(date = DateTime) %>% 
    mutate(date = ymd_hms(date),
           date = round_date(date, "1 sec")) %>% 
    remove_empty() %>% 
    remove_constant() %>%
    arrange(date)
  
}

artifact14_22 = bind_rows(datList) %>% 
  arrange(date)

all_artifacts = bind_rows(artifact14_22,artifact23) %>% 
  arrange(date)


# 2022 --------------------------------------------------------------------

artifact22_new_test = artifact22_new %>% 
  rename_with( .fn = function(.x){paste0(.x,"_new")},
               .cols=-date)

artifact22_old = artifact22_simone %>% 
  rename_with( .fn = function(.x){paste0(.x,"_new")},
               .cols=-date)


# Plotting ----------------------------------------------------------------

artifact23 %>% 
  timeAverage("5 min") %>% 
  # filter(date > "2023-04-01") %>% 
  rename(zero = Zero_artefact_mean,
         no_raw = PAG_Zero_NO_mean,
         no_signal = PAG_Zero_NO_signal,
         no_conc = PAG_Zero_NO_Conc,
         no_conc2 = PAG_Zero_NO_Conc_mean,
         blc = PAG_Zero_NO2_signal,
         diode = PAG_Zero_NO2_diode_signal) %>%
  mutate(no_signal_r = no_raw - zero) %>% 
  pivot_longer(c(no_signal,no_signal_r)) %>% 
  ggplot(aes(date,no_conc2)) +
  geom_point() +
  # facet_grid(rows = vars(name),scales = "free_y") +
  NULL


all_artifacts %>%
  mutate(year = year(date)) %>% 
  pivot_longer(c(PAG_Zero_NO_signal,PAG_Zero_NO2_signal,PAG_Zero_NO2_diode_signal)) %>% 
  ggplot(aes(date,value)) + 
  geom_point() +
  facet_grid(rows = vars(name),scales = "free_y") +
  NULL
