library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

Sys.setenv(TZ = 'UTC')


# Checking processing code with PMT temperature filtered and not ----------

#reading in processed data (filtered to remove when pmt temp is above -27 degrees)
setwd("~/Cape Verde/nox/processing/data/pmt_27")

processed_dat = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min")

#reading in processed data (with no filtering for pmt temp)
setwd("~/Cape Verde/nox/processing/data/no_pmt_filter")

processed_dat = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min")
# filter(date > "2023-06-01")

#raw data with parameters - updated until 8th August, update as necessary
setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

dat23 = read.csv("output/raw_dat23.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("5 min") %>% 
  filter(date < "2023-08-01 16:28")


# Rename and rearrange dfs ------------------------------------------------

raw_dat_organised = raw_dat %>% 
  select(date,raw_counts = CH1_Hz,lab_temp = Control_Temp,pmt_temp = PMT_Temp,rxn_cell_temp = Rxn_Cell_Temp,
         rxn_cell_pressure = Rxn_Vessel_Pressure,cal_flag = NOx_cal)

processed_dat = processed_dat %>% 
  select(date, background_counts = Zero_mean,no_counts = NO_Hz_mean,nox_blc_counts = NOx_Hz_mean,
         nox_diode_counts = NOx_Hz_diode_mean,no = NO_Conc_art_corrected,no2_blc = NO2_Conc_art_corrected,
         no2_diode = NO2_Conc_diode,sens = SENS,ce_blc = CE, ce_diode = CE_diode)

processed_dat_unfiltered_organised = processed_dat_unfiltered %>% 
  select(date, background_counts_u = Zero_mean,no_counts_u = NO_Hz_mean,nox_blc_counts_u = NOx_Hz_mean,
         nox_diode_counts_u = NOx_Hz_diode_mean,no_u = NO_Conc_art_corrected,no2_blc_u = NO2_Conc_art_corrected,
         no2_diode_u = NO2_Conc_diode,sens_u = SENS,ce_blc_u = CE, ce_diode_u = CE_diode)

df_list = list(raw_dat_organised,processed_dat,processed_dat_unfiltered_organised)
comparison = df_list %>% reduce(full_join,by = "date")


# Plotting ----------------------------------------------------------------

comparison %>% 
  filter(date > "2023-07-01") %>%
  filter(no2_diode < 300,
         no2_diode_u < 300) %>%
  pivot_longer(c(no2_diode,no2_diode_u)) %>%
  ggplot(aes(date,value,col = pmt_temp)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_grid(rows = vars(name)) +
  NULL

ggsave('no2_diode_filtered_vs_unfiltered_july.png',
       path = "output/plots",
       width = 30,
       height = 12,
       units = 'cm')


# Checking difference in background and zero measurements -----------------

#reading in processed data (filtered to remove when pmt temp is above -27 degrees)
setwd("~/Cape Verde/nox/processing/data/pmt_27")

processed_dat1 = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

#reading in processed data (with no filtering for pmt temp)
setwd("~/Cape Verde/nox/processing/data/no_pmt_filter")

processed_dat_unfiltered1 = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) 
# filter(date > "2023-06-01")

processed_dat2 = processed_dat1 %>% 
  select(date, background_counts = Zero_mean,flagged_background = Zero_bad,background_spikes_removed = Zero_spikes_removed,
         background_diff = Zero_diff,
         no_counts = NO_Hz_mean,nox_blc_counts = NOx_Hz_mean,
         nox_diode_counts = NOx_Hz_diode_mean,no = NO_Conc_art_corrected,no2_blc = NO2_Conc_art_corrected,
         no2_diode = NO2_Conc_diode,sens = SENS,ce_blc = CE, ce_diode = CE_diode)

processed_dat_unfiltered2 = processed_dat_unfiltered1 %>% 
  select(date, background_counts_u = Zero_mean,flagged_background_u = Zero_bad,background_spikes_removed_u = Zero_spikes_removed,
         background_diff_u = Zero_diff,
         no_counts_u = NO_Hz_mean,nox_blc_counts_u = NOx_Hz_mean,
         nox_diode_counts_u = NOx_Hz_diode_mean,no_u = NO_Conc_art_corrected,no2_blc_u = NO2_Conc_art_corrected,
         no2_diode_u = NO2_Conc_diode,sens_u = SENS,ce_blc_u = CE, ce_diode_u = CE_diode)

background = left_join(processed_dat2,processed_dat_unfiltered2) %>% 
  mutate(flagged_background = ifelse(flagged_background == "","True",flagged_background),
         flagged_background_u = ifelse(flagged_background_u == "","True",flagged_background_u))

background %>%  
  filter(date > "2023-02-14",
    # date < "2023-06-01" 
         background_diff_u < 450,
         background_diff < 450) %>% 
  pivot_longer(c(background_diff,background_diff_u)) %>%
  ggplot(aes(date,value)) +
  geom_point() +
  facet_grid(rows = vars(name))

ggsave('difference_between_background_counts.png',
       path = "output/plots",
       width = 30,
       height = 12,
       units = 'cm')
