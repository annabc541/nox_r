library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

setwd("~/Cape Verde/nox/processing/processed_data")

processed_dat24 = read.csv("processed_data_new_jan24/NOx_2024_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec"),
         NO = ifelse(NO_Conc_art_corrected < 50,
                     NO_Conc_art_corrected,NA_real_),
         NO2 = ifelse(NO2_Conc_diode < 200 & NO2_Conc_diode > 0,
                          NO2_Conc_diode,NA_real_)) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("1 hour") %>% 
  filter(date > "2024-01-01") %>% 
  select(date,NO,NO2)

write.csv(processed_dat24,"nox_jan24.csv",row.names = F)

processed_dat24 %>% 
  timeAverage("1 hour") %>%
  filter(date > "2024-01-01") %>%
  mutate(NO2_BLC = ifelse(NO2_Conc_art_corrected < 200 & NO2_Conc_art_corrected > 0,
                          NO2_Conc_art_corrected,NA_real_),
         NO = ifelse(NO_Conc_art_corrected < 50,
                     NO_Conc_art_corrected,NA_real_),
         "NO[2]" = ifelse(NO2_Conc_diode < 200 & NO2_Conc_diode > 0,
                          NO2_Conc_diode,NA_real_)) %>%
  pivot_longer(c("NO[2]",NO)) %>% 
  ggplot(aes(date,value)) +
  geom_path() +
  labs(x = "Datetime (UTC)",
       y = expression(NO[x]~(ppt))) +
  facet_grid(rows = vars(name),scales = "free",labeller = label_parsed) +
  scale_x_datetime(date_breaks = "2 day",date_labels = "%d/%m")

setwd("~/Cape Verde/nox/processing/nox_r/jan24_dust")

ggsave("nox_jan24.png",
       # path = "output/plots/no2_baseline/pag_no_cal_gas",
       width = 20,
       height = 12,
       units = 'cm')
