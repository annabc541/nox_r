library(tidyverse)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

dat = read.csv("output/processed_and_raw_data.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))

met = read.csv("data/met.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(date,ws,wd,temp)

summer = left_join(met,dat) %>% 
  filter(date > "2023-06-01")

summer %>% 
  mutate(flag = case_when(ws < 2 ~ "ws",
                          wd > 100 & wd < 340 ~ "wd",
                          TRUE ~ "good")) %>% 
  filter(NOx_cal == 0,
         CH1_Hz > 0,
         CH1_zero > 0) %>%
  pivot_longer(c(NO_Conc_art_corrected,NO2_Conc_diode)) %>% 
  ggplot(aes(date,value,col = ws)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free_y") +
  scale_colour_viridis_c()

ggsave('nox_summer23_ws.png',
       path = "output/plots",
       width = 30,
       height = 12,
       units = 'cm')

