library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

Sys.setenv(TZ = 'UTC')

# NO2 for Irene -----------------------------------------------------------

dat = read.csv("data/nox_irene.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  remove_empty() %>% 
  remove_constant() %>% 
  clean_names()

no2_dat = dat %>% 
  filter(date > "2023-06-01" & date < "2023-07-05") %>% 
  timeAverage("5 min") %>% 
  select(date,no2 = no2_conc_diode)

write.csv(no2_dat,"output/no2_june23.csv",row.names = FALSE)

no2_dat %>% 
  filter(date > "2023-06-05" & date < "2023-06-12") %>%
  # timeAverage("1 hour") %>% 
  ggplot(aes(date,no2)) +
  geom_path()
