library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

Sys.setenv(TZ = 'UTC')


# Raw data ----------------------------------------------------------------

#updating 2023 raw data
setwd('D:/Cape Verde/data/nox_raw_data')

files = list.files(pattern = "z_2308", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble()
  
}

raw_dat2308 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant() %>% 
  timeAverage("5 min")

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

dat23 = read.csv("output/raw_dat23.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("5 min") %>% 
  filter(date < "2023-08-01 16:28")


# Processed data ----------------------------------------------------------

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

all_dat23 = left_join(processed_dat,dat23, by = "date")


# Plots -------------------------------------------------------------------

#seeing how temperature changes affect processed data
all_dat23 %>% 
  filter(date > "2023-06-01",
         NOx_cal == 0,
         CH1_Hz > 0,
         NO2_Conc_diode < 500) %>% 
  pivot_longer(c(CH1_Hz,NO2_Conc_diode,PMT_Temp)) %>% 
  ggplot(aes(date,value,col = Control_Temp)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c()

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

ggsave('rawcounts_no2diode_pmt.png',
       path = "output/plots/2023",
       width = 30,
       height = 12,
       units = 'cm')

all_dat23 %>% 
  filter(date > "2023-02-14",
         PMT_Temp < -23,
         NOx_cal == 0,
         CH1_zero > 0) %>% 
  # pivot_longer(c(CH1_zero,Control_Temp,PMT_Temp)) %>% 
  mutate(month = month(date)) %>%
  # filter(PMT_Temp > -50 & PMT_Temp < -20,
  #        CH1_zero > 1000,
  #        date > "2023-02-14") %>% 
  ggplot(aes(PMT_Temp,CH1_zero,col = month)) +
  # facet_grid(rows = vars(name),scales = "free_y") +
  geom_point() +
  scale_colour_viridis_c()

ggsave('pmt_temp_vs_zero_counts_2023.png',
       path = "output/plots/2023",
       width = 30,
       height = 12,
       units = 'cm')
