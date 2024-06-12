library(tidyverse)
library(openair)
library(janitor)

setwd("~/Cape Verde/nox/processing/nox_r")

# Reading in data ---------------------------------------------------------

setwd("~/Cape Verde/nox/processing/processed_data/jasmin")

dat = read.csv("NOx_2024_calc_df.csv") %>% 
  mutate(date = ymd_hms(DateTime)) %>% 
  select(date,everything(),-DateTime) %>% 
  # select(date,NO = NO_Conc_art_corrected,no2 = NO2_Conc_diode) %>% 
  filter(date >= "2024-01-01")

dat_5min = dat %>% 
  timeAverage("5 min")

setwd("D:/Cape Verde/nox_raw_data")

files = list.files(pattern = "z_24", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble() %>% 
    timeAverage("5 min")
  
}

raw_dat = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant()


# Plotting processed NOx timeseries ---------------------------------------

#nox timeseries
dat %>% 
  timeAverage("1 hour") %>%
  rename("NO[2]" = NO2_Conc_diode,
         NO = NO_Conc_art_corrected) %>%
  pivot_longer(c(NO,"NO[2]")) %>%
  ggplot(aes(date,value)) +
  theme_bw() +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free",labeller = label_parsed) +
  scale_x_datetime(breaks = "14 day",date_labels = "%d %b") +
  labs(x = NULL,
       y = expression(NO[x]~(ppt)))

#sensitivity and ce
dat %>% 
  # filter(date > "2024-04-01") %>% 
  # timeAverage("1 hour") %>%
  # rename("NO[2]" = no2) %>% 
  pivot_longer(names_to = "ce_names",values_to = "ce_values",cols = c(CE,CE_diode)) %>% 
  pivot_longer(c(SENS,ce_values)) %>% 
  ggplot(aes(date,value,col = ce_names)) +
  theme_bw() +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free") +
  scale_x_datetime(breaks = "14 day",date_labels = "%d %b") +
  labs(x = NULL,
       y = NULL)

# ggsave('hourly_nox24.svg',
#        path = "output/plots/nox_checks/nox_overview_plots_jun24",
#        width = 30,
#        height = 15.04,
#        units = 'cm')

# Plotting parameters from raw data ---------------------------------------

raw_dat %>% 
  timeAverage("1 hour") %>% 
  mutate(month = month(date)) %>% 
  # filter(month == 5) %>% 
  # filter(NOx_cal == 1) %>% 
  ggplot(aes(date,PMT_Temp,col = Control_Temp)) +
  geom_point() +
  theme_bw() +
  scale_colour_viridis_c() +
  scale_x_datetime(breaks = "1 month",date_labels = "%b") +
  labs(x = NULL,
       y = "Lab temp",
       col = NULL)
 
# ggsave('pmt_temp24.png',
#        path = "output/plots/nox_checks/nox_overview_plots_jun24",
#        width = 27.48,
#        height = 13.77,
#        units = 'cm')


# Diurnal temperature variation -------------------------------------------

raw_dat %>% 
  mutate(month = month(date)) %>% 
  # filter(month == 4) %>% 
  timeVariation(pollutant = "Control_Temp",type = "month")

diurnal_dat = timeVariation(raw_dat,pollutant = c("Control_Temp","PMT_Temp"),type = "month")
diurnal_dat_month = diurnal_dat$data$hour

diurnal_dat_month %>% 
  ggplot(aes(hour,Mean,col = month)) +
  geom_path(size = 1) +
  theme_bw() +
  facet_wrap(~variable,scales = "free") +
  labs(x = "Hour of day",
       y = "Temp",
       col = NULL) +
  theme(legend.position = "top")

# ggsave('diurnal_temp_variation.png',
#        path = "output/plots/nox_checks/nox_overview_plots_jun24",
#        width = 27.48,
#        height = 13.77,
#        units = 'cm')
