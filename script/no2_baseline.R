library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

#investigating reasons for NO2 baseline increase at the end of 2023
#and looking into any related issues that come up


# Reading in raw data ---------------------------------------------------------

setwd("~/Cape Verde/nox/processing/nox_r")

#read in raw dataset for 2023
raw_dat23 = read.csv("output/data/raw_dat23.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(date))

#read in 1 second raw cal dataset for 2023
raw_dat23_1s = read.csv("output/data/raw_dat23_cals_1s.csv") %>%
  tibble() %>% 
  mutate(date = ymd_hms(date))

#updating 2024 raw dataset
setwd('C:/Users/anna_/Desktop/harddrive_backup/Cape Verde/nox_raw_data')

files = list.files(pattern = "z_2311", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    filter(zero_air_valve == 1 | NOx_cal == 1) %>%
    # timeAverage("5 min") %>%
    tibble()
  
}

raw_dat2311 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant()

# Reading in processed data -----------------------------------------------

setwd("~/Cape Verde/nox/processing/processed_data")

processed_dat23 = read.csv("processed_data_new_jan24/NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") %>% 
  filter(date > "2023-01-01")

processed_dat24 = read.csv("processed_data_new_jan24/NOx_2024_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("1 hour") %>% 
  filter(date > "2024-01-01")

processed_dat = bind_rows(processed_dat23,processed_dat24) %>% 
  arrange(date)

#reading in CV merge - met data and historical NO2 data to compare to
cv_merge = read.csv("20230827_CV_merge.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  clean_names() %>% 
  select(date,month,sahara:south_atlantic,ws,wd,o3_ppb_v,co_ppb_v)

processed_dat_no2 = processed_dat %>% 
  mutate(no2_ppt = ifelse(NO2_Conc_diode > 200, NA_real_,NO2_Conc_diode),
         year = year(date)) %>%
  select(date,year,no2_ppt)

no2 = ebas_no2 %>% 
  mutate(no2_ppt = ifelse(no2_flag > 0.149, NA_real_,no2_ppb * 1000),
         year = year(date)) %>%
  select(date,year,no2_ppt) %>% 
  bind_rows(processed_dat_no2)

no2_met = no2 %>% left_join(cv_merge,by = "date")

# Reading in artefact data - PAG ------------------------------------------

#changed digit as necessary to read in data from 2022,2023,2024 etc.
art21 = read.csv("processed_data/NOx_2021_art_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  # timeAverage("5 min") %>% 
  filter(date > "2021-01-01")

art = bind_rows(art21,art22,art23)


# Plotting PAG artefacts --------------------------------------------------

art %>% 
  mutate(year = year(date)) %>% 
  filter(year != 2024) %>% 
  filter(PAG_Zero_NO2_Conc > -200,
         PAG_Zero_NO2_Conc < 400) %>%
  ggplot(aes(date,PAG_Zero_NO2_Conc)) +
  geom_point() +
  facet_wrap(~year,ncol = 1,scales = "free")

ggsave('blc_pag_conc_21to23.png',
       path = "output/plots/no2_baseline",
       width = 30,
       height = 12,
       units = 'cm')

# Plotting processed data -------------------------------------------------

#looking at CE and SENS
processed_dat %>% 
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

processed_dat24 %>% 
  timeAverage("1 hour") %>%
  filter(date > "2023-09-01") %>%
  mutate(NO2_BLC = ifelse(NO2_Conc_art_corrected < 200 & NO2_Conc_art_corrected > 0,
                                         NO2_Conc_art_corrected,NA_real_),
         NO = ifelse(NO_Conc_art_corrected < 50,
                                        NO_Conc_art_corrected,NA_real_),
         NO2_diode = ifelse(NO2_Conc_diode < 200 & NO2_Conc_diode > 0,
                                 NO2_Conc_diode,NA_real_)) %>%
  pivot_longer(c(NO2_diode,NO)) %>% 
  ggplot(aes(date,value)) +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free")

#daily NO2 values plotted against doy coloured by year
no2_met %>% 
  mutate(flag = case_when(ws < 2 ~ 1,
                          wd > 100 & wd < 340 ~ 2,
                          TRUE ~ 0),
         no2_ppt = ifelse(flag != 0, NA_real_,no2_ppt),
         doy = yday(date),
         month = month(date)) %>% 
  timeAverage("1 day") %>% 
  filter(year != 2018, year != 2024,year != 2022,
         month != 1) %>% 
  ggplot(aes(doy,no2_ppt,col = as.character(year))) +
  geom_path(linewidth = 0.8) +
  # facet_wrap(~year,ncol = 1,scales = "free") +
  # scale_x_datetime(date_breaks = "1 month",date_labels = "%b") +
  scale_colour_viridis_d() +
  NULL

#timevariation for no2 across years with no2 measurements at the end of the year
no2 %>% 
  filter(year != 2018, year != 2024,year != 2022) %>%
  timeVariation(pollutant = "no2_ppt",group = "year")

#no2 diode and no2 blc timeseries for 2023 and 2024
processed_dat24 %>% 
  mutate(Diode = ifelse(NO2_Conc_diode > 100, NA_real_,NO2_Conc_diode),
         BLC = ifelse(NO2_Conc_art_corrected > 100,NA_real_,NO2_Conc_art_corrected),
         year = year(date),
         doy = yday(date)) %>%
  pivot_longer(c(Diode,BLC)) %>% 
  ggplot(aes(date,value)) +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free",labeller = label_parsed) +
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b") +
  labs(x = "Datetime (UTC)",
       y = expression(NO[2]~(ppt))) +
  NULL

ggsave('nox_24.png',
       path = "output/plots/no2_baseline",
       width = 30,
       height = 12,
       units = 'cm')


# Plotting raw data -------------------------------------------------------

raw_dat2312 %>% 
  filter(CH1_Hz > 0,
         date > "2023-12-21 04:00" & date < "2023-12-21 06:00",
         NOx_cal == 0,
         # CH1_Hz < 5000 & CH1_Hz > 2000
  ) %>% 
  mutate(flag = case_when(zero_air_valve == 1 ~ "PAG",
                          TRUE ~ "Sample")) %>%
  # timeAverage("5 min") %>% 
  ggplot(aes(date,CH1_Hz,col = flag)) +
  geom_point() +
  theme(legend.position = "top") +
  labs(col = NULL) +
  # scale_x_datetime(date_breaks = "2 days",date_labels = "%b %d") +
  NULL

raw_dat2312 %>% 
  filter(CH1_Hz > 0,
         date > "2023-12-21 04:00" & date < "2023-12-21 06:00",
         NOx_cal == 0,
         # CH1_Hz < 5000 & CH1_Hz > 2000
         ) %>% 
  mutate(flag = case_when(zero_air_valve == 1 ~ "PAG",
                          TRUE ~ "Sample")) %>%
  # timeAverage("5 min") %>% 
  ggplot(aes(date,CH1_Hz,col = flag)) +
  geom_point() +
  theme(legend.position = "top") +
  labs(col = NULL) +
  # scale_x_datetime(date_breaks = "2 days",date_labels = "%b %d") +
  NULL

ggsave("20april23.png",
       path = "output/plots/no2_baseline/pag_no_cal_gas",
       width = 10,
       height = 12,
       units = 'cm')

raw_dat22 %>% 
  filter(CH1_Hz > 0,
         CH1_Hz < 10000,
         date < "2022-11-01",
         NOx_cal == 0) %>% 
  mutate(flag = case_when(zero_air_valve == 1 ~ "PAG",
                          TRUE ~ "Sample")) %>% 
  ggplot(aes(date,CH1_Hz,col = flag)) +
  geom_point() +
  theme(legend.position = "top") +
  labs(col = NULL) +
  # scale_x_datetime(date_breaks = "2 days",date_labels = "%b %d") +
  NULL

raw_dat23 %>% 
  filter(CH1_Hz > 2500 & CH1_Hz < 10000,
         NOx_cal == 0,
         date > "2023-02-01") %>%
  ggplot(aes(date,CH1_Hz,col = zero_air_valve)) +
  scale_colour_viridis_c() +
  geom_point() +
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b") +
  NULL

ggsave("raw_dat23.png",
       path = "output/plots/no2_baseline/pag",
       width = 30,
       height = 12,
       units = 'cm')

raw_dat21_1s_cal %>% 
  mutate(measurement_type = case_when(diodes == 1 ~ "Diodes",
                                      NO2_converter == 1 ~ "BLC",
                                      zero_valve_1 == 1 ~ "Zero",
                                      TRUE ~ "NO")) %>% 
  filter(NOx_cal == 0,
         # date > "2021-10-01" & date < "2021-10-04"
         diodes == 1,
         # CH1_Hz < 10000
         ) %>% 
  # timeAverage("5 min") %>% 
  ggplot(aes(date,CH1_Hz,col = Rxn_Vessel_Pressure)) +
  geom_point()

raw_dat23_1s %>%
  filter(NOx_cal == 0,
         diodes == 1,
         # date > "2023-08-01" & date < "2023-09-01"
         ) %>% 
  # timeAverage("5 min") %>% 
  ggplot(aes(date,CH1_Hz,col = Rxn_Vessel_Pressure)) +
  geom_point()

raw_dat22_1s %>% 
  filter(NOx_cal == 0,
         diodes == 1,
         CH1_Hz < 20000
         # date > "2022-02-01" & date < "2022-03-01"
  ) %>% 
  # timeAverage("5 min") %>%
  ggplot(aes(date,CH1_Hz,col = Rxn_Vessel_Pressure)) +
  geom_point()

raw_dat23 %>% 
  filter(NOx_cal == 0,
         CH1_Hz > 2500 & CH1_Hz < 10000,
         Rxn_Vessel_Pressure < 1,
         date > "2023-08-25" & date < "2023-08-27") %>% 
  ggplot(aes(date,CH1_Hz,col = PMT_Temp)) +
  geom_point() +
  scale_x_datetime(date_breaks = "2 days",date_labels = "%b %d") +
  NULL

#looking at raw data (1s data) for 18th September (and related)
raw_dat2309 %>% 
  filter(date > "2023-09-18 20:16" & date < "2023-09-21") %>% 
  mutate(measurement_type = case_when(diodes == 1 ~ "Diodes",
                                      NO2_converter == 1 ~ "BLC",
                                      zero_valve_1 == 1 ~ "Zero",
                                      TRUE ~ "NO")) %>% 
  ggplot(aes(date,CH1_Hz,col = Rxn_Vessel_Pressure)) +
  geom_point() +
  # scale_x_datetime(date_breaks = "1 month",date_labels = "%b") +
  NULL

ggsave("diode_pag23_temp.png",
       path = "output/plots/no2_baseline",
       width = 30,
       height = 12,
       units = 'cm')


# Raw PAG measurements and temperature ------------------------------------

raw_dat2311 %>% 
  filter(NOx_cal == 1,
         CH1_Hz > 0,
         date > "2023-11-27" & date < "2023-11-30") %>% 
  ggplot(aes(date,CH1_Hz,col = NO_Cal_flow)) +
  geom_point() +
  scale_colour_viridis_c()

raw_dat2312 %>% 
  filter(NOx_cal == 1,
         CH1_Hz > 0,
         date > "2023-12-09" & date < "2023-12-10") %>% 
  ggplot(aes(date,CH1_Hz,col = NO_Cal_flow)) +
  geom_point() +
  scale_colour_viridis_c()

ggsave("26nov.png",
       path = "output/plots/no2_baseline/cals/november",
       width = 30,
       height = 12,
       units = 'cm')

raw_dat23_pag = raw_dat23 %>% 
  filter(zero_air_valve == 1)

raw_dat22_pag = raw_dat22 %>% 
  filter(zero_air_valve == 1)

raw_dat_pag = bind_rows(raw_dat21_pag,raw_dat22_pag,raw_dat23_pag)

raw_dat_pag %>% 
  mutate(year = year(date)) %>% 
  filter(CH1_Hz < 40000) %>% 
  ggplot(aes(date,CH1_Hz))+
  geom_point() +
  facet_wrap(~year,ncol = 1,scales = "free")

ggsave("pag_21to23.png",
       path = "output/plots/no2_baseline",
       width = 30,
       height = 12,
       units = 'cm')


# Raw and processed data --------------------------------------------------

dat = raw_dat23 %>% 
  select(-c(no2_ch1_5min:NOy_conc_nocyclone,Bypass_prereactor:no_conc_avg)) %>% 
  filter(date > "2023-02-01") %>% 
  left_join(processed_dat23,by = "date")

dat %>% 
  mutate(pag = ifelse(zero_air_valve == 1,CH1_Hz,NA_real_),
         cal_gas = ifelse(NOx_cal == 1, round(NO_Cal_flow),NA_real_)) %>% 
  fill(cal_gas,.direction = "down") %>% 
  pivot_longer(c(CE_diode,SENS,pag,CE)) %>% 
  ggplot(aes(date,value,col = as.character(cal_gas))) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free")

