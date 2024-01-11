library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

#for checking NOx data and its parameters

# Read in processed data ------------------------------------------------------------

setwd("~/Cape Verde/nox/processing/data")

processed_dat23 = read.csv("processed_data_2023/NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") %>% 
  filter(date > "2023-01-01")

processed_dat24 = read.csv("processed_data/NOx_2024_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min")

processed_dat = bind_rows(processed_dat23,processed_dat24) %>% 
  arrange(date)

# Reading in raw data -----------------------------------------------------

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

raw_dat23 = read.csv("output/data/raw_dat23.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(date))

# #updating 2024 raw data
setwd('E:/Cape Verde/data/nox_raw_data')
 
files = list.files(pattern = "z_2401", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {

  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    # timeAverage("5 min") %>%
    tibble()

}

raw_dat2401 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant()
#   
# setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")
# 
# initial_raw_dat23 = read.csv("output/data/raw_dat23.csv") %>% 
#   tibble() %>% 
#   mutate(date = ymd_hms(date))
# 
# raw_dat23 = bind_rows(initial_raw_dat23,raw_dat2311,raw_dat2312) %>% 
#   select(-c(NO_Conc,NO2_CE,NO2_CE_diodes,NO2_conc_diodes)) %>% 
#   remove_constant() %>% 
#   remove_empty() %>% 
#   arrange(date) %>% 
#   filter(date > "2023-01-01" & date < "2024-01-01")

# write.csv(raw_dat23,"output/data/raw_dat23.csv",row.names =  FALSE)

# all_dat = left_join(processed_dat,raw_dat23,by = "date")

# write.csv(all_dat23_pmt,"output/processed_and_raw_data.csv",row.names =  FALSE)

# Checking noxy parameters after pump tip seals were changed --------------

raw_dat2311 %>%
  # timeAverage("1 hour") %>%
  filter(
    # date > "2023-09-20",
    NOx_cal == 0,
    CH1_Hz > 0,
    # CH1_Hz < 5000,
    CH1_zero > 0) %>% 
  pivot_longer(c(Control_Temp,PMT_Temp,CH1_Hz,CH1_zero)) %>%
  ggplot(aes(date,value)) +
  geom_point() +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c() +
  NULL

#removed data between 18/09/23 13:30 to 19/09/23 19:00 when pump fan wasn't working

#spikes in zero measurements are associated with calibrations
all_dat %>%
  # timeAverage("1 hour") %>% 
  filter(date > "2023-10-07",
         NOx_cal == 0,
         CH1_Hz > 0,
         CH1_zero > 0) %>%
  pivot_longer(c(CH1_zero,CH1_Hz,NO_Conc_art_corrected,NO2_Conc_diode,NO2_Conc_art_corrected)) %>%
  ggplot(aes(date,value,col = PMT_Temp)) +
  geom_point() +
  scale_x_datetime(date_breaks = "4 days",date_labels = "%d/%m") +
  facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c() +
  NULL

#haven't done this for PAG data because it's in the artefact df and I couldn't be bothered - sorry!
ggsave('rxn_cell_pressure_vs_no_night_artefact.png',
       path = "output/plots/artefact",
       width = 30,
       height = 12,
       units = 'cm')

# Plotting for temperature issues summer 2023 -----------------------------

#look at PMT and lab temperatures
all_dat23_pmt %>% 
  filter(date > "2023-02-07") %>% 
  mutate(sensitivity = na.approx(SENS,na.rm = F)) %>% 
  filter(CH1_zero > 0,
         CH1_Hz > 0,
         NOx_cal == 0) %>%
  pivot_longer(c(CH1_Hz,CH1_zero)) %>% 
  ggplot(aes(date,value,col = sensitivity)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c() +
  NULL

ggsave('pmt_lab_temp.png',
       path = "output/plots/temperature_summer23",
       width = 30,
       height = 12,
       units = 'cm')



# Too much titration! -----------------------------------------------------

raw_dat2312 %>% 
  filter(NOx_cal == 1,
         date > "2023-12-02" & date < "2023-12-05") %>% 
  ggplot(aes(date,CH1_Hz,col = Titration_lamp)) +
  geom_point()

processed_dat %>% 
  # filter(date > "2023-09-01") %>%
  # pivot_longer(c(CE,CE_diode)) %>%
  ggplot(aes(date,NO_cal_flow_mean)) +
  geom_point()

files = list.files(pattern = "z_2304", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    # timeAverage("5 min") %>% 
    tibble()
  
}

raw_dat2304 = bind_rows(datList) %>%
  # mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant() 

raw_dat2304 %>% 
  filter(NOx_cal == 1,
         date < "2023-04-04"
  ) %>% 
  ggplot(aes(date,CH1_Hz,col = Titration_lamp)) +
  geom_point()


# PAG problems ------------------------------------------------------------

raw_dat2312 %>% 
  mutate(cal = ifelse(NOx_cal == 1 | zero_air_valve == 1, 1, 0)) %>% 
  filter(CH1_Hz < 8000,
         CH1_Hz > 0,
         # cal == 1,
         date < "2023-12-04 02:40" & date > "2023-12-04") %>% 
  # timeAverage("1 hour") %>% 
  # pivot_longer(c(Control_Temp,PMT_Temp,Rxn_Vessel_Pressure,CH1_Hz)) %>% 
  ggplot(aes(date,CH1_Hz,col = zero_air_valve)) +
  geom_point() +
  # facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c()

# ggsave('data_parameters_nov23.png',
#        path = "output/plots/pag_problems_plots",
#        width = 30,
#        height = 12,
#        units = 'cm')

all_dat %>%
  mutate(NO_Conc_art_corrected = ifelse(NO_Conc_art_corrected < 20,NO_Conc_art_corrected,NA_real_),
         NO2_Conc_diode = ifelse(NO2_Conc_diode < 100,NO2_Conc_diode,NA_real_)) %>%
  # timeAverage("1 hour") %>% 
  filter(date > "2023-10-07",
         NOx_cal == 0,
         CH1_Hz > 0,
         CH1_zero > 0) %>%
  pivot_longer(c(CH1_Hz,NO_Conc_art_corrected,NO2_Conc_diode)) %>%
  ggplot(aes(date,value,col = PMT_Temp)) +
  geom_path() +
  scale_x_datetime(date_breaks = "4 days",date_labels = "%d/%m") +
  facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c() +
  NULL

processed_dat %>% 
  filter(date > "2023-11-01") %>% 
  mutate(NO_Conc_art_corrected = ifelse(NO_Conc_art_corrected < 20,NO_Conc_art_corrected,NA_real_),
         NO2_Conc_diode = ifelse(NO2_Conc_diode < 100,NO2_Conc_diode,NA_real_)) %>% 
  pivot_longer(c(NO_Conc_art_corrected,NO2_Conc_diode)) %>% 
  ggplot(aes(date,value)) +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free")

ggsave('all_dat_nov23.png',
       path = "output/plots/pag_problems_plots",
       width = 30,
       height = 12,
       units = 'cm')

processed_dat %>% 
  filter(date > "2023-11-01") %>% 
  pivot_longer(c(CE,CE_diode)) %>% 
  ggplot(aes(date,SENS,col = name)) +
  geom_point()




# Checking when PAG problems started --------------------------------------

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

raw_dat22 = read.csv("output/data/raw_dat22.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(date))

raw_dat22 %>% 
  mutate(cal = ifelse(NOx_cal == 1 | zero_air_valve == 1, 1, 0)) %>% 
  filter(
    # CH1_Hz < 8000,
    # CH1_Hz > 0,
    zero_air_valve == 1,
     date > "2022-03-01" & date < "2022-06-01") %>% 
  # timeAverage("1 hour") %>% 
  # pivot_longer(c(Control_Temp,PMT_Temp,Rxn_Vessel_Pressure,CH1_Hz)) %>% 
  ggplot(aes(date,CH1_Hz,col = zero_air_valve)) +
  geom_point() +
  # facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c()

setwd('E:/Cape Verde/data/nox_raw_data')

files = list.files(pattern = "z_2204", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    # timeAverage("5 min") %>% 
    tibble()
  
}

raw_dat2204 = bind_rows(datList) %>%
  # mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant()

raw_dat2204 %>% 
  mutate(cal = ifelse(NOx_cal == 1 | zero_air_valve == 1, 1, 0)) %>% 
  filter(
    # CH1_Hz < 8000,
    # CH1_Hz > 0,
    NOx_cal == 0,
    date > "2022-04-06 14:30" & date < "2022-04-06 16:00") %>% 
  # timeAverage("1 hour") %>% 
  # pivot_longer(c(Control_Temp,PMT_Temp,Rxn_Vessel_Pressure,CH1_Hz)) %>% 
  ggplot(aes(date,CH1_Hz,col = zero_air_valve)) +
  geom_point() +
  # facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c()

# Checking CE and SENS ----------------------------------------------------

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

# Plotting processed data -------------------------------------------------

#nox data
processed_dat %>% 
  timeAverage("1 hour") %>% 
  # filter(date > "2023-11-01") %>% 
  mutate(NO_Conc_art_corrected = ifelse(NO_Conc_art_corrected > 20,NA_real_,NO_Conc_art_corrected),
         NO2_Conc_diode = ifelse(NO2_Conc_diode >150,NA_real_,NO2_Conc_diode)) %>%
  rename(NO = NO_Conc_art_corrected,"NO[2]" = NO2_Conc_diode) %>% 
  pivot_longer(c(NO,"NO[2]")) %>% 
  ggplot(aes(date,value)) +
  theme_bw() +
  geom_path(linewidth = 0.8) +
  facet_wrap(~name,scales = "free_y",labeller = label_parsed,ncol = 1) +
  labs(x = "Datetime (UTC)",
       y = expression(NO[x]~(ppt))) +
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b")

ggsave('ce_2023.png',
       path = "output/plots/nox_plots_2023",
       width = 30,
       height = 13,
       units = 'cm')

diurnal = processed_dat %>% 
  mutate(NO_Conc_art_corrected = ifelse(NO_Conc_art_corrected > 20,NA_real_,NO_Conc_art_corrected),
         NO2_Conc_diode = ifelse(NO2_Conc_diode >150,NA_real_,NO2_Conc_diode)) %>%
  rename(NO = NO_Conc_art_corrected,NO2 = NO2_Conc_diode) %>%
  timeVariation(pollutant = c("NO"),type = "month")

diurnal_dat = diurnal$data$hour %>% 
  ungroup() %>% 
  pivot_wider(names_from = variable,values_from = Mean) %>% 
  group_by(hour) %>% 
  summarise(NO = mean(NO,na.rm = T),
            NO2 = mean(NO2,na.rm = T))

diurnal_dat %>%
  ggplot(aes(hour,HONO)) +
  geom_path(size = 0.75,col = "steelblue1") +
  geom_ribbon(aes(ymin = HONO - hono_err,ymax = HONO + hono_err),alpha = 0.25,fill = "steelblue1") +
  # facet_grid(rows = vars(variable),scales = "free_y",labeller = label_parsed) +
  scale_colour_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "HONO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # ylim(-1,13) +
  theme(legend.position = "top")

ggsave('ce_sens.png',
       path = "output/plots/nox_plots_2023",
       width = 30,
       height = 13,
       units = 'cm')
