#archived nox check

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
