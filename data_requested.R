library(tidyverse)
library(lubridate)
library(openair)
library(janitor)

Sys.setenv(TZ = 'UTC')


# Functions ---------------------------------------------------------------

select_dates = function(dat,date1,date2) {
  
  #create new df in between selected dates
  #convert cols so that the type of measurement is defined in a specific column
  new_df = dat %>% 
    filter(date >= date1,date < date2) %>% 
    mutate(just_NO = ifelse(NO2_converter == 0 & zero_valve_1 == 0 & diodes == 0,1,0)) %>% 
    pivot_longer(cols = c(NO2_converter,just_NO,zero_valve_1,diodes)) %>% 
    filter(value == 1) %>% 
    select(-value)
  return(new_df)
}
create_plot = function(dat,title = NULL) {
  
  ggplot(dat,aes(date,CH1_Hz,col = name)) +
    geom_point() +
    theme_bw() +
    labs(x = 'Time',
         y = 'Raw data / Hz',
         title = title,
         color = 'Type of measurement') +
    scale_x_datetime(date_labels = "%b %d",date_breaks = '4 days',date_minor_breaks = '1 day') +
    scale_color_manual(labels = c(expression(NO[2]~diodes),'NO',expression(NO[2]~BLC),'Background'),
                       values = c('navy blue','magenta','purple','turquoise')) +
    theme(legend.position = 'bottom')
  
}

# NO2 for Irene -----------------------------------------------------------
setwd('~/Cape Verde/nox/processing/initial_processing/nox_r')

dat = read.csv("data/nox_irene.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date)

no2_dat = dat %>% 
  clean_names() %>% 
  filter(date > "2023-06-01" & date < "2023-07-05") %>%
  timeAverage("5 min") %>% 
  select(date,no2 = no2_conc_diode, no = no_conc_art_corrected,no2_blc= no2_conc_art_corrected,zero = zero_mean)

write.csv(no2_dat,"output/no2_june23.csv",row.names = FALSE)

#checking large spike on 14/06, only in NO2 and quite long
no2_dat %>% 
  pivot_longer(c(no2,no,no2_blc)) %>%
  filter(date > "2023-06-13" & date < "2023-06-15") %>%
  # timeAverage("1 hour") %>% 
  ggplot(aes(date,no2)) +
  geom_path()

#reading in raw data -> parameters for that period
setwd('D:/Cape Verde/data/nox_raw_data')

files = list.files(pattern = "z_2306", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble()
  
}

raw_dat_june = bind_rows(datList) %>%
  timeAverage("5 min") %>%
  remove_empty() %>%
  remove_constant()

#joining parameters and processed data
temp_no2_dat = left_join(no2_dat,raw_dat_june)

temp_no2_dat %>% 
  filter(date > "2023-06-14" & date < "2023-06-14 06:00") %>%
  pivot_longer(c(no2,no2_blc,no)) %>%
  ggplot(aes(date,value,col = PMT_Temp)) +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free_y") +
  scale_color_viridis_c()

#checking that spike was just in NO2 (code from first year!)

data_for_analysis = raw_dat_june %>%
  filter(PMT_Temp < -28,
         NOx_cal == 0,
         NO_valve == 0,
         CH1_Hz > -1000) %>%
  select(c(date,CH1_Hz,zero_valve_1,NO2_converter,diodes))

create_plot(select_dates(data_for_analysis,'2023-06-14 00:00','2023-06-14 06:00'))


