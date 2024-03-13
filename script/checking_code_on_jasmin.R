library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')


# Reading in data ---------------------------------------------------------

#processed data
setwd("~/Cape Verde/nox/processing")

dat23 = read.csv("processed_data/NOx_2023_calc_df.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  select(date,everything(),-X)

dat23_jaz = read.csv("D:/Cape Verde/NOx_2023_calc_df.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  select(date,everything(),-X) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_jaz")},	
               .cols=-date)

#raw data
setwd("D:/Cape Verde/nox_raw_data")

files = list.files(pattern = "z_230721", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    # timeAverage("5 min") %>%
    tibble()
  
}

raw_dat230721 = bind_rows(datList) %>%
  mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant()

raw_dat1 = read.table("NOxy_all_1hz_230721_011146",header=TRUE,sep = ",") %>% 
  mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
  rename(date = TheTime) %>%
  # timeAverage("5 min") %>%
  tibble()


# Plotting data -----------------------------------------------------------

comparison = left_join(dat23,dat23_jaz)

comparison %>% 
  pivot_longer(c(NO_Conc_art_corrected,NO_Conc_art_corrected_jaz)) %>%
  # pivot_longer(c(NO2_Conc_diode,NO2_Conc_diode_jaz)) %>%
  mutate(month = month(date),
         flag = case_when(date > "2023-02-01" & date < "2023-02-04" ~ "Feb",
                          date > "2023-02-27" & date < "2023-03-03" ~ "March",
                          date > "2023-07-21" & date < "2023-07-23" ~ "July",
                          date > "2023-08-16" & date < "2023-08-19" ~ "August",
                          date > "2023-08-30" & date < "2023-09-05" ~ "September",
                          date > "2023-11-11" & date < "2023-11-15" ~ "November",
                          date > "2023-12-17" & date < "2023-12-20" ~ "December",
                          TRUE ~ "null")) %>%
  filter(flag != "null") %>%
  ggplot(aes(date,value,col = name)) +
  geom_point() +
  # scale_x_datetime(date_breaks = "2 hour",date_labels = "%H:%M") +
  theme(legend.position = "top") +
  facet_wrap(~flag,scales = "free") +
  NULL
  
  # mutate(no2_diff = NO2_Conc_diode - NO2_Conc_diode_jaz,
  #        test = ifelse(no2_diff == 0,"zero","not zero")) %>%
  # ggplot(aes(date,no2_diff,col = test)) +
  # geom_point()
  
  # ggplot(aes(NO_Conc_art_corrected,NO_Conc_art_corrected_jaz)) +
  # geom_point() +
  # geom_abline(slope = 1,col = "red")

ggsave('jaz_difference.png',
       path = "ebas/ebas_submission23/nox_qa_qc_plots",
       width = 29,
       height = 12,
       units = 'cm')

raw_dat1 %>% 
  # filter(date > "2023-07-21" & date < "2023-07-22") %>% 
  ggplot(aes(date,CH1_Hz)) +
  scale_x_datetime(date_breaks = "2 hour",date_labels = "%H:%M") +
  geom_point()

