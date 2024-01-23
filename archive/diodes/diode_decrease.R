library(tidyverse)
library(lubridate)
library(ggplot2)
library(openair)

setwd("~/Cape Verde/nox/processing/data")

files = list.files("processed_data",pattern = "cal_",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  datList[[index]] = read.csv(files[index],header=TRUE,na.strings= c('NA','missing'))%>%
    tibble()
  
}

dat = bind_rows(datList) %>%
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 min"))

dat %>%
  pivot_longer(c(CE,CE_diode,SENS)) %>% 
  # filter(CE_diode > 0.4) %>% 
  ggplot(aes(date,value)) +
  geom_point() +
  facet_grid(rows = vars(name),scale = "free_y") +
  scale_x_datetime(date_breaks = "6 month",date_labels = "%m/%y")

ggsave("ce_diodes_19-23.png",
       path = "~/Cape Verde/nox/processing/problem_periods/diodes",
       width = 30,
       height = 12,
       units = 'cm')


# Checking raw data -------------------------------------------------------

setwd("D:/Cape Verde/data/nox_raw_data")

tidy_rle = function(rleObj){
  
  require(dplyr)
  
  rleDf = tibble(lengths = rleObj$lengths,
                 values = rleObj$values) %>% 
    mutate(idxStart = cumsum(c(1,lengths))[1:(nrow(.))],
           idxEnd = cumsum(lengths))
  
  #
  rleDf
}

files = list.files(pattern = "z_2305",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  datList[[index]] = read.csv(files[index],header=TRUE,na.strings= c('NA','missing'))%>%
    tibble()
  
}

raw_dat = bind_rows(datList) %>% 
  rename(date = TheTime) %>% 
  mutate(date = waclr::parse_excel_date(date))

cal = rle(raw_dat$NOx_cal) %>%
  tidy_rle() %>% 
  filter(values != 0) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% #df with row numbers and corresponding groups
  tibble() 

cal_flagged = raw_dat %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(cal, "idx") %>% #joins two dfs by their row number
  mutate(id = ifelse(is.na(id), 0, id)) %>% 
  select(date,CH1_Hz,NOx_cal,id)


cal_flagged %>% 
  filter(NOx_cal != 0) %>% 
  ggplot(aes(date,CH1_Hz)) +
  geom_point() +
  facet_wrap(vars(id),scales = "free",ncol = 1)
