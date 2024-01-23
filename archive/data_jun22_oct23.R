library(tidyverse)
library(lubridate)
library(zoo)
library(SciViews)

setwd("~/Cape Verde/nox/processing")

# Read in data 2022 -------------------------------------------------------

#reading in calc data, filtering for just reaquired year and selecting necessary columns
#5 minute cycle
calc_dat22 = read.csv("data/processed_data_nov23/NOx_2022_calc_df.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2022-06-01") %>% 
  select(date,
         no_ppt = NO_Conc_art_corrected,no2_diode_ppt = NO2_Conc_diode,no2_blc_ppt = NO2_Conc_art_corrected,
         ce_blc = CE, ce_diode = CE_diode,
         no_night_unc = NO_night_uncertainty, no_art_tot_unc = NO_art_total_uncertainty, no_night_sd = NO_night_sd,no_night_drift_unc = NO_night_drift_uncertainty,no_night_mean = NO_night_mean)

#reading in error data, filtering for just required year and selecting necessary columns
#already hourly
#flagging and uncertainty
err_dat22 = read.csv("data/processed_data_nov23/NOx_2022_error_df.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date > "2022-06-01") %>% 
  select(date,no_lod = LOD_NO_pptv_1h,no2_diode_lod = LOD_NO2_diode_pptv_1h,no2_blc_lod = LOD_NO2_pptv_1h)

#reading in cal data, filtering for just required year and selecting necessary columns
#one value per calibration run
#uncertainty calculation
cal_dat22 = read.csv("data/processed_data_nov23/NOx_2022_cal_df.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date, "1 hour")) %>%
  filter(date > "2022-06-01") %>%  
  select(date,tot_cal_unc_no = Total_calibration_uncertainty_NO,tot_cal_unc_no2_blc = Total_calibration_uncertainty_NO2,tot_cal_unc_no2_diode = Total_calibration_uncertainty_NO2_diode)

#reading in error data, filtering for just required year and selecting necessary columns
#1 minute averaged (why? how?)
#uncertainty calculation
art_dat22 = read.csv("data/processed_data_nov23/NOx_2022_art_df.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date, "1 hour")) %>% 
  filter(date > "2022-06-01") %>% 
  filter(is.na(PAG_NO2_total_uncertainty) == FALSE) %>% 
  select(date,pag_tot_unc = PAG_NO2_total_uncertainty #only one actually used in the code
         # ,pag_blc_unc = PAG_Zero_NO2_Conc_uncertainty,pag_diode_unc = PAG_Zero_NO2_Conc_diode_uncertainty,pag_drift_unc = PAG_Zero_NO2_BLC_art_drift_uncertainty,pag_art = PAG_Zero_NO2_BLC_art,pag_blc_mean = PAG_Zero_NO2_Conc_mean,pag_diode_mean = PAG_Zero_NO2_Conc_diode_mean
  )

#importing met data - ozone, wind speed and direction, hourly averaged
#ozone for correction, ws and wd for flagging
met_data22 = read.csv("ozone_correction/2006-2022_Met_O3_data.csv") %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 hour")) %>% 
  filter(date > "2022-06-01",
         date < "2023-01-01") %>% 
  select(date,ws = WINDSPD_10M,wd = WINDDIR_10M,o3 = O3)

# Read in data 2023 -------------------------------------------------------

#reading in calc data, filtering for just reaquired year and selecting necessary columns
#5 minute cycle
calc_dat23 = read.csv("data/processed_data_nov23/NOx_2023_calc_df.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date < "2023-11-01") %>% 
  select(date,
         no_ppt = NO_Conc_art_corrected,no2_diode_ppt = NO2_Conc_diode,no2_blc_ppt = NO2_Conc_art_corrected,
         ce_blc = CE, ce_diode = CE_diode,
         no_night_unc = NO_night_uncertainty, no_art_tot_unc = NO_art_total_uncertainty, no_night_sd = NO_night_sd,no_night_drift_unc = NO_night_drift_uncertainty,no_night_mean = NO_night_mean)

#reading in error data, filtering for just required year and selecting necessary columns
#already hourly
#flagging and uncertainty
err_dat23 = read.csv("data/processed_data_nov23/NOx_2023_error_df.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
  mutate(date = ymd_hms(X)) %>% 
  filter(date < "2023-11-01") %>% 
  select(date,no_lod = LOD_NO_pptv_1h,no2_diode_lod = LOD_NO2_diode_pptv_1h,no2_blc_lod = LOD_NO2_pptv_1h)

#reading in cal data, filtering for just required year and selecting necessary columns
#one value per calibration run
#uncertainty calculation
cal_dat23 = read.csv("data/processed_data_nov23/NOx_2023_cal_df.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date, "1 hour")) %>%
  filter(date < "2023-11-01") %>% 
  select(date,tot_cal_unc_no = Total_calibration_uncertainty_NO,tot_cal_unc_no2_blc = Total_calibration_uncertainty_NO2,tot_cal_unc_no2_diode = Total_calibration_uncertainty_NO2_diode)

#reading in error data, filtering for just required year and selecting necessary columns
#1 minute averaged (why? how?)
#uncertainty calculation
art_dat23 = read.csv("data/processed_data_nov23/NOx_2023_art_df.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date, "1 hour")) %>% 
  filter(date < "2023-11-01") %>%
  filter(is.na(PAG_NO2_total_uncertainty) == FALSE) %>% 
  select(date,pag_tot_unc = PAG_NO2_total_uncertainty #only one actually used in the code
         # ,pag_blc_unc = PAG_Zero_NO2_Conc_uncertainty,pag_diode_unc = PAG_Zero_NO2_Conc_diode_uncertainty,pag_drift_unc = PAG_Zero_NO2_BLC_art_drift_uncertainty,pag_art = PAG_Zero_NO2_BLC_art,pag_blc_mean = PAG_Zero_NO2_Conc_mean,pag_diode_mean = PAG_Zero_NO2_Conc_diode_mean
  )

#importing met data - ozone, wind speed and direction, hourly averaged
#ozone for correction, ws and wd for flagging
met_data23 = read.csv("Met_2023_hourly.csv") %>% 
  mutate(date = dmy_hm(date),
         date = round_date(date, "1 hour")) %>% 
  filter(date < "2023-11-01") %>% 
  select(date,ws,wd)

# Hourly stats ------------------------------------------------------------

#determining how many measurements have been done each hour and getting hourly mean and median

hourly_calc_dat = bind_rows(calc_dat22,calc_dat23) %>%
  select(date, no_ppt,no2_blc_ppt,no2_diode_ppt,ce_blc,ce_diode,no_art_tot_unc) %>%
  group_by(date = floor_date(date,"1 hour")) %>% 
  summarise(no_count = sum(!is.na(no_ppt)),
            no2_diode_count = sum(!is.na(no2_diode_ppt)),
            no2_blc_count = sum(!is.na(no2_blc_ppt)),
            ce_blc_mean = mean(ce_blc,na.rm = TRUE),
            ce_diode_mean = mean(ce_diode,na.rm = TRUE),
            no_mean = mean(no_ppt,na.rm = TRUE),
            no2_blc_mean = mean(no2_blc_ppt,na.rm = TRUE),
            no2_diode_mean = mean(no2_diode_ppt,na.rm = TRUE),
            no_median = median(no_ppt,na.rm = TRUE),
            no2_blc_median = median(no2_blc_ppt,na.rm = TRUE),
            no2_diode_median = median(no2_diode_ppt,na.rm = TRUE),
            no_art_u = mean(no_art_tot_unc,na.rm = TRUE)) %>% 
  mutate(ce_diode_mean = na.approx(ce_diode_mean,na.rm = FALSE),
         ce_blc_mean = na.approx(ce_blc_mean,na.rm = FALSE)) %>% 
  fill(ce_diode_mean,ce_blc_mean,no_art_u,.direction = "up")

# Flagging data -----------------------------------------------------------

flagged_dat = bind_rows(met_data22,met_data23) %>% #o3 for calc and ws and wd for flagging
  left_join(hourly_calc_dat) %>% 
  left_join(bind_rows(err_dat22,err_dat23)) %>% #lods for flagging
  mutate(no_flag = case_when(no_mean > 50 ~ 0.459, #0.459 denotes extreme value -> no above 50 ppt
                             is.na(no_mean) ~ 0.999, #0.999 missing data
                             wd > 100 & wd < 340 | ws < 2 ~ 0.599, #0.599 for local contamination (ws and wd indicating air coming from over the island)
                             no_count < 6 ~ 0.391, #0.391 for data coverage < 50% (12 measurements an hour)
                             abs(no_mean - no_median) > 4.8 ~ 0.456, #0.456 invalidated by data originator, likely a spike if these two differ too much
                             no_mean < no_lod ~ 0.147, #0.147 indicate that no is below LOD, data point still considered valid)
                             TRUE ~ 0), 
         no2_flag = case_when(no2_diode_mean > 200 ~ 0.459,
                              is.na(no2_diode_mean) ~ 0.999,
                              wd > 100 & wd < 340 | ws < 2 ~ 0.599,
                              no2_diode_count < 6 ~ 0.391,
                              abs(no2_diode_mean - no2_diode_median) > 33.3 ~ 0.456,
                              abs(no2_blc_mean - no2_diode_mean) > 33.7 ~ 0.456, #remove when blc and diode measurements are too different
                              no2_diode_mean < no2_diode_lod ~ 0.147, #always put this last, because data is still valid if this flag applies and I don't want it to override any invalidating flags
                              TRUE ~ 0))

# Uncertainty calculations ------------------------------------------------

#uncertainty of hourly measurements estimated by combining all uncertainties associated with measurements
#ie uncertainties in cal, art, ozone correction and instrument precision
#error propagation = ((err_a^2 + err_b^2)^0.5 * product)

uncertainties = flagged_dat %>% 
  left_join(bind_rows(art_dat22,art_dat23)) %>% 
  left_join(bind_rows(cal_dat22,cal_dat23)) %>% 
  fill(pag_tot_unc,tot_cal_unc_no,tot_cal_unc_no2_blc,tot_cal_unc_no2_diode,.direction = "up") %>%  #for 2021 leave end of year blank as is blank for measurements
  mutate(no_u = ((no_lod)^2 + (tot_cal_unc_no * no_mean)^2 + (no_art_u)^2 )^0.5,
         no2_diode_u = ((no_lod)^2 + (no2_diode_lod)^2 + (tot_cal_unc_no2_diode * no2_diode_mean)^2)^0.5,
         # no2_blc_u = ((no_lod)^2 + (no2_blc_lod)^2 + (tot_cal_unc_no2_blc * no2_corr_blc)^2 + (pag_tot_unc)^2 + (o3_corr_u * no2_corr_blc)^2)^0.5
  ) 

dat_for_utrecht %>%
  pivot_longer(c(no_ppt,no2_ppt,no_uncertainty_ppt,no2_uncertainty_ppt,no_lod_ppt,no2_lod_ppt)) %>% 
  ggplot(aes(date,value)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free_y") +
  scale_colour_viridis_d() +
  theme(legend.position = "top")

dat_for_utrecht = uncertainties %>% 
  mutate(no2_diode_mean = ifelse(date > "2023-09-17",NA_real_,no2_diode_mean),
         no2_diode_lod = ifelse(date > "2023-09-17",NA_real_,no2_diode_lod),
         no2_diode_u = ifelse(date > "2023-09-17",NA_real_,no2_diode_u),
         no2_flag = ifelse(date > "2023-09-17",NA_real_,no2_flag)) %>% 
  select(date,no_ppt = no_mean, no2_ppt = no2_diode_mean,
         no_lod_ppt = no_lod,no2_lod_ppt = no2_diode_lod,
         no_uncertainty_ppt = no_u,no2_uncertainty_ppt = no2_diode_u,
         no_flag,no2_flag)

write.csv(dat_for_utrecht,"cvao_nox_jun22-oct23.csv",row.names = F)

