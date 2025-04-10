library(tidyverse)
library(RSocrata)
library(parquetize)
library(arrow)
library(MMWRweek)


source('./R/runIfExpired.R') #function for archiving
source('./R/epic_age_import.R')
source('./R/harmonize_epic.R')

#Creates a time/date stamped parquet file in the folder indicated in runIfExpired. 
#if a file has been downloaded within past week (24*7 hours), 
#it just reads in latest file, otherwise it downloads fresh copy
#Check that formatting is consistent between vintages. (1) check column names (2) check variable formats
#compare newest data to previous dataset


#######################################
###National syndromic surveillance data (CDC) county*week (no age stratification)
#######################################
url_nssp <- "https://data.cdc.gov/resource/rdmq-nq56.csv"

cdc_nssp_ed1 <- runIfExpired('nssp_ed1',
                    ~ read.socrata(url_nssp),maxage=hours(24*7)
)

verify_update(  test_file = cdc_nssp_ed1, ds_path='./Data/nssp_ed1/') %>%
  write_parquet('./Data/live_files/nssp_rsv_county.parquet')

nssp_harmonized <- open_dataset('./Data/live_files/nssp_rsv_county.parquet') %>%
  filter(county=='All'  ) %>%
  rename(state=geography, date='week_end') %>%
  dplyr::select(state, date, percent_visits_rsv) %>%
  collect() %>%
  as.data.frame() %>%
  rename(Outcome_value1=percent_visits_rsv,
         geography=state) %>%
  mutate(outcome_type='ED',
         outcome_label1 = 'Pct of ED visits',
         domain = 'Respiratory infections',
         date_resolution = 'week',
         update_frequency = 'weekly',
         source = 'CDC NSSP',
         url = 'https://healthdata.gov/dataset/NSSP-Emergency-Department-Visit-Trajectories-by-St/hr4c-e7p6/',
         geo_strata = 'state',
         age_strata = 'none',
         race_strata = 'none',
         race_level = NA_character_,
         additional_strata1 = 'none',
         additional_strata_level = NA_character_,
         sex_strata = 'none',
         sex_level = NA_character_)

#######################################
###RespNet (CDC) Flu, COVID RSV, by week, state, race, etc. For some reason age*state missing for RSV
#######################################
url_resp_net <- "https://data.cdc.gov/resource/kvib-3txy.csv"

cdc_respnet <- runIfExpired('respnet',
                             ~ read.socrata(url_resp_net),maxage=hours(24*7)
)

current_data = verify_update(  test_file = cdc_respnet, ds_path='./Data/respnet/') %>%
  write_parquet('./Data/live_files/respnet_hosp.parquet')

h1_harmonized <- open_dataset('./Data/live_files/respnet_hosp.parquet') %>%
  filter( site != 'Overall' & surveillance_network=='RSV-NET' & type=='Unadjusted Rate' & age_group=='Overall' &
            sex=='Overall' & race_ethnicity=='Overall') %>%
  rename(state=site, hosp_rate=weekly_rate, date=X_weekenddate) %>%
  mutate(date=as.Date(substr(date,1,10)) ) %>%
  dplyr::select(state, date, hosp_rate) %>%
  as.data.frame() %>% 
  rename(Outcome_value1=hosp_rate,
         geography=state) %>%
  mutate(outcome_type='Inpatient Hospitalization',
         outcome_label1 = 'Hospitalization Rate',
         domain = 'Respiratory infections',
         date_resolution = 'week',
         update_frequency = 'weekly',
         source = 'CDC RSV-NET (RespNet)',
         url = 'https://data.cdc.gov/resource/kvib-3txy.csv',
         geo_strata = 'state',
         age_strata = 'none',
         race_strata = 'none',
         race_level = NA_character_,
         additional_strata1 = 'none',
         additional_strata_level = NA_character_,
         sex_strata = 'none',
         sex_level = NA_character_)

#######################################
###RSVNet (CDC) by week, state, age, etc
#######################################
url_rsv_net <- "https://data.cdc.gov/resource/29hc-w46k.csv"

cdc_rsvnet <- runIfExpired('rsvnet',
                            ~ read.socrata(url_rsv_net),maxage=hours(24*7)
)

verify_update(  test_file = cdc_rsvnet, ds_path='./Data/rsvnet/') %>%
  write_parquet('./Data/live_files/rsvnet_hosp.parquet')


h1.age <- cdc_rsvnet %>%
  filter(state!="RSV-NET" & sex=='All' & race=='All' & type=='Crude Rate') %>%
  rename( hosp_rate=rate, date=week_ending_date, Level=age_category) %>%
  filter(Level %in% c('1-4 years', '0-<1 year','5-17 years', '18-49 years' ,
                      "≥65 years" ,"50-64 years" )) %>%
  mutate( Level = if_else(Level=='0-<1 year',"<1 Years",
                          if_else( Level=='1-4 years', "1-4 Years",
                          if_else(Level=="5-17 years" ,"5-17 Years",
                          if_else(Level=="18-49 years" ,"18-49 Years",
                          if_else(Level=="50-64 years" ,"50-64 Years",
                           if_else(Level=="≥65 years",'65+ Years', 'other'               
                                  ))))))
  ) %>%
  dplyr::select(state, date, hosp_rate, Level) %>%
  ungroup() %>%
  filter( date >=as.Date('2023-07-01')) %>%
  arrange(state, Level, date) %>%
  group_by(state, Level) %>%
  mutate(hosp_rate_3m=zoo::rollmean(hosp_rate, k = 3, fill = NA, align='right'),
         scale_age=hosp_rate_3m/max(hosp_rate_3m, na.rm=T )*100,
  ) %>%
  as.data.frame()


write.csv(h1.age,'./Data/plot_files/h1.age_rsv_hosp.csv')

#######################################
###Google searches for RSV vaccination
#######################################
g_states <- paste('US',state.abb,sep='-')

url2 <- "https://github.com/DISSC-yale/gtrends_collection/raw/refs/heads/main/data/term=%252Fg%252F11j30ybfx6/part-0.parquet" #rsv vaccination category

temp_file2 <- tempfile(fileext = ".parquet")
download.file(url2, temp_file2, mode = "wb")

google_rsv_vax <- runIfExpired('google_rsv_vax',
                            ~ read_parquet(temp_file2),
                            maxage=hours(24*7)
)

g1_vax_state <- google_rsv_vax %>%
  collect() %>%
  mutate(date=as.Date(date),
         date = as.Date(ceiling_date(date, 'week'))-1,
         stateabb= gsub('US-','', location),
         state=state.name[match(stateabb,state.abb)],
         value=round(value,2)) %>%
  rename(search_volume_vax=value) %>%
  dplyr::select(state, date, search_volume_vax) %>%
  distinct() %>%
  filter(date>='2014-01-01') %>%
  write_parquet('./Data/live_files/google_rsv_vax.parquet')

#######################################
###Google searches for 'RSV' 
#######################################

url1 <- "https://github.com/DISSC-yale/gtrends_collection/raw/refs/heads/main/data/term=rsv/part-0.parquet"
temp_file1 <- tempfile(fileext = ".parquet")
download.file(url1, temp_file1, mode = "wb")

google_rsv <- runIfExpired('google_rsv',
                               ~ read_parquet(temp_file1),
                               maxage=hours(24*7)
)

current_data = verify_update(test_file = google_rsv, ds_path='./Data/google_rsv/') %>%
  filter(location %in% g_states) %>%
  collect() %>%
  mutate(date=as.Date(date),
         date = as.Date(ceiling_date(date, 'week'))-1,
         stateabb= gsub('US-','', location),
         state=state.name[match(stateabb,state.abb)],
         value=round(value,2)) %>%
  rename(search_volume=value) %>%
  dplyr::select(state, date, search_volume) %>%
  distinct() %>%
  filter(date>='2014-01-01') %>%
  full_join(g1_vax_state, by=c('state', 'date') ) %>%
  mutate(month=month(date),
         season = if_else(month>=7 & month <=10,1,0),
         rsv_novax = search_volume - search_volume_vax ,
         rsv_novax = rsv_novax / max(rsv_novax, na.rm=T),
         rsv_novax2 = search_volume - season*(4.41-1.69)*search_volume_vax - (1-season)*3.41*search_volume_vax,  #2.655 based on the regression below
         rsv_novax2 = if_else(rsv_novax2<0,0,rsv_novax2),
         rsv_novax2 = rsv_novax2 / max(rsv_novax2, na.rm=T),
         search_volume = search_volume/ max(search_volume, na.rm=T),
         search_volume_vax = search_volume_vax/ max(search_volume_vax, na.rm=T)
           ) %>%
  write_parquet('./Data/live_files/google_rsv.parquet')


g1_state_harmonized_v1 <- open_dataset('./Data/live_files/google_rsv.parquet') %>%
  rename(Outcome_value1=rsv_novax,
         geography=state) %>%
  mutate(outcome_type='Google Searches',
         outcome_label1 = 'Google Searches 1',
         domain = 'Respiratory infections',
         date_resolution = 'week',
         update_frequency = 'weekly',
         source = 'Google Health Trends',
         url = 'https://dissc-yale.github.io/gtrends_collection/',
         geo_strata = 'state',
         age_strata = 'none',
         race_strata = 'none',
         race_level = NA_character_,
         additional_strata1 = 'none',
         additional_strata_level = NA_character_,
         sex_strata = 'none',
         sex_level = NA_character_) %>%
  collect() 
  


g1_state_harmonized_v2 <- open_dataset('./Data/live_files/google_rsv.parquet') %>%
  rename(Outcome_value1=rsv_novax2,
         geography=state) %>%
  mutate(outcome_type='Google Searches',
         outcome_label1 = 'Google Searches 2',
         domain = 'Respiratory infections',
         date_resolution = 'week',
         update_frequency = 'weekly',
         source = 'Google Health Trends',
         url = 'https://dissc-yale.github.io/gtrends_collection/',
         geo_strata = 'state',
         age_strata = 'none',
         race_strata = 'none',
         race_level = NA_character_,
         additional_strata1 = 'none',
         additional_strata_level = NA_character_,
         sex_strata = 'none',
         sex_level = NA_character_) %>%
  collect()

#od1 <- lme4::lmer(search_volume~ 1 + season*search_volume_vax +  (1|state), data=g1_state[g1_state$date>=as.Date('2023-07-01'),])
# coef.vax <- summary(mod1)$coefficients['search_volume_vax','Estimate']

#######################################
###NREVSS viral testing data for rsv
#######################################

url_nrevss_rsv <- "https://data.cdc.gov/resource/3cxc-4k8q.csv"

cdc_nrevss_rsv <- runIfExpired('nrevss_rsv',
                             ~ read.socrata(url_nrevss_rsv),maxage=hours(24*7)
)

verify_update(  test_file = cdc_nrevss_rsv, ds_path='./Data/nrevss_rsv/') %>%
  filter(posted==max(posted)) %>%
  write_parquet('./Data/live_files/nrevss_rsv.parquet')

key <- readRDS('./Data/hhs_regions.rds')

rsv1_tests <- open_dataset('./Data/live_files/nrevss_rsv.parquet') %>%
  collect() %>%
  as.data.frame()

rsv_ts <- rsv1_tests %>%
  mutate(date = as.Date(mmwrweek_end),
         postdate = as.Date(posted) ) %>%
  filter(postdate==max(postdate)) %>%
  ungroup() %>%
  filter(level != 'National') %>%
  group_by(level ) %>%
  left_join(key, by=c('level'='Group.1')) %>%
  mutate(scaled_cases = pcr_detections/max(pcr_detections)*100,
         hhs_abbr = x  ) %>%
  ungroup()

dates2_rsv_ts <- MMWRweek(as.Date(rsv_ts$date))

max.wk.yr <- max(dates2_rsv_ts$MMWRweek[dates2_rsv_ts$MMWRyear==max(dates2_rsv_ts$MMWRyear)])

rsv_ts <- cbind.data.frame(rsv_ts,dates2_rsv_ts[,c('MMWRyear', 'MMWRweek')]) %>%
  mutate( epiyr = MMWRyear, 
          epiyr = if_else(MMWRweek<=26,MMWRyear - 1 ,MMWRyear),
          epiwk  = if_else( MMWRweek<=26, MMWRweek+52, MMWRweek  ),
          epiwk=epiwk-26
  )

write.csv(rsv_ts,'./Data/plot_files/rsv_ts_nrevss_test_rsv.csv')

#######################################
###Wastewater for RSV
#######################################

url_ww_rsv <- "https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/RSVStateLevelDownloadCSV.csv"

cdc_ww_rsv <- runIfExpired('wastewater_rsv',
                               ~ read_csv(url_ww_rsv),maxage=hours(24*7)
)

verify_update(  test_file = cdc_ww_rsv, ds_path='./Data/wastewater_rsv/') %>%
  write_parquet('./Data/live_files/wastewater_rsv.parquet')

ww1_harmonized <- open_dataset('./Data/live_files/wastewater_rsv.parquet') %>%
  mutate(date=as.Date(Week_Ending_Date)) %>%
  filter(Data_Collection_Period=='All Results') %>%
  rename(state="State/Territory", rsv_ww="State/Territory_WVAL") %>%
  arrange(state, date) %>%
  dplyr::select(state, date, rsv_ww) %>%
  arrange(state, date) %>%
  collect() %>%
  rename(Outcome_value1=rsv_ww,
         geography=state) %>%
  mutate(outcome_type='WasteWater',
         outcome_label1 = 'Waste Water',
         domain = 'Respiratory infections',
         date_resolution = 'week',
         update_frequency = 'weekly',
         source = 'CDC NWWS',
         url = 'https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/RSVStateLevelDownloadCSV.csv',
         geo_strata = 'state',
         age_strata = 'none',
         race_strata = 'none',
         race_level = NA_character_,
         additional_strata1 = 'none',
         additional_strata_level = NA_character_,
         sex_strata = 'none',
         sex_level = NA_character_) 


#######################################
###Epic ED for RSV
#######################################

epic_ed_virus <- open_dataset( './Data/harmonized_epic_flu_rsv_covid.parquet') %>%
  collect()

epic_ed_virus_all_age <- epic_ed_virus %>%
  group_by(outcome_name, outcome_type, geography,date ) %>%
  summarize( N_virus_pos =sum(Outcome_value4, na.rm=T), N_visits=sum(Outcome_value5, na.rm=T) )

e1 <- readr::read_csv('./Data/Epic Cosmos Data/RSVICD10week_age_state.csv', skip=15, col_names=F) %>%
  rename(geography=X1, age=X2) %>%
  tidyr::fill( geography, .direction = 'down') %>%
  reshape2::melt(., id.vars=c('geography','age')) %>%
  arrange(geography, age, variable) %>%
  group_by(geography, age) %>%
  #week END date
  mutate(date= seq.Date(from=as.Date('2019-01-12'), length.out=n() , by='week')) %>%
  ungroup() %>%
  rename(N_cases=value) %>%
  mutate( N_cases = if_else(N_cases=='10 or fewer',NA_character_, N_cases),
          N_cases = as.numeric(N_cases),
          geography= if_else(geography=='Total', 'United States', geography)) %>%
  dplyr::select(-variable)

e1_all_ages <- e1 %>% filter(age=='Total') %>%
  rename(state=geography, N_epic=N_cases) 

e1_age <-  e1 %>% filter(age!='Total' & geography !='United States' & age != 'No value') %>%
  mutate( Level = if_else(age == 'Less than 1 years', '<1 Years',
                          if_else( age=='? 1 and < 5 years','1-4 Years',
                                   if_else(age=='? 5 and < 18 years' , "5-17 Years",
                                           if_else( age=="? 18 and < 50 years" ,"18-49 Years" ,         
                                                    if_else( age=="? 50 and < 65 years" , "50-64 Years" ,        
                                                             if_else( age=="? 65 and < 75 years" ,"65-74 Years",          
                                                                      if_else( age=="75 years or more" , "75+ Years", NA_character_         
                                                                      ) )))))
  )) %>%
  dplyr::select(-age) %>%
  ungroup() %>%
  arrange( geography,Level, date) %>%
  group_by( geography,Level) %>%
  rename(N_cases_epic=N_cases) %>%
  filter(date>='2023-07-01') %>%
  mutate( N_cases_epic_3m=zoo::rollmean(N_cases_epic, k = 3, fill = NA, align='right'),
          max_grp= max(N_cases_epic_3m, na.rm=T),
          scale_age_epic = N_cases_epic_3m/max_grp*100
  ) %>%
  ungroup()

write.csv(e1_age, './Data/plot_files/e1_age_epic_age_rsv.csv')

epic_rsv_aggregate_harmonized <- e1_all_ages %>%
  rename(Outcome_value1=N_epic,
         geography=state) %>%
  mutate(outcome_type='ED',
         outcome_label1 = 'ED (N)',
         domain = 'Respiratory infections',
         date_resolution = 'week',
         update_frequency = 'weekly',
         source = 'Epic Cosmos',
         url = 'https://www.epicresearch.org/',
         geo_strata = 'state',
         age_strata = 'none',
         race_strata = 'none',
         race_level = NA_character_,
         additional_strata1 = 'none',
         additional_strata_level = NA_character_,
         sex_strata = 'none',
         sex_level = NA_character_)


#EPIC ED all cause
epic_ed_all <- epic_age_import(ds_name="all_ED_week_age_state_sunday.csv" ) %>%
  rename(N_ED_epic= N_cases_epic
  ) %>%
  dplyr::select(geography,Level, date,N_ED_epic)

epic_ed_rsv <- epic_age_import(ds_name="RSV_ED_week_age_state_sunday.csv" ) %>%
  rename( N_RSV_ED_epic= N_cases_epic) %>%
  dplyr::select(geography,Level, date,N_RSV_ED_epic)

epic_ed_flu <- epic_age_import(ds_name="FLU_ED_week_age_state_sunday.csv" ) %>%
  rename( N_flu_ED_epic= N_cases_epic) %>%
  dplyr::select(geography,Level, date,N_flu_ED_epic)

epic_ed_covid <- epic_age_import(ds_name="COVID_ED_week_age_state_sunday.csv" ) %>%
  rename( N_covid_ED_epic= N_cases_epic) %>%
  dplyr::select(geography,Level, date,N_covid_ED_epic)

epic_ed_combo <- epic_ed_all %>%
  left_join(epic_ed_rsv, by=c('geography','Level', 'date')) %>%
  left_join(epic_ed_flu, by=c('geography','Level', 'date')) %>%
  left_join(epic_ed_covid, by=c('geography','Level', 'date')) %>%
  arrange(Level, geography, date) %>%
  group_by(Level, geography) %>%
  mutate(pct_RSV_ED_epic =N_RSV_ED_epic/N_ED_epic*100,
         pct_flu_ED_epic =N_flu_ED_epic/N_ED_epic*100,
         pct_covid_ED_epic =N_covid_ED_epic/N_ED_epic*100,
         
         pct_RSV_ED_epic = zoo::rollmean(pct_RSV_ED_epic, k = 3, fill = NA, align='right'),
         pct_flu_ED_epic = zoo::rollmean(pct_flu_ED_epic, k = 3, fill = NA, align='right'),
         pct_covid_ED_epic = zoo::rollmean(pct_covid_ED_epic, k = 3, fill = NA, align='right'),
         
         ED_epic_scale_RSV= 100*pct_RSV_ED_epic/max(pct_RSV_ED_epic,na.rm=T),
         ED_epic_scale_flu= 100*pct_flu_ED_epic/max(pct_flu_ED_epic,na.rm=T),
         ED_epic_scale_covid= 100*pct_covid_ED_epic/max(pct_covid_ED_epic,na.rm=T),
  )


write.csv(epic_ed_combo, './Data/plot_files/epic_ed_combo_rsv_flu_covid.csv')


#######################
###Combined file for overlaid time series RSV figure

dwh <- bind_rows(nssp_harmonized, ww1_harmonized,h1_harmonized,epic_rsv_aggregate_harmonized,g1_state_harmonized_v1, g1_state_harmonized_v2) %>%
  filter(date >=as.Date('2023-07-01')) %>%
  arrange(geography, outcome_label1,source,date) %>%
  group_by(geography,outcome_label1, source) %>%
  filter(date>='2023-07-01') %>%
  mutate(outcome_3m = zoo::rollmean(Outcome_value1, k = 3, fill = NA, align='right'),
         outcome_3m_scale = outcome_3m / max(outcome_3m, na.rm=T)*100
  )

dates2 <- MMWRweek(as.Date(dwh$date))

max.wk.yr <- max(dates2$MMWRweek[dates2$MMWRyear==max(dates2$MMWRyear)])

dwh <- cbind.data.frame(dwh,dates2[,c('MMWRyear', 'MMWRweek')]) %>%
  mutate( epiyr = MMWRyear, 
          epiyr = if_else(MMWRweek<=26,MMWRyear - 1 ,MMWRyear),
          epiwk  = if_else( MMWRweek<=26, MMWRweek+52, MMWRweek  ),
          epiwk=epiwk-26
  )

write.csv(dwh,'./Data/plot_files/dwh_combined_plot1_long.csv')


##############################################################

#################################################
### State map NSSP
################################################
dates <- seq.Date(from=as.Date('2022-10-01'), to=Sys.Date(),by='week')

i=length(dates)-1

d1_state <- open_dataset('./Data/live_files/nssp_rsv_county.parquet') %>%
  filter(county=='All'  ) %>%
  rename(percent_visits_rsv_state =percent_visits_rsv ) %>%
  # percent_visits_rsv_state=if_else(percent_visits_rsv>1,1,percent_visits_rsv) ) %>%
  rename(state=geography) %>%
  dplyr::select(state,week_end, percent_visits_rsv_state) %>%
  collect()

d1_all <- open_dataset('./Data/live_files/nssp_rsv_county.parquet') %>%
  filter(county!='All' ) %>%
  collect() %>%
  rename(state=geography) %>%
  dplyr::select(state, county, fips, week_end, percent_visits_rsv) %>%
  left_join(d1_state, by=c('week_end', 'state')) %>%
  mutate(percent_visits_rsv = if_else(is.na(percent_visits_rsv),percent_visits_rsv_state,percent_visits_rsv ),
         #fix CT county coding
         fips = if_else(state=='Connecticut' & county=='Fairfield',9001 ,
                        if_else(state=='Connecticut' &  county=='Hartford', 9003,
                                if_else(state=='Connecticut'& county=='Litchfield', 9005 ,
                                        if_else(state=='Connecticut' & county=='Middlesex',9007 ,
                                                if_else(state=='Connecticut' & county=='New Haven', 9009 ,
                                                        if_else(state=='Connecticut' & county=='New London',9011 ,
                                                                if_else(state=='Connecticut' & county=='Tolland',9013 ,
                                                                        if_else(state=='Connecticut' & county=='Windham', 9015, fips)))))))
         ) ) %>%
  as.data.frame()

write.csv(d1_all,'./Data/plot_files/rsv_county_filled_map_nssp.csv')

#fluA
#ww_flu1 <- read.csv('https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/FluA/FluAStateMapDownloadCSV.csv')%>%
#write.csv(ww_flu1,'./Data/FluStateLevelDownloadCSV_ww.csv')



##Metro; Crosswalk the DMA to counties FIPS codes
#https://www.kaggle.com/datasets/kapastor/google-trends-countydma-mapping?resource=download
# cw1 <- read.csv('./Data/GoogleTrends_CountyDMA_Mapping.csv') %>%
#   mutate(GOOGLE_DMA=toupper(GOOGLE_DMA))
# 
# #Metro region
# #https://stackoverflow.com/questions/61213647/what-do-gtrendsr-statistical-areas-correlate-with
# #Nielsen DMA map: http://bl.ocks.org/simzou/6459889
# data("countries")
# 
# metros <- countries[countries$country_code == 'US', ]
# 
# metros <-
#   metros[grep("[[:digit:]]", substring(metros$sub_code, first = 4)), ]
# 
# metros$numeric.sub.area <- gsub('US-', '', metros$sub_code)
# 
# 
# dma_link1 <- cbind.data.frame('DMA_name'=metros$name,'DMA'=metros$numeric.sub.area) %>%
#   rename(DMA_ID=DMA) %>%
#   full_join(cw1, by=c("DMA_name"="GOOGLE_DMA"))
# 


##Google metro data
# g1_metro <- read_parquet(temp_file) %>%
#   filter(!(location %in% g_states) ) %>%
#   collect() %>%
#   mutate(date2=as.Date(date, '%b %d %Y'),
#          date = as.Date(ceiling_date(date2, 'week'))-1) %>%
#   filter(date>='2021-03-01') %>%
#   rename(search_volume=value) %>%
#   left_join(dma_link1, by=c('location'='DMA_ID')) %>% #many to many join by date and counties
#    group_by(STATEFP,CNTYFP) %>%
#    mutate(fips=paste0(STATEFP,sprintf("%03d", CNTYFP)),
#           fips=as.numeric(fips),
#           
#           search_volume_scale = search_volume/max(search_volume,na.rm=T)*100) %>%
#    ungroup() 