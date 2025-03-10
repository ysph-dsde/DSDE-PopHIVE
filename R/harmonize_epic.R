

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
  filter(!is.na(Level)) %>%
  arrange(Level, geography, date) %>%
  group_by(Level, geography) %>%
  mutate(pct_RSV_ED_epic =N_RSV_ED_epic/N_ED_epic*100,
         pct_flu_ED_epic =N_flu_ED_epic/N_ED_epic*100,
         pct_covid_ED_epic =N_covid_ED_epic/N_ED_epic*100,
         
         pct_RSV_ED_epic_smooth = zoo::rollmean(pct_RSV_ED_epic, k = 3, fill = NA, align='right'),
         pct_flu_ED_epic_smooth  = zoo::rollmean(pct_flu_ED_epic, k = 3, fill = NA, align='right'),
         pct_covid_ED_epic_smooth  = zoo::rollmean(pct_covid_ED_epic, k = 3, fill = NA, align='right'),
         
         ED_epic_scale_RSV= 100*pct_RSV_ED_epic_smooth /max(pct_RSV_ED_epic_smooth ,na.rm=T),
         ED_epic_scale_flu= 100*pct_flu_ED_epic_smooth /max(pct_flu_ED_epic_smooth ,na.rm=T),
         ED_epic_scale_covid= 100*pct_covid_ED_epic_smooth/max(pct_covid_ED_epic_smooth ,na.rm=T),
  )  %>%
  ungroup() %>%
  reshape2::melt(., id.vars= c('geography','Level', 'date')) %>%
  mutate(outcome_type = if_else(grepl('RSV',toupper(variable)), 'RSV',
                                if_else(grepl('FLU',toupper(variable)), 'flu', 
                                 if_else(grepl('COVID',toupper(variable)), 'COVID',
                                   if_else(grepl('ED_EPIC',toupper(variable)), 'ED', 'other',
                                    
                                )))),
         outcome_variable_type = if_else(grepl('N_',toupper(variable)), 'Number',
                                if_else(grepl('SMOOTH',toupper(variable)), 'percent_smoothed',  
                                if_else(grepl('PCT_',toupper(variable)), 'percent',  
                                if_else(grepl('SCALE',toupper(variable)), 'scaled',  'other'
                                      
                            )))))


epic_ed_combo2 <- reshape2::dcast( epic_ed_combo, geography+Level+date + outcome_type ~ outcome_variable_type ) %>%
  rename(Outcome_value1 =percent_smoothed,
       Outcome_value2 =percent,
       Outcome_value3 =Number,
       Outcome_value4 =scaled,
       age_level=Level
       ) %>%
  dplyr::select(geography, age_level, date, outcome_type, Outcome_value1,Outcome_value2,Outcome_value3,Outcome_value4 ) %>%
  mutate(outcome_name='RSV',
         outcome_type='ED',
         domain='Respiratory infections',
         date_resolution ='week',
         update_frequency='weekly',
         source='Epic Cosmos',
         url='https://www.epicresearch.org/',
         geo_strata ='state',
         age_strata='age_scheme_1',
         race_strata='none',
         race_level=NA_character_,
         additional_strata1 = 'none',
         additional_strata_level =NA_character_,
         sex_strata='none',
         sex_level= NA_character_
         
         )
write_parquet(epic_ed_combo2, './Data/harmonized_epic_flu_rsv_covid.parquet')

#test <- read_parquet( './Data/harmonized_epic_flu_rsv_covid.parquet') %>% collect()
