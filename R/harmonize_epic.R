

#EPIC ED all cause
epic_ed_all <- epic_age_import(ds_name="all_ED_week_age_state_sunday.csv" ) %>%
  rename(N_ED_epic_all_cause= N_cases_epic
  ) %>%
  dplyr::select(geography,Level, date,N_ED_epic_all_cause) 


#EPIC ED RSV

epic_ed_rsv <- epic_age_import(ds_name="RSV_ED_week_age_state_sunday.csv" ) %>%
  rename( N_ED_type= N_cases_epic) %>%
  dplyr::select(geography,Level, date,N_ED_type) %>%
    full_join(epic_ed_all, by=c('geography','Level', 'date')) %>%
  arrange(Level, geography, date) %>%
 group_by(Level, geography)  %>%
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


# EPIC ED FLU
epic_ed_flu <- epic_age_import(ds_name="FLU_ED_week_age_state_sunday.csv" ) %>%
  rename( N_ED_type= N_cases_epic) %>%
  dplyr::select(geography,Level, date,N_ED_type) %>%
  full_join(epic_ed_all, by=c('geography','Level', 'date')) %>%
  arrange(Level, geography, date) %>%
  group_by(Level, geography)  %>%
  mutate(outcome_name='FLU',
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

# EPIC ED COVID
epic_ed_covid <- epic_age_import(ds_name="COVID_ED_week_age_state_sunday.csv" ) %>%
  rename( N_ED_type = N_cases_epic) %>%
  dplyr::select(geography,Level, date,N_ED_type) %>%
  full_join(epic_ed_all, by=c('geography','Level', 'date')) %>%
  arrange(Level, geography, date) %>%
  group_by(Level, geography)  %>%
  mutate(outcome_name='COVID',
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


epic_ed_combo <- epic_ed_all %>%
  bind_rows(epic_ed_rsv,epic_ed_flu ,epic_ed_covid) %>%
  filter(!is.na(Level)) %>%
  arrange(outcome_name,outcome_type,source, Level, geography, date) %>%
  group_by(outcome_name,outcome_type,source, Level, geography) %>%
  mutate(
    N_ED_type = if_else(!is.na(N_ED_epic_all_cause) & is.na(N_ED_type), 4.9999, N_ED_type ), #is suppressed <10 counts, set to 4.9999

         pct_ED_epic =N_ED_type/N_ED_epic_all_cause*100,
       
         pct_ED_epic_smooth = zoo::rollmean(pct_ED_epic, k = 3, fill = NA, align='right'),
         
         ED_epic_scale= 100*pct_ED_epic_smooth /max(pct_ED_epic_smooth ,na.rm=T),
         
  )  %>%
  ungroup() %>%
  rename(Outcome_value1= pct_ED_epic_smooth,
         Outcome_value3 =pct_ED_epic,
         Outcome_value4 =N_ED_type,
         Outcome_value2 =ED_epic_scale,
         Outcome_value5 =N_ED_epic_all_cause,
         age_level=Level
         ) %>%
  dplyr::select("geography", "age_level", "date",  "outcome_name", "outcome_type", "source",
                "url",  "geo_strata", "age_strata", "race_strata",  "race_level", "additional_strata1", "additional_strata_level", "sex_strata", "sex_level",
                      "Outcome_value1",   "Outcome_value2",      "Outcome_value3" ,       "Outcome_value4","Outcome_value5"     ) %>%
          mutate(outcome_label1 = 'Pct of ED visits (smoothed)',
                 outcome_label2 = 'Pct of ED visits (smoothed and scaled)',
                 outcome_label3 = 'Pct of ED visits',
                 outcome_label4 = 'Number of ED visits for the outcome',
                 outcome_label5 = 'Number of ED visits for any outcome'
                 
)

  



         
write_parquet(epic_ed_combo, './Data/harmonized_epic_flu_rsv_covid.parquet')

#test <- read_parquet( './Data/harmonized_epic_flu_rsv_covid.parquet') %>% collect()
