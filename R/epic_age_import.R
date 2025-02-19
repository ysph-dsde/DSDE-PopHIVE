epic_age_import <- function(ds_name, skipN=15) {

  ds_out <- readr::read_csv(paste0("./Data/CONFIDENTIAL/",ds_name), skip=skipN, col_names=F) %>%
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
  dplyr::select(-variable)%>%
    mutate( Level = if_else(age %in% c('Less than 1 years', 'Less than 1 year'), '<1 Years',
                            if_else( age %in%  c('? 1 and < 5 years','1 year or more and less than 5 years'),'1-4 Years',
                                     if_else(age %in% c('? 5 and < 18 years','5 years or more and less than 18 years (1)') , "5-17 Years",
                                             if_else( age %in% c("? 18 and < 65 years",'18 years or more and less than 65 years') ,"18-64 Years" ,         
                                                      if_else( age %in% c("65 years or more","? 65 and < 110 years") , "65+ Years" , NA_character_        
                                                                 
                                                                       ) ))))) %>%
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
    ungroup() %>%
    filter(!is.na(geography))

return(ds_out)
}
