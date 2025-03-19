library(tidyverse)
library(plotly)
library(shiny)
library(tidyverse)
library(scales)
library(janitor)
library(gtrendsR)
library(MMWRweek)
library(arrow)
#library(rnaturalearth)
library(parquetize)
library(viridisLite)
library(tigris)
library(usmap)
library(cowplot)
library(leaflet)
library(viridis)
library(sf)
library(ggrepel)
library(readxl)
library(ggalluvial)
library(waffle)

a1 <- read.csv('./Data/plot_files/dwh_combined_plot1.csv') %>%
  dplyr::select(state, date, percent_visits_rsv ,rsv_ww,
                 rsv_novax,rsv_novax2 )%>%
  reshape2::melt(.,id.vars=c('state','date')) %>%
  group_by(state,variable, date ) %>%
  summarize(value=mean(value)) %>% #duplicates in google data
  ungroup() %>%
  arrange(state, variable, date) %>%
  group_by(state, variable) %>%
  
  mutate( lag0 =value,
          lag1 = lag(value,1),
          lag2 = lag(value,2),
          lag3 = lag(value,3),
          lag4 = lag(value,4),
          lag5 = lag(value,5),
          lag6 = lag(value,6),
          lag7 = lag(value,7),
          lag8 = lag(value,8),
          lag9 = lag(value,9),
          lag10 = lag(value,10),
          lag11 = lag(value,11),
          lag12 = lag(value,12),
          lag13 = lag(value,13),
          lag14 = lag(value,14),
          lag15 = lag(value,15),
          lag16 = lag(value,16),
  ) %>%
  rename(variable1=variable) %>%
  ungroup() %>%
  dplyr::select(-value) %>%
  reshape2::melt(id.vars=c('state','date', 'variable1')) %>%
  mutate(scale_value=value/max(value, na.rm=T),
         variable=paste(variable, variable1, sep="_")) %>%
  dplyr::select(-value) %>%
  filter(!is.na(scale_value)) %>%
  reshape2::dcast(date~variable+state, value.var='scale_value') %>%
  mutate(date=as.Date(date))


a2 <- read.csv('./Data/plot_files/e1_age_epic_age_rsv.csv') %>%
  dplyr::select(geography, date, Level,N_cases_epic) %>%
  rename(agec=Level, state=geography)


a3 <- open_dataset('./Data/NSSP_detailed.parquet') %>%
  filter(county!='All' ) %>%
  collect() %>%
  rename(state=geography) %>%
  dplyr::select(state, county, fips, week_end, percent_visits_rsv) %>%
  mutate(
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
  as.data.frame() %>%
  mutate(geo = paste(state, county, sep='_')) %>%
  rename(date=week_end) %>%
  group_by(geo) %>%
  mutate(scale_value=percent_visits_rsv/max(percent_visits_rsv, na.rm=T),
         variable1='nssp') %>%
  dplyr::select(geo,date,variable1, scale_value) %>%
  filter(!is.na(scale_value)) %>%
  ungroup() %>%
  arrange(geo, variable1, date) %>%
  group_by(geo, variable1) %>%
  mutate( lag0 =scale_value,
          lag1 = lag(scale_value,1),
          lag2 = lag(scale_value,2),
          lag3 = lag(scale_value,3),
          lag4 = lag(scale_value,4),
          lag5 = lag(scale_value,5),
          lag6 = lag(scale_value,6),
          lag7 = lag(scale_value,7),
          lag8 = lag(scale_value,8),
          lag9 = lag(scale_value,9),
          lag10 = lag(scale_value,10),
          lag11 = lag(scale_value,11),
          lag12 = lag(scale_value,12),
          lag13 = lag(scale_value,13),
          lag14 = lag(scale_value,14),
          lag15 = lag(scale_value,15),
          lag16 = lag(scale_value,16),
  ) %>%
  dplyr::select(-scale_value) %>%
  reshape2::melt(id.vars=c('geo','date', 'variable1')) %>%
  ungroup() %>%
  mutate( variable=paste(variable, variable1, sep="_")) %>%
  reshape2::dcast(date~variable+geo, value.var='value') 

#search data 
url2 <- "https://github.com/DISSC-yale/gtrends_collection/raw/refs/heads/main/data/term=%252Fg%252F11j30ybfx6/part-0.parquet" #rsv vaccination category

temp_file2 <- tempfile(fileext = ".parquet")
download.file(url2, temp_file2, mode = "wb")
g_states <- paste('US',state.abb,sep='-')

g1_vax_metro <- read_parquet(temp_file2) %>%
    filter(!(location %in% g_states) ) %>%
    collect() %>%
    mutate( date = ceiling_date(as.Date(date), 'week')-1) %>%
    filter(date>='2021-03-01') %>%
    rename(search_volume_vax=value) %>%
  dplyr::select(date, location,search_volume_vax) %>%
  group_by(date, location) %>%
  summarize(search_volume_vax=mean(search_volume_vax)) %>% #average repeated pulls
 ungroup()

url1 <- "https://github.com/DISSC-yale/gtrends_collection/raw/refs/heads/main/data/term=rsv/part-0.parquet"


# Download the file temporarily
temp_file1 <- tempfile(fileext = ".parquet")
download.file(url1, temp_file1, mode = "wb")



g1_metro <- read_parquet(temp_file1) %>%
  filter(!(location %in% g_states)) %>%
  collect() %>%
  mutate(date=as.Date(date),
         date = as.Date(ceiling_date(date, 'week'))-1,
         value=round(value,2)) %>%
  rename(search_volume_rsv=value) %>%
  dplyr::select(location, date, search_volume_rsv) %>%
  group_by(date, location) %>%
  summarize(search_volume_rsv=mean(search_volume_rsv)) %>% #average repeated pulls
  ungroup() %>%
  filter(date>='2014-01-01') %>%
  full_join(g1_vax_metro, by=c('location', 'date') ) %>%
  mutate(month=month(date),
         season = if_else(month>=7 & month <=10,1,0),
         rsv_novax = search_volume_rsv - search_volume_vax ,
         rsv_novax2 = search_volume_rsv - season*(4.41-1.69)*search_volume_vax - (1-season)*3.41*search_volume_vax,  #2.655 based on the regression below
         rsv_novax2 = if_else(rsv_novax2<0,0,rsv_novax2),
         variable1='rsv_search_novax'
  ) %>%
  dplyr::select(date, location,variable1, rsv_novax) %>%
  group_by(location) %>%
  mutate(scale_value=rsv_novax/max(rsv_novax, na.rm=T)) %>%
  filter(!is.na(scale_value))  %>%
  arrange(location, variable1, date) %>%
  group_by(location, variable1) %>%
  mutate( lag0 =scale_value,
          lag1 = lag(scale_value,1),
          lag2 = lag(scale_value,2),
          lag3 = lag(scale_value,3),
          lag4 = lag(scale_value,4),
          lag5 = lag(scale_value,5),
          lag6 = lag(scale_value,6),
          lag7 = lag(scale_value,7),
          lag8 = lag(scale_value,8),
          lag9 = lag(scale_value,9),
          lag10 = lag(scale_value,10),
          lag11 = lag(scale_value,11),
          lag12 = lag(scale_value,12),
          lag13 = lag(scale_value,13),
          lag14 = lag(scale_value,14),
          lag15 = lag(scale_value,15),
          lag16 = lag(scale_value,16),
  ) %>%
  dplyr::select(-scale_value) %>%
  reshape2::melt(id.vars=c('location','date', 'variable1')) %>%
  ungroup() %>%
  mutate( variable=paste(variable, variable1, sep="_")) %>%
  reshape2::dcast(date~variable+location, value.var='value') 


##Combine the variables into one large dataframe

all.y <- a2 %>%
  mutate(variable='EpicAgeRSV') %>%
  reshape2::dcast(date ~ variable+state+ agec, value.var='N_cases_epic')

X <- a1 %>%
  full_join(a3, by='date') %>%
  full_join(g1_metro, by='date')


##MICE imputation on X

# Y-aware PCA on X