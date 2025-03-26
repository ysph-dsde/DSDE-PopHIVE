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
  dplyr::select(state, date, 
                #percent_visits_rsv ,
                #rsv_ww,
                 rsv_novax,
                rsv_novax2 )%>%
  reshape2::melt(.,id.vars=c('state','date')) %>%
  group_by(state,variable, date ) %>%
  summarize(value=mean(value)) %>% #duplicates in google data
  ungroup() %>%
  arrange(state, variable, date) %>%
  group_by(state, variable) %>%
  
  mutate( #lag0 =value,
          #lag1 = lag(value,1),
          #lag2 = lag(value,2),
          #lag3 = lag(value,3),
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

#age stratified Epic Cosmos data
a2 <- read.csv('./Data/plot_files/e1_age_epic_age_rsv.csv') %>%
  dplyr::select(geography, date, Level,N_cases_epic) %>%
  rename(agec=Level, state=geography) %>%
  mutate(date=as.Date(date)) %>%
  arrange(state, agec, date) %>%
  group_by(state,agec) %>%
  rename(value=N_cases_epic) %>%
  mutate( 
    lag0 = lag(value,0),
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
    agec = gsub('<','u',agec),
    agec = gsub('-','_',agec),
    
      ) %>%
  dplyr::select(-value) %>%
  reshape2::melt(id.vars=c('state','date', 'agec')) %>%
  group_by(state,agec,variable ) %>%
  mutate(scale_value=value/max(value, na.rm=T),
           agec=paste(agec, state, sep="_")) %>%
  dplyr::select(-value) %>%
  filter(!is.na(scale_value)) %>%
  reshape2::dcast(date~variable+agec, value.var='scale_value') 


##NSSP
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
  mutate( #lag0 =scale_value,
          #lag1 = lag(scale_value,1),
          #lag2 = lag(scale_value,2),
          #lag3 = lag(scale_value,3),
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
  dplyr::select(date, `lag0_u1 Years_New York`) %>%
  rename(Y=`lag0_u1 Years_New York`) %>%
  mutate(variable='EpicAgeRSV' ) 

all.vars<- a2 %>%  #exclude a1
  full_join(a3, by='date') %>%
  full_join(g1_metro, by='date') %>%
  right_join(all.y, by='date')

X <- all.vars %>%
  dplyr::select(-starts_with('lag0'), -date,-starts_with('N_cases_epic'),-Y ) %>%
  dplyr::select( starts_with('lag')) %>%
  as.matrix()

X.scale <- apply(X,2, function(xx) (xx- mean(xx,na.rm=T))/sd(xx,na.rm=T) )

Y= all.vars %>%
  dplyr::select(Y)%>%
  pull()

length.y <- length(Y)
Y.fit <- Y
Y.fit[(length.y-2):length.y] <- NA
##MICE imputation on X

# Y-aware PCA on X

library(softImpute)
library(pls)

X_imputed <- softImpute::complete(X.scale, softImpute(X.scale))

model <- plsr(Y.fit ~ X_imputed, ncomp = 20, validation = "CV")

# Check cross-validation results
summary(model)

# Plot RMSE to find the optimal number of components
plot(RMSEP(model), legendpos = "topright")

# Get the best number of components
best_ncomp <- which.min(RMSEP(model)$val['CV', 'Y.fit', ])

cat("Optimal number of components:", best_ncomp)

# Refit the final model with the optimal number of components
final_model <- plsr(Y.fit ~ X_imputed, ncomp = best_ncomp)

preds.pls <- all.vars %>%
  dplyr::select(date) %>%
  mutate(pred1=predict(final_model, ncomp = best_ncomp, newdata=as.data.frame(X_imputed)), Y=Y, Y.fit=Y.fit)

preds.pls %>%
  ggplot()+
  geom_line(aes(x=date, y=pred1), col='red')+
  geom_line(aes(x=date, y=Y))+
  theme_minimal()

loadings <- as.data.frame(final_model$loadings[, 1:best_ncomp])

get_vip <- function(model) {
  W <- t(model$loading.weights)
  SSY <- sum(model$Yloadings^2)
  SSW <- colSums((W^2) * SSY)
  VIP <- sqrt(ncol(W) * SSW / sum(SSW))
  return(VIP)
}

vip_scores <- get_vip(final_model)

# View VIP scores
vip_df <- cbind.data.frame(Variable = colnames(X), VIP = vip_scores)
vip_df <- vip_df[order(-vip_df$VIP), ]
print(vip_df)

####################################################
####################################################
# 
# ## SUPERVISED PCA
# 
# Y.scale <- ( Y - mean(Y, na.rm=T) )/ sd(Y,na.rm=T)
# 
# y.aware.scale<- apply(X_imputed, 2, function(x1){
#   x1 <- as.vector(x1)
#     reg<-lm(Y.scale~x1)
#   slope<- reg$coefficients[2]
#   x.scale<- x1*slope - mean(x1*slope)
#   return(x.scale)
# })
# 
# y.aware.scale_nomiss <- y.aware.scale[,colSums(is.na(y.aware.scale)) == 0]
# 
# pca1<- prcomp(y.aware.scale_nomiss, center = FALSE,scale. = FALSE)
# 
# # plot(pca1$sdev)
# n.pcs.keep<-10
# pcs<-pca1$x
# pcs<- as.data.frame(apply(pcs,2, scale)) #SCALE THE PCS prior to regression!
# 
# names(pcs) <- paste0('PC', 1:ncol(pcs))
# 
# pc.df <- cbind.data.frame('date'=all.vars$date, 'Y'=Y,  pcs[,1:n.pcs.keep])
# 
# mod1 <- lm(Y ~ PC1 +PC2, data=pc.df  )
# summary(mod1)
# pc.df$pred1 <- predict(mod1, newdata = pc.df)
# 
# 
# loadings <- pca1$rotation[,1:10]
# 
# pc.df %>%
#   ggplot()+
#   geom_line(aes(x=date, y=pred1))+
#   geom_line(aes(x=date, y=Y))+
#   theme_minimal()


comp.Y <- a2 %>%
  rename(NY=`lag0_u1 Years_New York`, 
         GA=`lag0_u1 Years_Georgia`,
         SC=`lag0_u1 Years_South Carolina`
         ) %>%
  mutate(variable='EpicAgeRSV' ) 


ggplot(comp.Y) +
  geom_line(aes(x=date, y=NY))+
geom_line(aes(x=date, y=SC))


##IDEA: use the correlations in lagged search data from current year (available real time)
#to inform location weighting...then apply these weighting to the ED data


#yearsize
test1  <- read.csv('./Data/plot_files/e1_age_epic_age_rsv.csv') %>%
  dplyr::select(geography, date, Level,N_cases_epic) %>%
  rename(agec=Level, state=geography) %>%
  mutate(date=as.Date(date),
         year=year(date),
         week=week(date),
         epiyr=if_else(week<=26,year-1, year)) %>%
  arrange(state, agec, date) %>%
  filter(agec=='<1 Years' & epiyr>=2023) %>%
  group_by(state, epiyr) %>%
  summarize(rsv_int = max(N_cases_epic, na.rm=T)) %>%
  reshape2::dcast(state~epiyr, value.var='rsv_int') %>%
  rename(yr2024=`2024`, yr2023=`2023`) %>%
  mutate(ratio=yr2024/yr2023)

ggplot(test1) +
  geom_point(aes(x=yr2023, ratio))

#prop <1
test1  <- read.csv('./Data/plot_files/e1_age_epic_age_rsv.csv') %>%
  dplyr::select(geography, date, Level,N_cases_epic) %>%
  rename(agec=Level, state=geography) %>%
  mutate(date=as.Date(date),
         year=year(date),
         week=week(date),
         epiyr=if_else(week<=26,year-1, year)) %>%
  arrange(state, agec, date) %>%
  filter(agec %in% c('<1 Years','1-4 Years') & epiyr>=2023) %>%
  group_by(state, epiyr) %>%
  summarize(rsv_int = max(N_cases_epic, na.rm=T)) %>%
  reshape2::dcast(state~epiyr, value.var='rsv_int') %>%
  rename(yr2024=`2024`, yr2023=`2023`) %>%
  mutate(ratio=yr2024/yr2023)
