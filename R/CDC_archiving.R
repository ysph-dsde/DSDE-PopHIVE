
#install.packages("RSocrata")
library(RSocrata)
source('./R/runIfExpired.R') #function for archiving


url_nssp <- "https://data.cdc.gov/resource/rdmq-nq56.csv"

#Creates a time/date stamped parquet file in the folder nssp_ed1. 
#if a file has been downloaded within past week (24*7 hours), 
#it just reads in latest file, otherwise it downloads fresh copy

cdc_nssp_ed1 <- runIfExpired('nssp_ed1',
                    ~ read.socrata(url_nssp),maxage=hours(24*7)
)

#Check that formatting is consistent between vintages. (1) check column names (2) check variable formats
#compare newest data to previous dataset

current_data = verify_update(  test_file = cdc_nssp_ed1, ds_path='./Data/nssp_ed1') %>%
  write_parquet('./Data/live_files/rsvnet_hosp.parquet')



