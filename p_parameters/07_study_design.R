#-----------------------------------
# Parameters specific for each study
#-----------------------------------

#study_years_datasource

# firstjan2021 <- ymd(20210101)
# 
date_format<-"%Y%m%d"
 study_start <- as.Date(as.character(20060101), date_format)
 
 if (thisdatasource=="SNDS") study_start <- as.Date(as.character(20180101), date_format)
 if (thisdatasource=="UOSL") study_start <- as.Date(as.character(20090101), date_format)
     
# start_lookback <- as.Date(as.character(20190101), date_format)
# 
 
 study_end <- as.Date(as.character(20191231), date_format)
 
 if (thisdatasource=="Finland??") study_end <- as.Date(as.character(20190331), date_format)
 if (thisdatasource=="ARS") study_end <- as.Date(as.character(20201231), date_format)

 # study_end <- min(instance_creation, recommended_end_date, na.rm = T)
 # rm(recommended_end_date)
# 
# study_years <- c("2020","2021")
# 
# 
# firstYearComponentAnalysis = "2019"
# secondYearComponentAnalysis = "2020"


#----------------------------
# admissible gap between observation periods (DAP-specific)
admissible_gap_obs_periods <- vector(mode="list")
admissible_gap_obs_periods[['ARS']] <- 365
admissible_gap_obs_periods[['TEST']] <- 365
admissible_gap_obs_periods[['BIPS']] <- 30

gap_days <- ifelse(is.na(admissible_gap_obs_periods[[thisdatasource]]),1, admissible_gap_obs_periods[[thisdatasource]])



# define number of days a spells should not be shorter
min_spell_lenght<-365
