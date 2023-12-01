#-----------------------------------
# Parameters specific for each study
#-----------------------------------

#study_years_datasource


study_start <- ymd(20060101)

if (thisdatasource=="UOSL") study_start <- ymd(20090101)
if (thisdatasource=="RDRU_FISABIO") study_start <- ymd(20140101)


study_end <-  ymd(20191231)

# study_end <-   min(instance_creation, recommended_end_date, ymd(20191231), na.rm = T)
# rm(recommended_end_date)
 
# 
if (thisdatasource=="THL") study_end <- ymd(20190331)
if (thisdatasource=="ARS") study_end <- ymd(20201231)
if (thisdatasource=="RDRU_FISABIO") study_end <- ymd(20201231)
if (thisdatasource=="SAIL Databank") study_end <- ymd(20201231)
if (thisdatasource=="EFEMERIS") study_end <- ymd(20201231)
if (thisdatasource=="UOSL") study_end <- ymd(20191231)
if (thisdatasource=="FERR") study_end <- ymd(20201231)

y_end<-year(study_end)

#----------------------------
# admissible gap between observation periods (DAP-specific)
admissible_gap_obs_periods <- vector(mode="list")
admissible_gap_obs_periods[['ARS']] <- 365
admissible_gap_obs_periods[['TEST']] <- 365

gap_days <- ifelse(thisdatasource %not in% names(admissible_gap_obs_periods),
                   1, admissible_gap_obs_periods[[thisdatasource]])



# define number of days a spells should not be shorter
min_spell_lenght<-365

# recommended_start_date_vect <- vector(mode = "list")
# recommended_start_date_vect[['ARS']] <- ymd(20030101)
# recommended_start_date_vect[['TEST']] <- ymd(20030101)
# recommended_start_date_vect[['THL']] <- ymd(20150101)
# recommended_start_date_vect[['EFEMERIS']] <- ymd(20050101)
# recommended_start_date_vect[['UOSL']] <- ymd(20090101)
# recommended_start_date_vect[['RDRU_FISABIO']] <- ymd(20100101)
# recommended_start_date_vect[['SAIL Databank']] <- ymd(19980101)
# recommended_start_date_vect[['SNDS']] <- ymd(20180101)
# #add Emilia Romagna
# 
# 
# if (thisdatasource %not in% names(recommended_start_date_vect)) {
#         stop(paste0("DATASOURCE not present inside the list: ",
#                     paste(names(recommended_start_date_vect), collapse = ", "),
#                     ".\nPlease open an issue"))
# }
# 
# 
# recommended_start_date <- recommended_start_date_vect[[thisdatasource]]
# rm(recommended_start_date_vect)

DAP_with_pregnancy_only<-c("EFEMERIS", "THL","RDRU_FISABIO")
