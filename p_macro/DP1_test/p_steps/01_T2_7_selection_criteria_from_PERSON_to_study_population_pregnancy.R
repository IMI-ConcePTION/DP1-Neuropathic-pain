# -----------------------------------------------------
# CREATE EXCLUSION CRITERIA for persons/spells

# input: D3_PERSONS, OBSERVATION_PERIODS, output_spells_category
# output: D3_sel_cri

print('CREATE EXCLUSION CRITERIA FOR STUDY POPULATION')

load(paste0(dirpregnancyinput, "D3_pregnancy_final.RData"))


if (thisdatasource=="EFEMERIS") D3_pregnancy_final<-D3_pregnancy_final[CONCEPTSETS=="no",]

D3_sel_cri <- D3_pregnancy_final[,.(person_id,pregnancy_id,pregnancy_start_date,pregnancy_end_date,sex_at_instance_creation,age_at_start_of_pregnancy)]

D3_sel_cri[,pregnancy_not_instudy := fifelse(
  pregnancy_start_date>=study_start & pregnancy_start_date<=study_end, 0, 1)]

#select women between 15 and 49 years of age at LMP
D3_sel_cri <- D3_sel_cri[, not_in_relevant_age_at_LMP := fifelse(
  age_at_start_of_pregnancy>14 &  age_at_start_of_pregnancy<50, 0, 1)]


#create a small table with the number of pregnancies in the right age band
temp<-D3_sel_cri[pregnancy_not_instudy==0 & not_in_relevant_age_at_LMP==0,]
number_pregnancies<-length(unique(temp$pregnancy_id))

table <- data.frame(description="Pregnancies of women between 15 and 49 years of age",  N=number_pregnancies)

if (!(thisdatasource %in% DAP_with_pregnancy_only)){
  table1<-fread(paste0(direxp,"Table_number_of_women_and_pregnancies.csv"))
  table<-rbind(table,table1) 
  fwrite(table, paste0(direxp, "Table_number_of_women_and_pregnancies.csv"))
}else{
  fwrite(table, paste0(direxp, "Table_number_of_pregnancies.csv"))
}



#•	to receive at least one prescription/dispensing for pregabalin or gabapentin during 12 months before LMP date through the end of the pregnancy
smart_load("selected_dispensing_gabapentin_pregnancy", dirtemp, extension = "rds")

if (thisdatasource %in% DAP_with_pregnancy_only  ) {
  days_window<-91
}else{   
  days_window<-365
}



tmp2<-merge(unique(D3_pregnancy_final[,.(person_id,pregnancy_id, pregnancy_start_date, pregnancy_end_date )]),selected_dispensing_gabapentin_pregnancy,by="person_id",all.x = T,allow.cartesian = T)  
tmp2[, gabadisp_notin_1yb_or_duringpreg := fifelse(!is.na(date) &
                                                     date<=pregnancy_end_date & date>=pregnancy_start_date-days_window , 1, 0)]

tmp2[,preg_exposed:=max(gabadisp_notin_1yb_or_duringpreg,na.rm = T),by=c("pregnancy_id")]

tmp2<-unique(tmp2[,.(pregnancy_id,preg_exposed)])
tmp2[preg_exposed==1,preg_not_exposed_gaba:=0][preg_exposed==0,preg_not_exposed_gaba:=1]

tmp2<-unique(tmp2[,.(pregnancy_id,preg_not_exposed_gaba)])


D3_sel_cri<-merge(D3_sel_cri,tmp2,all.x=T,by=c("pregnancy_id"))



# D3_sel_cri[is.na(all_disp_gaba_not_passed_inclusion_criteria),all_disp_gaba_not_passed_inclusion_criteria:=1]
# D3_sel_cri[is.na(all_gabadisp_notin_1yb_or_duringpreg),all_gabadisp_notin_1yb_or_duringpreg:=1]

#pregabalin
smart_load("selected_dispensing_pregabalin_pregnancy", dirtemp, extension = "rds")


tmp2<-merge(unique(D3_pregnancy_final[,.(person_id,pregnancy_id, pregnancy_start_date, pregnancy_end_date )]),selected_dispensing_pregabalin_pregnancy,by="person_id",all.x = T,allow.cartesian = T)  

tmp2[, pregabadisp_notin_1yb_or_duringpreg := fifelse(!is.na(date) &
                                                        date<=pregnancy_end_date & date>=pregnancy_start_date-days_window , 1, 0)]

tmp2[,preg_exposed:=max(pregabadisp_notin_1yb_or_duringpreg,na.rm = T),by=c("pregnancy_id")]

tmp2<-unique(tmp2[,.(pregnancy_id,preg_exposed)])
tmp2[preg_exposed==1,preg_not_exposed_pregaba:=0][preg_exposed==0,preg_not_exposed_pregaba:=1]

tmp2<-unique(tmp2[,.(pregnancy_id,preg_not_exposed_pregaba)])


D3_sel_cri<-merge(D3_sel_cri,tmp2,all.x=T,by=c("pregnancy_id"))

if (!(thisdatasource %in% DAP_with_pregnancy_only)){
  smart_load("D4_study_population_gaba", diroutput, extension = "rds")
  D4_study_population_gaba[,in_pop_gaba:=1]
  
  smart_load("D4_study_population_pregaba", diroutput, extension = "rds")
  D4_study_population_pregaba[,in_pop_pregaba:=1]
  
  D3_sel_cri<-merge(D3_sel_cri,D4_study_population_gaba,all.x = T,by="person_id")
  
  D3_sel_cri[,pregnancy_notin_wocba_gaba_cohort := fifelse(
    !is.na(in_pop_gaba), 0, 1)]
  
  D3_sel_cri<-merge(D3_sel_cri,D4_study_population_pregaba,all.x = T,by="person_id")
  
  D3_sel_cri[,pregnancy_notin_wocba_pregaba_cohort := fifelse(
    !is.na(in_pop_pregaba), 0, 1)]
  
  D3_sel_cri[,pregnancy_notin_wocba_gaba_pregaba_cohort:=pmin(pregnancy_notin_wocba_gaba_cohort,pregnancy_notin_wocba_pregaba_cohort),by="pregnancy_id"]
  
  # Clean dataset
  D3_sel_cri <- D3_sel_cri[, .(person_id,pregnancy_id,pregnancy_not_instudy,pregnancy_notin_wocba_gaba_cohort,pregnancy_notin_wocba_pregaba_cohort,pregnancy_notin_wocba_gaba_pregaba_cohort,not_in_relevant_age_at_LMP,preg_not_exposed_gaba,preg_not_exposed_pregaba )]
  
  D3_sel_cri <- D3_sel_cri[, preg_not_exposed_gaba_pregaba:=pmin(preg_not_exposed_gaba,preg_not_exposed_pregaba),by="pregnancy_id"]
  
}else{
  
  # Remove persons with sex  missing 
  D3_sel_cri <- D3_sel_cri[, sex_is_not_defined := fifelse(
    is.na(sex_at_instance_creation) | sex_at_instance_creation == "U" , 1, 0)]
  
  #select only women
  D3_sel_cri <- D3_sel_cri[, not_female := fifelse(
    sex_at_instance_creation== "F", 0, 1)]
  
  # Clean dataset
  D3_sel_cri <- D3_sel_cri[, .(person_id,pregnancy_id,sex_is_not_defined,not_female ,pregnancy_not_instudy,not_in_relevant_age_at_LMP,preg_not_exposed_gaba, preg_not_exposed_pregaba )]
  
  D3_sel_cri <- D3_sel_cri[, preg_not_exposed_gaba_pregaba:=pmin(preg_not_exposed_gaba,preg_not_exposed_pregaba),by="pregnancy_id"]
  
}

smart_load("D3_clean_spells", dirtemp, extension = "rds")


D3_clean_spells <- D3_clean_spells[, .(person_id, entry_spell_category, exit_spell_category, starts_after_ending,
                                       no_overlap_study_period, spell_less_than_24_months)]


# Creation of no_spells criteria
D3_sel_cri <- D3_sel_cri[, no_spells := fifelse(person_id %in% unlist(unique(D3_clean_spells[, .(person_id)])), 0, 1)]

# Creation of all_spells_start_after_ending criteria
D3_clean_spells[, tot_spell_num := .N, by = person_id]
D3_clean_spells[, tot_starts_after_ending := sum(starts_after_ending), by = person_id]
D3_clean_spells[, all_spells_start_after_ending := fifelse(tot_starts_after_ending == tot_spell_num, 1, 0)]
D3_clean_spells[, removed_row := starts_after_ending]
D3_clean_spells[, c("starts_after_ending", "tot_starts_after_ending", "tot_spell_num") := NULL]

# Creation of no_spell_overlapping_the_study_period criteria
D3_clean_spells[removed_row == 0, tot_spell_num := .N, by = person_id]
D3_clean_spells[removed_row == 0, tot_no_overlap_study_period := sum(no_overlap_study_period), by = person_id]
D3_clean_spells[removed_row == 0, no_spell_overlapping_the_study_period := fifelse(
  tot_no_overlap_study_period == tot_spell_num, 1, 0)]
D3_clean_spells[removed_row == 0, removed_row := rowSums(.SD), .SDcols = c("removed_row", "no_overlap_study_period")]
D3_clean_spells[, c("no_overlap_study_period", "tot_no_overlap_study_period", "tot_spell_num") := NULL]

# Creation of no_spell_longer_than_24_months. Keep other spells even if they are less than 365 days long
D3_clean_spells[removed_row == 0, tot_spell_num := .N, by = person_id]
D3_clean_spells[removed_row == 0, tot_x_days := sum(spell_less_than_24_months), by = person_id]
D3_clean_spells[removed_row == 0, no_spell_longer_than_24_months := fifelse(tot_x_days == tot_spell_num, 1, 0)]
D3_clean_spells[removed_row == 0, removed_row := rowSums(.SD),
                .SDcols = c("removed_row", "spell_less_than_24_months")]
D3_clean_spells[, c("spell_less_than_24_months", "tot_x_days", "tot_spell_num") := NULL]



# # Keep only one row for each spell which syntethize the previously defined exclusion criteria
D3_clean_spells <- unique(D3_clean_spells[, c("entry_spell_category", "exit_spell_category") := NULL])
for (i in names(D3_clean_spells)) D3_clean_spells[is.na(get(i)), (i) := 0]
D3_clean_spells <- D3_clean_spells[, lapply(.SD, max), by = person_id]

# Add spells exclusion criteria to the one for person. Keep only persons which have a spell
D3_sel_cri_spells <- merge(D3_sel_cri, D3_clean_spells,
                           all.x = T, by = "person_id")  #all.y=T


smart_save(D3_sel_cri_spells, dirtemp, override_name = "D3_selection_criteria_from_PERSONS_to_study_population_pregnancy", extension = "rds")


fwrite(D3_sel_cri_spells, file=paste0(dirtemp,"D3_selection_criteria_from_PERSONS_to_study_population_pregnancy.csv"))
