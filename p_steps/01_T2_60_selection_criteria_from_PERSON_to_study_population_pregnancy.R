# -----------------------------------------------------
# CREATE EXCLUSION CRITERIA for persons/spells

# input: D3_PERSONS, OBSERVATION_PERIODS, output_spells_category
# output: D3_sel_cri

print('CREATE EXCLUSION CRITERIA FOR STUDY POPULATION')

load(paste0(dirpregnancyinput, "D3_pregnancy_final.RData"))

if (thisdatasource=="EFEMERIS") D3_pregnancy_final<-D3_pregnancy_final[CONCEPTSETS=="no",]


D3_sel_cri <- D3_pregnancy_final[,.(person_id,pregnancy_id,pregnancy_start_date,pregnancy_end_date,sex_at_instance_creation,age_at_start_of_pregnancy)]

### Create the criteria based on D3_PERSONS. They are the same for adults and children populations.
# Remove persons with sex  missing 
D3_sel_cri <- D3_sel_cri[, sex_is_not_defined := fifelse(
  is.na(sex_at_instance_creation) | sex_at_instance_creation == "U" , 1, 0)]

#select only women
D3_sel_cri <- D3_sel_cri[, not_female := fifelse(
  sex_at_instance_creation== "F", 0, 1)]

#ongoing da togliere?

#never prescribd gabapentinoids
load(paste0(dirtemp, "flag_spells_containing_disp.RData"))

# if (this_datasource_has_multiple_obs_period== FALSE) {
#   flag_spells_containing_disp[,prescription_in_spells:=max(is_the_study_spell,na.rm=T),by="person_id"]
# }else{
#summaize the information per pregnanacy
  flag_spells_containing_disp[,prescription_in_spells:=max(spell_include_disp ,na.rm=T),by=c("person_id","codvar")]
#}

flag_spells_containing_disp<-unique(flag_spells_containing_disp[,.(person_id,codvar,prescription_in_spells)])

D3_sel_cri<-merge(D3_sel_cri,flag_spells_containing_disp,by="person_id",all.x = T)
D3_sel_cri <- D3_sel_cri[, prescription_notin_spells := fifelse(
  prescription_in_spells== 1, 0, 1)]

#select women between 15 and 49 years of age at LMP
D3_sel_cri <- D3_sel_cri[, not_in_relevant_age_at_LMP := fifelse(
  age_at_start_of_pregnancy>14 &  age_at_start_of_pregnancy<50, 0, 1)]



#exclution criteria: keep pregnancies with dispesations in the pregnancy or in the 1 year before
load(paste0(dirtemp, "D3_dispensing_during_observation_period_SAP1.RData"))
tmp<-unique(D3_pregnancy_final[,.(person_id,pregnancy_id,pregnancy_start_date, pregnancy_end_date)])
tmp<-merge(tmp,D3_dispensing_during_observation_period_SAP1,all.x=T)

tmp[, gabadisp_notin_1yb_or_duringpreg := fifelse(!is.na(date) &
                                                date<=pregnancy_end_date & date>=pregnancy_start_date-365 & codvar=="N03AX12", 0, 1)]

tmp[, pregaadisp_notin_1yb_or_duringpreg := fifelse(!is.na(date) &
                                                    date<=pregnancy_end_date & date>=pregnancy_start_date-365 & codvar=="N03AX16", 0, 1)]


tmp<-tmp[,.(person_id,pregnancy_id,gabadisp_notin_1yb_or_duringpreg,pregaadisp_notin_1yb_or_duringpreg)]

D3_sel_cri<-merge(D3_sel_cri,tmp,by=c("person_id","pregnancy_id"),all.x = T)
# Clean dataset
D3_sel_cri <- D3_sel_cri[, .(person_id,pregnancy_id, sex_is_not_defined, not_female,not_in_relevant_age_at_LMP,prescription_notin_spells,gabadisp_notin_1yb_or_duringpreg,pregaadisp_notin_1yb_or_duringpreg)]

load(paste0(dirtemp, "D3_clean_spells.RData"))

D3_clean_spells <- D3_clean_spells[, .(person_id, entry_spell_category, exit_spell_category, starts_after_ending,
                                       no_overlap_study_period, less_than_x_days_and_not_starts_at_birth,
                                       spell_include_disp)]


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

# Creation of no_spell_longer_than_x_days. Keep other spells even if they are less than 365 days long
D3_clean_spells[removed_row == 0, tot_spell_num := .N, by = person_id]
D3_clean_spells[removed_row == 0, tot_x_days := sum(less_than_x_days_and_not_starts_at_birth), by = person_id]
D3_clean_spells[removed_row == 0, no_spell_longer_than_x_days := fifelse(tot_x_days == tot_spell_num, 1, 0)]
D3_clean_spells[removed_row == 0, removed_row := rowSums(.SD),
                .SDcols = c("removed_row", "less_than_x_days_and_not_starts_at_birth")]
D3_clean_spells[, c("less_than_x_days_and_not_starts_at_birth", "tot_x_days", "tot_spell_num") := NULL]


# Keep only study spells chosen in 01_T2_043_clean_spells
study_spells <- D3_clean_spells[spell_include_disp == 1, ][, .(person_id, entry_spell_category, exit_spell_category)]
study_spells <- unique(study_spells)
D3_sel_cri_temp<-merge(study_spells,D3_sel_cri,all.y = T,by="person_id")

# Keep only one row for each spell which syntethize the previously defined exclusion criteria
D3_clean_spells <- unique(D3_clean_spells[, c("entry_spell_category", "exit_spell_category",
                                              "spell_include_disp") := NULL])
for (i in names(D3_clean_spells)) D3_clean_spells[is.na(get(i)), (i) := 0]
D3_clean_spells <- D3_clean_spells[, lapply(.SD, max), by = person_id]

# Add spells exclusion criteria to the one for person. Keep only persons which have a spell
D3_sel_cri_spells <- merge(D3_sel_cri_temp, D3_clean_spells,
                           all.x = T, by = "person_id")  #all.y=T


#D3_sel_cri_spells[, study_entry_date := pmax(entry_spell_category, start_lookback)]
D3_sel_cri_spells[, study_entry_date := entry_spell_category]
D3_sel_cri_spells[, study_exit_date := pmin(exit_spell_category, study_end)]
D3_sel_cri_spells[, c("entry_spell_category", "exit_spell_category") := NULL]

D3_sel_cri_spells<-unique(D3_sel_cri_spells)
# Saving exclusion criteria for populations
nameoutput1 <- "D3_selection_criteria_from_PERSONS_to_study_population_pregnancy"
assign(nameoutput1, D3_sel_cri_spells)
save(nameoutput1, file = paste0(dirtemp, nameoutput1, ".RData"), list = nameoutput1)



