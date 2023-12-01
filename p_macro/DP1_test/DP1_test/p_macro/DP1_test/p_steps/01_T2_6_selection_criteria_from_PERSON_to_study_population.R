# -----------------------------------------------------
# CREATE EXCLUSION CRITERIA for persons/spells

# input: D3_PERSONS, OBSERVATION_PERIODS, output_spells_category
# output: D3_sel_cri

print('CREATE EXCLUSION CRITERIA FOR STUDY POPULATION')

smart_load("D3_PERSONS", dirtemp, extension = "rds")

### Create the criteria based on D3_PERSONS. They are the same for adults and children populations.
# Remove persons with sex or birth day missing (recoded to year 9999)
D3_sel_cri <- D3_PERSONS[, sex_or_birth_date_is_not_defined := fifelse(
  is.na(sex_at_instance_creation) | sex_at_instance_creation == "U" | year(birth_date) == 9999, 1, 0)]

#select only women
D3_sel_cri <- D3_sel_cri[, not_female := fifelse(
  sex_at_instance_creation== "F", 0, 1)]

#create a small table with the number of women in the right age band at study start
D3_sel_cri[,age_studystart:=age_fast(birth_date,study_start)][,too_old_at_study_start:=fifelse(age_studystart>49,1,0)]
D3_sel_cri[,age_studyend:=age_fast(birth_date,study_end)][,too_old_at_study_end:=fifelse(age_studyend<15,1,0)]
temp<-D3_sel_cri[not_female==0 & too_old_at_study_start==0 & too_old_at_study_end ==0,]
number_women<-length(unique(temp$person_id))
table <- data.frame(description="Women between 15 and 49 years of age",  N=number_women)
fwrite(table, paste0(direxp, "Table_number_of_women_and_pregnancies.csv"))


# Clean dataset
D3_sel_cri <- D3_sel_cri[, .(person_id,birth_date, sex_or_birth_date_is_not_defined,not_female )]

#	to receive at least one prescription/dispensing for pregabalin or gabapentin between the start and end dates and to have at least 12 months of data available before and after the prescription/dispensing for gabapentinoids

smart_load("selected_dispensing_gabapentin", dirtemp,extension = "rds")

selected_dispensing_gabapentin_tmp<-unique(selected_dispensing_gabapentin[,disp_passed_inclusion_criteria:=1][,.(person_id,disp_passed_inclusion_criteria)])

D3_sel_cri<-merge(D3_sel_cri,selected_dispensing_gabapentin_tmp,all.x=T,by="person_id")

D3_sel_cri<-D3_sel_cri[disp_passed_inclusion_criteria==1,disp_gaba_not_passed_inclusion_criteria:=0]

D3_sel_cri<-D3_sel_cri[is.na(disp_gaba_not_passed_inclusion_criteria),disp_gaba_not_passed_inclusion_criteria:=1][,disp_passed_inclusion_criteria:=NULL]

#check: women receiving at least one gabapentinoids prescription/dispensing when she is between 15 and 49 years

age_check<-merge(D3_sel_cri[,.(person_id,birth_date)],unique(selected_dispensing_gabapentin[,.(person_id,date)]),all=F,by="person_id")

age_check[,age_at_disp:=age_fast(birth_date,date)][,age_at_right_range:=fifelse(age_at_disp>=15 & age_at_disp<=49,1,0)]

age_tmp<-unique(age_check[,age_at_right_range_perperson:=max(age_at_right_range),by="person_id"][,.(person_id,age_at_right_range_perperson)])
rm(age_check)

D3_sel_cri<-merge(D3_sel_cri,age_tmp,by="person_id",all.x=T)
rm(age_tmp)

D3_sel_cri[,no_gaba_disp_atright_age_range:=fifelse(age_at_right_range_perperson==0 | is.na(age_at_right_range_perperson),1,0)][,age_at_right_range_perperson:=NULL]

smart_load("selected_dispensing_pregabalin", dirtemp, extension = "rds")

selected_dispensing_pregabalin_tmp<-unique(selected_dispensing_pregabalin[,disp_passed_inclusion_criteria:=1][,.(person_id,disp_passed_inclusion_criteria)])

D3_sel_cri<-merge(D3_sel_cri,selected_dispensing_pregabalin_tmp,all.x=T,by="person_id")

D3_sel_cri<-D3_sel_cri[disp_passed_inclusion_criteria==1,disp_pregaba_not_passed_inclusion_criteria:=0]

D3_sel_cri<-D3_sel_cri[is.na(disp_pregaba_not_passed_inclusion_criteria),disp_pregaba_not_passed_inclusion_criteria:=1][,disp_passed_inclusion_criteria:=NULL]

###
D3_sel_cri<-D3_sel_cri[disp_pregaba_not_passed_inclusion_criteria==1 & disp_gaba_not_passed_inclusion_criteria==1, disp_gaba_pregaba_not_passed_inclusion_criteria:=1][is.na(disp_gaba_pregaba_not_passed_inclusion_criteria),disp_gaba_pregaba_not_passed_inclusion_criteria:=0]
  
#check: women receiving at least one gabapentinoids prescription/dispensing when she is between 15 and 49 years

age_check<-merge(D3_sel_cri[,.(person_id,birth_date)],unique(selected_dispensing_pregabalin[,.(person_id,date)]),all=F,by="person_id")

age_check[,age_at_disp:=age_fast(birth_date,date)][,age_at_right_range:=fifelse(age_at_disp>=15 & age_at_disp<=49,1,0)]

age_tmp<-unique(age_check[,age_at_right_range_perperson:=max(age_at_right_range),by="person_id"][,.(person_id,age_at_right_range_perperson)])

D3_sel_cri<-merge(D3_sel_cri,age_tmp,by="person_id",all.x=T)

D3_sel_cri[,no_pregaba_disp_atright_age_range:=fifelse(age_at_right_range_perperson==0 | is.na(age_at_right_range_perperson),1,0)][,age_at_right_range_perperson:=NULL]

D3_sel_cri[,no_gaba_pregaba_disp_atright_age_range:=pmin(no_gaba_disp_atright_age_range,no_pregaba_disp_atright_age_range),by="person_id"]




smart_load("D3_clean_spells", dirtemp, extension = "rds")

D3_clean_spells <- D3_clean_spells[, .(person_id, entry_spell_category, exit_spell_category, starts_after_ending,
                                       no_overlap_study_period,spell_less_than_24_months )]

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
D3_clean_spells[, c("spell_less_than_24_months", "tot_x_days", "tot_spell_num","removed_row") := NULL]

# Creation of all_spells_include_vax1_but_less than_365_days_from_it
# D3_clean_spells[removed_row == 0, tot_spell_num := .N, by = person_id]
# D3_clean_spells[removed_row == 0, tot_less_than_365_days := sum(has_vax1_before_365_days), by = person_id]
# D3_clean_spells[removed_row == 0, all_spells_include_vax1_but_less_than_365_days_from_it := fifelse(
#   tot_less_than_365_days == tot_spell_num, 1, 0)]
# D3_clean_spells[removed_row == 0, removed_row := rowSums(.SD),
#                 .SDcols = c("removed_row", "has_vax1_before_365_days")]
# D3_clean_spells[, c("has_vax1_before_365_days", "tot_less_than_365_days", "tot_spell_num", "removed_row") := NULL]

spells_per_person<-unique(D3_clean_spells[,.(person_id,entry_spell_category,exit_spell_category)])
# # Keep only one row for each spell which syntethize the previously defined exclusion criteria
D3_clean_spells <- unique(D3_clean_spells[, c("entry_spell_category", "exit_spell_category") := NULL])
for (i in names(D3_clean_spells)) D3_clean_spells[is.na(get(i)), (i) := 0]
D3_clean_spells <- D3_clean_spells[, lapply(.SD, max), by = person_id]

# Add spells exclusion criteria to the one for person. Keep only persons which have a spell
D3_sel_cri_spells <- merge(D3_sel_cri, D3_clean_spells,
                           all.x = T, by = "person_id")  #all.y=T



smart_save(D3_sel_cri_spells, dirtemp, override_name = "D3_selection_criteria_from_PERSONS_to_study_population", extension = "rds")

fwrite(D3_sel_cri_spells, file=paste0(dirtemp,"D3_selection_criteria_from_PERSONS_to_study_population.csv"))


