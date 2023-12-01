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
D3_sel_cri <- D3_sel_cri[, .(person_id,birth_date, sex_or_birth_date_is_not_defined,not_female,too_old_at_study_start,too_old_at_study_end )]


smart_load("D3_clean_spells", dirtemp, extension = "rds")

D3_clean_spells <- D3_clean_spells[, c("birth_date", "death_date", "entry_spell_category_crude",
                                       "exit_spell_category_crude", "op_meaning", "num_spell") := NULL]

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

# Creation of too_young_female criteria
D3_clean_spells[removed_row == 0, tot_spell_num := .N, by = person_id]
D3_clean_spells[removed_row == 0, tot_too_young_at_exit_spell := sum(too_young_at_exit_spell), by = person_id]
D3_clean_spells[removed_row == 0, too_young_female := fifelse(tot_too_young_at_exit_spell == tot_spell_num, 1, 0)]
D3_clean_spells[removed_row == 0, removed_row := rowSums(.SD), .SDcols = c("removed_row", "too_young_at_exit_spell")]
D3_clean_spells[, c("too_young_at_exit_spell", "tot_too_young_at_exit_spell", "tot_spell_num") := NULL]

# Creation of too_young_female criteria
D3_clean_spells[removed_row == 0, tot_spell_num := .N, by = person_id]
D3_clean_spells[removed_row == 0, tot_too_old_at_start_spell := sum(too_old_at_start_spell), by = person_id]
D3_clean_spells[removed_row == 0, too_old_female := fifelse(tot_too_old_at_start_spell == tot_spell_num, 1, 0)]
D3_clean_spells[removed_row == 0, removed_row := rowSums(.SD), .SDcols = c("removed_row", "too_old_at_start_spell")]
D3_clean_spells[, c("too_old_at_start_spell", "tot_too_old_at_start_spell", "tot_spell_num") := NULL]

# Creation of no_spell_longer_than_12_months. Keep other spells even if they are less than 365 days long
D3_clean_spells[removed_row == 0, tot_spell_num := .N, by = person_id]
D3_clean_spells[removed_row == 0, tot_x_days := sum(spell_less_than_12_months), by = person_id]
D3_clean_spells[removed_row == 0, no_spell_longer_than_12_months := fifelse(tot_x_days == tot_spell_num, 1, 0)]
D3_clean_spells[removed_row == 0, removed_row := rowSums(.SD),
                .SDcols = c("removed_row", "spell_less_than_12_months")]
D3_clean_spells[, c("spell_less_than_12_months", "tot_x_days", "tot_spell_num","removed_row") := NULL]


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


