# CLEAN THE SPELLS

# input: D3_output_spells_category
# output: D3_clean_spells

# Load datasets
smart_load("D3_PERSONS", dirtemp, extension = "rds")
smart_load("D3_output_spells_category", dirtemp, extension = "rds")

# Combine persons and spells, then select only the column we need and create new ones
person_spell <- unique(merge(D3_output_spells_category, D3_PERSONS, all.x = T, by = "person_id"))
person_spell <- person_spell[, .(person_id, birth_date, death_date,entry_spell_category,exit_spell_category, op_meaning, num_spell)]


# Calculate cohort entry and exit date (censor for age and study_start)
person_spell[, entry_spell_category := pmax(entry_spell_category, birth_date,na.rm=T)]
person_spell[, exit_spell_category := pmin(exit_spell_category,death_date,na.rm=T )]


# find spells that end before they start (using original start/end)
person_spell[, starts_after_ending := data.table::fifelse(entry_spell_category <= exit_spell_category, 0, 1)]


# find spells that do not overlap the study period (using original start/end)
person_spell[, no_overlap_study_period := data.table::fifelse(
  entry_spell_category > study_end | exit_spell_category < study_start, 1, 0)]



# find spells that are shorter than x days (using cleaned start/end)
person_spell[, spell_less_than_24_months := data.table::fifelse(
  correct_difftime(exit_spell_category, entry_spell_category) < min_spell_lenght &
    thisdatasource %not in% DAP_with_pregnancy_only , 1, 0)]

# add a criteria that identify the specific spell of interest
person_spell[, is_the_study_spell := data.table::fifelse(starts_after_ending == 0 & no_overlap_study_period == 0 & spell_less_than_24_months == 0, 1, 0)]

smart_save(person_spell, dirtemp, override_name = "D3_clean_spells",extension="rds")
fwrite(person_spell, file=paste0(dirtemp,"D3_clean_spells.csv"))
#smart_save(person_spell, dirtemp, override_name = "D3_clean_spells", extension = extension)
