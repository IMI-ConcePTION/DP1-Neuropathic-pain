# Create D3_study_pop

# input: D3_output_spells_category
# output: D3_clean_spells

# Load person_id in study_populations
smart_load("D4_study_population_pregnancy", diroutput, extension = extension)

load(paste0(dirpregnancyinput, "D3_pregnancy_final.RData"))
D3_pregnancy_final<-D3_pregnancy_final[!is.na(pregnancy_end_date) & !is.na(pregnancy_start_date),]


# Merge all datasets
D4_study_population_pregnancy <- merge(D4_study_population_pregnancy, D3_pregnancy_final[,.(pregnancy_id,pregnancy_start_date,pregnancy_end_date)], all.x = T, by = "pregnancy_id")

D4_study_population_pregnancy<-D4_study_population_pregnancy[!is.na(pregnancy_start_date) & pregnancy_start_date<=pregnancy_end_date,]

# Import the spells and clean
smart_load("D3_clean_spells", dirtemp, extension = extension)
D3_clean_spells <- D3_clean_spells[, .(person_id, entry_spell_category, exit_spell_category, birth_date,
                                       is_the_study_spell)]
D3_clean_spells <- D3_clean_spells[is_the_study_spell == 1, ][, is_the_study_spell := NULL]

# Set keys and then foverlaps to find the events inside each spell
setkey(D4_study_population_pregnancy, person_id, pregnancy_start_date, pregnancy_end_date)
setkey(D3_clean_spells, person_id, entry_spell_category, exit_spell_category)
D4_study_population_pregnancy_merged <- better_foverlaps(D4_study_population_pregnancy, D3_clean_spells, by.x = key(D4_study_population_pregnancy))

D4_study_population_pregnancy_merged<-D4_study_population_pregnancy_merged[!is.na(entry_spell_category),]


# Calculate cohort entry and exit date (censor for age and study_start)
D4_study_population_pregnancy_merged[, cohort_entry_date := pmax(entry_spell_category, study_start, birth_date + ceiling(15 * 365.25))]
D4_study_population_pregnancy_merged[, cohort_exit_date := pmin(exit_spell_category, birth_date + floor(50 * 365.25) - 1)]
D4_study_population_pregnancy_merged[, exit_spell_category := NULL][, entry_spell_category := NULL][,birth_date:=NULL]

# load(paste0(dirpregnancyinput, "D3_pregnancy_final.RData"))
# D4_study_population_pregnancy<-merge(D4_study_population_pregnancy,D3_pregnancy_final[,.(pregnancy_id,pregnancy_start_date,pregnancy_end_date)],all.x = T,by="pregnancy_id")

D4_study_population_pregnancy<-D4_study_population_pregnancy_merged[,calendar_year_LMP:=year(pregnancy_start_date)]


# Save the dataset
smart_save(D4_study_population_pregnancy, diroutput, extension = extension, save_copy = "csv")


