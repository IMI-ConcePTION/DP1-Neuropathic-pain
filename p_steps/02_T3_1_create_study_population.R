#---------------------------------------------------------------
# Apply exclusion criteria to create study population 

# USE THE FUNCTION CREATEFLOWCHART TO SELECT THE SUBJECTS IN POPULATION

print('FLOWCHART')

#WOMEN OF CHILDBEARING AGE -----------------------

# Create flowchart for adults and save D4_study_population
smart_load("D3_selection_criteria_from_PERSONS_to_study_population", dirtemp, extension = "rds")


selected_population <- unique(CreateFlowChart(
  dataset = D3_selection_criteria_from_PERSONS_to_study_population,
  listcriteria = c("sex_or_birth_date_is_not_defined", "not_female","too_young_female", "too_old_female","no_spells",
                   "all_spells_start_after_ending", "no_spell_overlapping_the_study_period","no_spell_longer_than_12_months"),
  flowchartname = "Flowchart_exclusion_criteria"))


fwrite(get("Flowchart_exclusion_criteria"),
       paste0(direxp, "Flowchart_exclusion_criteria.csv"))


smart_save(selected_population[, .(person_id)], diroutput, override_name = "D4_study_population", extension = "rds")

fwrite(selected_population[, .(person_id)], file=paste0(diroutput,"D4_study_population.csv"))

print(paste0(length(unique(selected_population$person_id)) ," women were included in the women of childbearing age population"))



