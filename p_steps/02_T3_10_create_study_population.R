#---------------------------------------------------------------
# Apply exclusion criteria to create study population 

# input: D3_selection_criteria_from_PERSONS_to_study_population, D3_selection_criteria_from_PERSONS_to_children_study_population
# output: Flowchart_exclusion_criteria_children, Flowchart_exclusion_criteria, D4_study_population, D4_children_study_population

print('FLOWCHART')

# USE THE FUNCTION CREATEFLOWCHART TO SELECT THE SUBJECTS IN POPULATION


  # Create flowchart for adults and save D4_study_population
  load(paste0(dirtemp,"D3_selection_criteria_from_PERSONS_to_study_population_pregnancy.RData"))
  selection_criteria <- get("D3_selection_criteria_from_PERSONS_to_study_population_pregnancy")
  
  selected_population <- CreateFlowChart(
    dataset = selection_criteria,
    listcriteria = c("sex_is_not_defined", "not_female","never_prescribed_gaba","not_in_relevant_age_at_LMP", "no_spells",
                     "all_spells_start_after_ending", "no_spell_overlapping_the_study_period",
                     "no_spell_longer_than_x_days"),
    flowchartname = "Flowchart_exclusion_criteria")
  
  fwrite(get("Flowchart_exclusion_criteria"),
         paste0(direxp, "Flowchart_exclusion_criteria"))
  
  #selected_population <- selected_population[, .(person_id, study_entry_date, study_exit_date)]
  
  nameoutput <- "D4_study_population"
  assign(nameoutput, selected_population)
  save(nameoutput, file = paste0(diroutput, nameoutput, ".RData"), list = nameoutput)
  rm(list = nameoutput)

