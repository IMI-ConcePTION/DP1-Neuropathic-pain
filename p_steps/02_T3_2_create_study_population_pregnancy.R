

if (!(thisdatasource %in% DAP_with_pregnancy_only)){
  # Create flowchart for adults and save D4_study_population
  smart_load("D3_selection_criteria_from_PERSONS_to_study_population_pregnancy", dirtemp, extension = "rds")
  
  selected_population_pregnancy <- unique(CreateFlowChart(
    dataset = D3_selection_criteria_from_PERSONS_to_study_population_pregnancy,
    listcriteria = c("pregnancy_not_instudy","not_in_relevant_age_at_LMP", "pregnancy_notin_wocba_cohort"),
    flowchartname = "Flowchart_exclusion_criteria_pregnancy"))
  
  fwrite(get("Flowchart_exclusion_criteria_pregnancy"),
         paste0(direxp, "Flowchart_exclusion_criteria_pregnancy.csv"))
  
  smart_save(selected_population_pregnancy[, .(person_id,pregnancy_id)], diroutput, override_name = "D4_study_population_pregnancy", extension = "rds")
  
  fwrite(selected_population_pregnancy[, .(person_id,pregnancy_id)],
         paste0(diroutput, "D4_study_population_pregnancy.csv"))
  
  print(paste0(length(unique(selected_population_pregnancy$pregnancy_id)) ," pregnancies were included in the pregnancy sub population"))
  

}else{
  # Create flowchart for adults and save D4_study_population
  smart_load("D3_selection_criteria_from_PERSONS_to_study_population_pregnancy", dirtemp, extension = "rds")
  
  selected_population_pregnancy <- unique(CreateFlowChart(
    dataset = D3_selection_criteria_from_PERSONS_to_study_population_pregnancy,
    listcriteria = c("sex_is_not_defined","not_female","pregnancy_not_instudy","not_in_relevant_age_at_LMP", "no_spells",
                     "all_spells_start_after_ending", "no_spell_overlapping_the_study_period","no_spell_longer_than_12_months"),
    flowchartname = "Flowchart_exclusion_criteria_pregnancy"))
  
  fwrite(get("Flowchart_exclusion_criteria_pregnancy"),
         paste0(direxp, "Flowchart_exclusion_criteria_pregnancy.csv"))
  
  smart_save(selected_population_pregnancy[, .(person_id,pregnancy_id)], diroutput, override_name = "D4_study_population_pregnancy", extension = "rds")
  
  fwrite(selected_population_pregnancy[, .(person_id,pregnancy_id)],
         paste0(diroutput, "D4_study_population_pregnancy.csv"))
  
  print(paste0(length(unique(selected_population_pregnancy$pregnancy_id)) ," pregnancies were included in the pregnancy sub population"))
  
}


# #create  dataset flowchart_source_popolation containing one row for the pregnancy population and one row for the childbearing age one.
# # It include the number of pregnancies in the study period between 15 and 50 years of age and for some DAPs the number of women 
# 
# intermediate_population <- unique(CreateFlowChart(
#   dataset = D3_selection_criteria_from_PERSONS_to_study_population_pregnancy,
#   listcriteria = c("pregnancy_not_instudy","not_in_relevant_age_at_LMP"),
#   flowchartname = "Flowchart_source_popolation"))
# 
# fwrite(get("Flowchart_source_popolation"),
#        paste0(dirtemp, "Flowchart_source_popolation_pregnancy.csv"))


