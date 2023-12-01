

if (!(thisdatasource %in% DAP_with_pregnancy_only)){
  # Create flowchart for adults and save D4_study_population
  smart_load("D3_selection_criteria_from_PERSONS_to_study_population_pregnancy", dirtemp, extension = "rds")
  
  selected_population_gabapentin_pregnancy <- unique(CreateFlowChart(
    dataset = D3_selection_criteria_from_PERSONS_to_study_population_pregnancy,
    listcriteria = c("pregnancy_not_instudy","not_in_relevant_age_at_LMP", "pregnancy_notin_wocba_gaba_cohort","preg_not_exposed_gaba"),
    flowchartname = "Flowchart_exclusion_criteria_gaba_pregnancy"))
  
  fwrite(get("Flowchart_exclusion_criteria_gaba_pregnancy"),
         paste0(direxp, "Flowchart_exclusion_criteria_gaba_pregnancy.csv"))
  
  smart_save(selected_population_gabapentin_pregnancy[, .(person_id,pregnancy_id)], diroutput, override_name = "D4_study_population_gaba_pregnancy", extension = "rds")
  
  fwrite(selected_population_gabapentin_pregnancy[, .(person_id,pregnancy_id)],
         paste0(diroutput, "D4_study_population_gaba_pregnancy.csv"))
  
  print(paste0(length(unique(selected_population_gabapentin_pregnancy$pregnancy_id)) ," pregnancies were included in the pregnancy sub population for gabapentin"))
  

  
# PREGABALIN  
  selected_population_pregabalin_pregnancy <- unique(CreateFlowChart(
    dataset = D3_selection_criteria_from_PERSONS_to_study_population_pregnancy,
    listcriteria = c("pregnancy_not_instudy", "pregnancy_notin_wocba_pregaba_cohort","not_in_relevant_age_at_LMP", "preg_not_exposed_pregaba"),
    flowchartname = "Flowchart_exclusion_criteria_pregaba_pregnancy"))
  
  
  
  fwrite(get("Flowchart_exclusion_criteria_pregaba_pregnancy"),
         paste0(direxp, "Flowchart_exclusion_criteria_pregaba_pregnancy.csv"))
  
  smart_save(selected_population_pregabalin_pregnancy[, .(person_id,pregnancy_id)], diroutput, override_name = "D4_study_population_pregaba_pregnancy", extension = "rds")
  
  fwrite(selected_population_pregabalin_pregnancy[, .(person_id,pregnancy_id)],
         paste0(diroutput, "D4_study_population_pregaba_pregnancy.csv"))
  
  print(paste0(length(unique(selected_population_pregabalin_pregnancy$pregnancy_id)) ," pregnancies were included in the pregnancy sub population for pregabalin"))
  
  
  selected_population_any_pregnancy <- unique(CreateFlowChart(
    dataset = D3_selection_criteria_from_PERSONS_to_study_population_pregnancy,
    listcriteria = c("pregnancy_not_instudy","not_in_relevant_age_at_LMP", "pregnancy_notin_wocba_gaba_pregaba_cohort", "preg_not_exposed_gaba_pregaba"),
    flowchartname = "Flowchart_exclusion_criteria_any_pregnancy"))
  
  
  
  fwrite(get("Flowchart_exclusion_criteria_any_pregnancy"),
         paste0(direxp, "Flowchart_exclusion_criteria_any_pregnancy.csv"))
  
  smart_save(selected_population_any_pregnancy[, .(person_id,pregnancy_id)], diroutput, override_name = "D4_study_population_any_pregnancy", extension = "rds")
  
  fwrite(selected_population_any_pregnancy[, .(person_id,pregnancy_id)],
         paste0(diroutput, "D4_study_population_any_pregnancy.csv"))
  
  print(paste0(length(unique(selected_population_any_pregnancy$pregnancy_id)) ," pregnancies were included in the pregnancy sub population for any gabapentinoids"))
  
}else{
  # Create flowchart for adults and save D4_study_population
  smart_load("D3_selection_criteria_from_PERSONS_to_study_population_pregnancy", dirtemp, extension = "rds")
  
  selected_population_gabapentin_pregnancy <- unique(CreateFlowChart(
    dataset = D3_selection_criteria_from_PERSONS_to_study_population_pregnancy,
    listcriteria = c("sex_is_not_defined","not_female","pregnancy_not_instudy","not_in_relevant_age_at_LMP", "preg_not_exposed_gaba", "no_spells",
                     "all_spells_start_after_ending", "no_spell_overlapping_the_study_period","no_spell_longer_than_24_months"),
    flowchartname = "Flowchart_exclusion_criteria_gaba_pregnancy"))
  
  fwrite(get("Flowchart_exclusion_criteria_gaba_pregnancy"),
         paste0(direxp, "Flowchart_exclusion_criteria_gaba_pregnancy.csv"))
  
  smart_save(selected_population_gabapentin_pregnancy[, .(person_id,pregnancy_id)], diroutput, override_name = "D4_study_population_gaba_pregnancy", extension = "rds")
  
  fwrite(selected_population_gabapentin_pregnancy[, .(person_id,pregnancy_id)],
         paste0(diroutput, "D4_study_population_gaba_pregnancy.csv"))
  
  print(paste0(length(unique(selected_population_gabapentin_pregnancy$pregnancy_id)) ," pregnancies were included in the pregnancy sub population for gabapentin"))
  
  selected_population_pregabalin_pregnancy <- unique(CreateFlowChart(
    dataset = D3_selection_criteria_from_PERSONS_to_study_population_pregnancy,
    listcriteria = c("sex_is_not_defined","not_female","not_in_relevant_age_at_LMP","pregnancy_not_instudy", "preg_not_exposed_pregaba", "no_spells",
                     "all_spells_start_after_ending", "no_spell_overlapping_the_study_period","no_spell_longer_than_24_months"),
    flowchartname = "Flowchart_exclusion_criteria_pregaba_pregnancy"))
  
  
  
  fwrite(get("Flowchart_exclusion_criteria_pregaba_pregnancy"),
         paste0(direxp, "Flowchart_exclusion_criteria_pregaba_pregnancy.csv"))
  
  smart_save(selected_population_pregabalin_pregnancy[, .(person_id,pregnancy_id)], diroutput, override_name = "D4_study_population_pregaba_pregnancy", extension = "rds")
  fwrite(selected_population_pregabalin_pregnancy[, .(person_id,pregnancy_id)],
         paste0(diroutput, "D4_study_population_pregaba_pregnancy.csv"))
  
  print(paste0(length(unique(selected_population_pregabalin_pregnancy$pregnancy_id)) ," pregnancies were included in the pregnancy sub population for pregabalin"))
  
  
  #create study population: include women exposed to a gabapentinoids ---------
  
  selected_population_any_pregnancy <- unique(CreateFlowChart(
    dataset = D3_selection_criteria_from_PERSONS_to_study_population_pregnancy,
    listcriteria = c("sex_is_not_defined","not_female","not_in_relevant_age_at_LMP","pregnancy_not_instudy", "preg_not_exposed_gaba_pregaba", "no_spells",
                     "all_spells_start_after_ending", "no_spell_overlapping_the_study_period","no_spell_longer_than_24_months"),
    flowchartname = "Flowchart_exclusion_criteria_any_pregnancy"))
  
  
  
  fwrite(get("Flowchart_exclusion_criteria_any_pregnancy"),
         paste0(direxp, "Flowchart_exclusion_criteria_any_pregnancy.csv"))
  
  smart_save(selected_population_any_pregnancy[, .(person_id,pregnancy_id)], diroutput, override_name = "D4_study_population_any_pregnancy", extension = "rds")
  fwrite(selected_population_any_pregnancy[, .(person_id,pregnancy_id)],
         paste0(diroutput, "D4_study_population_any_pregnancy.csv"))
  
  
  print(paste0(length(unique(selected_population_any_pregnancy$pregnancy_id)) ," pregnancies were included in the pregnancy sub population for any gabapentinoids"))
  

}


#create  dataset flowchart_source_popolation containing one row for the pregnancy population and one row for the childbearing age one.
# It include the number of pregnancies in the study period between 15 and 50 years of age and for some DAPs the number of women 

intermediate_population <- unique(CreateFlowChart(
  dataset = D3_selection_criteria_from_PERSONS_to_study_population_pregnancy,
  listcriteria = c("pregnancy_not_instudy","not_in_relevant_age_at_LMP"),
  flowchartname = "Flowchart_source_popolation"))

fwrite(get("Flowchart_source_popolation"),
       paste0(dirtemp, "Flowchart_source_popolation_pregnancy.csv"))




#count number of dispensing linked to the population
smart_load("selected_dispensing_gabapentin_pregnancy", dirtemp, extension = "rds")
smart_load("selected_dispensing_pregabalin_pregnancy", dirtemp, extension = "rds")

selected_dispensing_any_pregnancy<-rbind(selected_dispensing_gabapentin_pregnancy,selected_dispensing_pregabalin_pregnancy,fill=T)


selected_dispensing_gabapentin_pregnancy[(person_id %in% selected_population_gabapentin_pregnancy$person_id),disp_linked_to_gaba_population:="gabapentin"]

selected_dispensing_pregabalin_pregnancy [(person_id %in% selected_population_pregabalin_pregnancy$person_id),disp_linked_to_pregaba_population:="pregabalin"]

selected_dispensing_any_pregnancy[(person_id %in% selected_population_any_pregnancy$person_id),disp_linked_to_anygaba_population:="any gabapentinoids"]


tab1<-selected_dispensing_gabapentin_pregnancy[,.N,by=c("disp_linked_to_gaba_population")][!is.na(disp_linked_to_gaba_population),]
setnames(tab1,"disp_linked_to_gaba_population","drug")
setnames(tab1,"N","N_disp_linked_to_population")
tab2<-selected_dispensing_pregabalin_pregnancy[,.N,by=c("disp_linked_to_pregaba_population")][!is.na(disp_linked_to_pregaba_population),]
setnames(tab2,"disp_linked_to_pregaba_population","drug")
setnames(tab2,"N","N_disp_linked_to_population")
tab3<-selected_dispensing_any_pregnancy[,.N,by=c("disp_linked_to_anygaba_population")][!is.na(disp_linked_to_anygaba_population),]
setnames(tab3,"disp_linked_to_anygaba_population","drug")
setnames(tab3,"N","N_disp_linked_to_population")

Tab<-rbind(tab1,tab2,tab3,fill=T)

fwrite(Tab,file=paste0(direxp,"Dispensing_linked_to_pregnancy_population.csv"))





