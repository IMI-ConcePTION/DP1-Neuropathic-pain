#---------------------------------------------------------------
# Apply exclusion criteria to create study population 

# input: D3_selection_criteria_from_PERSONS_to_study_population, D3_selection_criteria_from_PERSONS_to_children_study_population
# output: Flowchart_exclusion_criteria_children, Flowchart_exclusion_criteria, D4_study_population, D4_children_study_population

# USE THE FUNCTION CREATEFLOWCHART TO SELECT THE SUBJECTS IN POPULATION

print('FLOWCHART')

#WOMEN OF CHILDBEARING AGE -----------------------

# Create flowchart for adults and save D4_study_population
smart_load("D3_selection_criteria_from_PERSONS_to_study_population", dirtemp, extension = "rds")


selected_population_gabapentin <- unique(CreateFlowChart(
  dataset = D3_selection_criteria_from_PERSONS_to_study_population,
  listcriteria = c("sex_or_birth_date_is_not_defined", "not_female","no_gaba_disp_atright_age_range","disp_gaba_not_passed_inclusion_criteria",  "no_spells",
                   "all_spells_start_after_ending", "no_spell_overlapping_the_study_period","no_spell_longer_than_24_months"),
  flowchartname = "Flowchart_exclusion_criteria_gaba"))


fwrite(get("Flowchart_exclusion_criteria_gaba"),
       paste0(direxp, "Flowchart_exclusion_criteria_gaba.csv"))


smart_save(selected_population_gabapentin[, .(person_id)], diroutput, override_name = "D4_study_population_gaba", extension = "rds")

fwrite(selected_population_gabapentin[, .(person_id)], file=paste0(diroutput,"D4_study_population_gaba.csv"))

print(paste0(length(unique(selected_population_gabapentin$person_id)) ," women were included in the women of childbearing age population for gabapentin"))

#pregabalin
selected_population_pregabalin <- unique(CreateFlowChart(
  dataset = D3_selection_criteria_from_PERSONS_to_study_population,
  listcriteria = c("sex_or_birth_date_is_not_defined", "not_female", "no_pregaba_disp_atright_age_range","disp_pregaba_not_passed_inclusion_criteria", "no_spells",
                   "all_spells_start_after_ending", "no_spell_overlapping_the_study_period", "no_spell_longer_than_24_months"),
  flowchartname = "Flowchart_exclusion_criteria_pregaba"))


fwrite(get("Flowchart_exclusion_criteria_pregaba"),
       paste0(direxp, "Flowchart_exclusion_criteria_pregaba.csv"))

# 
smart_save(selected_population_pregabalin[, .(person_id)], diroutput, override_name = "D4_study_population_pregaba", extension = "rds")
fwrite(selected_population_pregabalin[, .(person_id)], file=paste0(diroutput,"D4_study_population_pregaba.csv"))

print(paste0(length(unique(selected_population_pregabalin$person_id)) ," women were included in the women of childbearing age population for pregabalin"))

#any gabapentinoid
selected_population_any <- unique(CreateFlowChart(
  dataset = D3_selection_criteria_from_PERSONS_to_study_population,
  listcriteria = c("sex_or_birth_date_is_not_defined", "not_female","no_gaba_pregaba_disp_atright_age_range","disp_gaba_pregaba_not_passed_inclusion_criteria",  "no_spells",
                   "all_spells_start_after_ending", "no_spell_overlapping_the_study_period", "no_spell_longer_than_24_months"),
  flowchartname = "Flowchart_exclusion_criteria_any"))


fwrite(get("Flowchart_exclusion_criteria_any"),
       paste0(direxp, "Flowchart_exclusion_criteria_any.csv"))


 smart_save(selected_population_any[, .(person_id)], diroutput, override_name = "D4_study_population_any", extension = "rds")

fwrite(selected_population_any[, .(person_id)], file=paste0(diroutput,"D4_study_population_any.csv"))


print(paste0(length(unique(selected_population_any$person_id)) ," women were included in the women of childbearing age population for any gabapentinoids"))

intermediate_population <- unique(CreateFlowChart(
  dataset = D3_selection_criteria_from_PERSONS_to_study_population,
  listcriteria = c("sex_or_birth_date_is_not_defined", "not_female","no_gaba_pregaba_disp_atright_age_range"),
  flowchartname = "Flowchart_source_popolation_general_pop"))

fwrite(get("Flowchart_source_popolation_general_pop"),
       paste0(dirtemp, "Flowchart_source_popolation_general_pop.csv"))


#count number of dispensing linked to the population
smart_load("selected_dispensing_gabapentin", dirtemp, extension = "rds")
smart_load("selected_dispensing_pregabalin", dirtemp, extension = "rds")

selected_dispensing_any<-rbind(selected_dispensing_gabapentin,selected_dispensing_pregabalin,fill=T)


selected_dispensing_gabapentin[(person_id %in% selected_population_gabapentin$person_id),disp_linked_to_gaba_population:="gabapentin"]

selected_dispensing_pregabalin[(person_id %in% selected_population_pregabalin$person_id),disp_linked_to_pregaba_population:="pregabalin"]

selected_dispensing_any[(person_id %in% selected_population_any$person_id),disp_linked_to_anygaba_population:="any gabapentinoids"]


tab1<-selected_dispensing_gabapentin[,.N,by=c("disp_linked_to_gaba_population")][!is.na(disp_linked_to_gaba_population),]
setnames(tab1,"disp_linked_to_gaba_population","drug")
setnames(tab1,"N","N_disp_linked_to_population")
tab2<-selected_dispensing_pregabalin[,.N,by=c("disp_linked_to_pregaba_population")][!is.na(disp_linked_to_pregaba_population),]
setnames(tab2,"disp_linked_to_pregaba_population","drug")
setnames(tab2,"N","N_disp_linked_to_population")
tab3<-selected_dispensing_any[,.N,by=c("disp_linked_to_anygaba_population")][!is.na(disp_linked_to_anygaba_population),]
setnames(tab3,"disp_linked_to_anygaba_population","drug")
setnames(tab3,"N","N_disp_linked_to_population")

Tab<-rbind(tab1,tab2,tab3,fill=T)

fwrite(Tab,file=paste0(direxp,"Dispensing_linked_to_population.csv"))