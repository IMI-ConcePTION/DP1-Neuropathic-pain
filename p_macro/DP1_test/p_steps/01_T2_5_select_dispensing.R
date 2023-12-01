###################  GABAPENTIN
#keep only dispensations overlapping the spells and with at least 1 year of distance from entry spell and 1 year from exit spell
load(paste0(dirconceptsets,"GABAPENTIN.RData"))

GABAPENTIN<-unique(GABAPENTIN[!is.na(date)][, .(person_id,medicinal_product_id,codvar,date ,indication_code,prescriber_speciality)])

GABAPENTIN[,date2:=date]
setkeyv(GABAPENTIN,c("person_id","date","date2"))

smart_load("D3_clean_spells", dirtemp, extension = ".rds")


dispensing_selection_base<-D3_clean_spells[starts_after_ending==0,]



person_spells_tmp<-unique(dispensing_selection_base[!is.na(entry_spell_category),.(person_id,entry_spell_category,exit_spell_category,birth_date,death_date)])


setkeyv(person_spells_tmp,c("person_id","entry_spell_category","exit_spell_category"))

flag_spells_containing_disp_gaba<-unique(better_foverlaps(person_spells_tmp,GABAPENTIN))

flag_spells_containing_disp_gaba<-flag_spells_containing_disp_gaba[!is.na(date),]

flag_spells_containing_disp_gaba<-unique(flag_spells_containing_disp_gaba[,spell_not_include_disp_gaba:=0][,date2:=NULL])

selection_criteria_disp_gaba<-merge(GABAPENTIN,unique(flag_spells_containing_disp_gaba[,.(person_id,date,spell_not_include_disp_gaba,entry_spell_category,exit_spell_category,death_date)]),all.x=T,by=c("person_id","date"))

selection_criteria_disp_gaba<-selection_criteria_disp_gaba[is.na(entry_spell_category),spell_not_include_disp_gaba:=1]

# # Censor spells which start before the recommended start date
# selection_criteria_disp_gaba<-selection_criteria_disp_gaba[!is.na(entry_spell_category), entry_spell_category := pmax(entry_spell_category, recommended_start_date, na.rm = T)]

# # Censor spells which ends after the death date, study_end or 50th birthday
# if(study_end>ymd(20191231)){
#   study_end_tmp<-study_end
# }else{
#   study_end_tmp<-study_end+1
# }
# 
# 
# selection_criteria_disp_gaba[!is.na(exit_spell_category), exit_spell_category := pmin(exit_spell_category, death_date, study_end_tmp, na.rm = T)]


if (!(thisdatasource %in% DAP_with_pregnancy_only )) {
  #distanza tra disp e entry e disp e exit
  selection_criteria_disp_gaba[,dist_entry_disp:=difftime(date,entry_spell_category,unit="days")]
  selection_criteria_disp_gaba[,dist_exit_disp:=difftime(exit_spell_category,date,unit="days")]
  
  selection_criteria_disp_gaba<-selection_criteria_disp_gaba[,not_y1_lb_or_y1_fup := fifelse(dist_entry_disp<365 | dist_exit_disp<365, 1, 0)]
  
  election_criteria_disp_gaba<-selection_criteria_disp_gaba[is.na(not_y1_lb_or_y1_fup),not_y1_lb_or_y1_fup :=1]
}


load(paste0(dirpregnancyinput,"D3_pregnancy_final.RData"))

if (thisdatasource %in% DAP_with_pregnancy_only ) {
  days_window<-91
}else{   
  days_window<-365
}



tmp<-merge(selection_criteria_disp_gaba,unique(D3_pregnancy_final[,.(person_id,pregnancy_id, pregnancy_start_date, pregnancy_end_date )]),by="person_id",all.x = T)  
tmp[, gabadisp_notin_1yb_or_duringpreg := fifelse(!is.na(date) &
                                                    date<=pregnancy_end_date & date>=pregnancy_start_date-days_window , 0, 1)]

tmp[is.na(gabadisp_notin_1yb_or_duringpreg),gabadisp_notin_1yb_or_duringpreg:=1]
 
# tmp[,all_gabadisp_notin_1yb_or_duringpreg:=min(gabadisp_notin_1yb_or_duringpreg ,na.rm=T),by=c("pregnancy_id")]
#  
# tmp<-unique(tmp[,.(person_id,date,pregnancy_id,gabadisp_notin_1yb_or_duringpreg)])
 
tmp[,gabadisp_notin_1yb_or_duringpreg_per_person:=min(gabadisp_notin_1yb_or_duringpreg ,na.rm=T),by=c("person_id","date","medicinal_product_id")]
 
tmp<-unique(tmp[,.(person_id,date,medicinal_product_id,gabadisp_notin_1yb_or_duringpreg_per_person)])

selection_criteria_disp_gaba<-merge(selection_criteria_disp_gaba,tmp,all.x=T,by=c("person_id","date","medicinal_product_id"))

selection_criteria_disp_gaba[, gabadisp_notin_studyperiod := fifelse(date>=study_start & date<=study_end , 0, 1) ]

if (!(thisdatasource %in% DAP_with_pregnancy_only)){
selected_dispensing_gabapentin <- unique(CreateFlowChart(
  dataset = selection_criteria_disp_gaba,
  listcriteria = c("gabadisp_notin_studyperiod","spell_not_include_disp_gaba","not_y1_lb_or_y1_fup"
                   ),
  flowchartname = "Flowchart_exclusion_criteria_dispensing_gaba"))

fwrite(get("Flowchart_exclusion_criteria_dispensing_gaba"),
       paste0(diroutput, "Flowchart_exclusion_criteria_dispensing_gaba.csv"))

#save the dispensations included in a spell
smart_save(selected_dispensing_gabapentin[,.(person_id,date,codvar,indication_code,prescriber_speciality)], dirtemp, override_name = "selected_dispensing_gabapentin", extension = "rds")
fwrite(selected_dispensing_gabapentin, file=paste0(dirtemp,"selected_dispensing_gabapentin.csv"))

print(paste0(nrow(selected_dispensing_gabapentin) ," gabapentin dispensing were selected for the women of childbearing age cohort"))
}



selected_dispensing_gabapentin_pregnancy <- unique(CreateFlowChart(
  dataset = selection_criteria_disp_gaba,
  listcriteria = c("gabadisp_notin_studyperiod","spell_not_include_disp_gaba", "gabadisp_notin_1yb_or_duringpreg_per_person"),
  flowchartname = "Flowchart_exclusion_criteria_dispensing_gaba_preg"))

fwrite(get("Flowchart_exclusion_criteria_dispensing_gaba_preg"),
       paste0(diroutput, "Flowchart_exclusion_criteria_dispensing_gaba_preg.csv"))

selected_dispensing_gabapentin_pregnancy<-selected_dispensing_gabapentin_pregnancy[, .(person_id,medicinal_product_id,codvar,date , indication_code,prescriber_speciality)]

#save the dispensations included in a spell
smart_save(selected_dispensing_gabapentin_pregnancy[,.(person_id,date,codvar,indication_code,prescriber_speciality)], dirtemp, override_name = "selected_dispensing_gabapentin_pregnancy", extension = "rds")

fwrite(selected_dispensing_gabapentin_pregnancy, file=paste0(dirtemp,"selected_dispensing_gabapentin_pregnancy.csv"))

print(paste0(nrow(selected_dispensing_gabapentin_pregnancy) ," gabapentin dispensing were selected in the one year before or during a pregnancy"))



################ PREGABALIN
load(paste0(dirconceptsets,"PREGABALIN.RData"))

PREGABALIN<-unique(PREGABALIN[!is.na(date)][, .(person_id,medicinal_product_id,codvar,date ,    indication_code,prescriber_speciality)])


PREGABALIN<-PREGABALIN[!is.na(date),][,date2:=date]
setkeyv(PREGABALIN,c("person_id","date","date2"))


person_spells_tmp<-unique(dispensing_selection_base[!is.na(entry_spell_category),.(person_id,entry_spell_category,exit_spell_category,birth_date,death_date)])


setkeyv(person_spells_tmp,c("person_id","entry_spell_category","exit_spell_category"))

flag_spells_containing_disp_pregaba<-unique(better_foverlaps(person_spells_tmp,PREGABALIN))

flag_spells_containing_disp_pregaba<-flag_spells_containing_disp_pregaba[!is.na(date),]

flag_spells_containing_disp_pregaba<-unique(flag_spells_containing_disp_pregaba[,spell_not_include_disp_pregaba:=0][,date2:=NULL])

selection_criteria_disp_pregaba<-merge(PREGABALIN,unique(flag_spells_containing_disp_pregaba[,.(person_id,date,spell_not_include_disp_pregaba,entry_spell_category,exit_spell_category,death_date)]),all.x=T,by=c("person_id","date"))

selection_criteria_disp_pregaba<-selection_criteria_disp_pregaba[is.na(entry_spell_category),spell_not_include_disp_pregaba:=1]

# # Censor spells which start before the recommended start date
# selection_criteria_disp_pregaba<-selection_criteria_disp_pregaba[!is.na(entry_spell_category), entry_spell_category := pmax(entry_spell_category, recommended_start_date, na.rm = T)]
# 
# 
# selection_criteria_disp_pregaba[!is.na(exit_spell_category), exit_spell_category := pmin(exit_spell_category_crude, death_date, study_end_tmp, na.rm = T)]


if (!(thisdatasource %in% DAP_with_pregnancy_only )) {
  #distanza tra disp e entry e disp e exit
  selection_criteria_disp_pregaba[,dist_entry_disp:=difftime(date,entry_spell_category,unit="days")]
  selection_criteria_disp_pregaba[,dist_exit_disp:=difftime(exit_spell_category,date,unit="days")]
  
  selection_criteria_disp_pregaba<-selection_criteria_disp_pregaba[,not_y1_lb_or_y1_fup := fifelse(dist_entry_disp<365 | dist_exit_disp<365, 1, 0)]
}


load(paste0(dirpregnancyinput,"D3_pregnancy_final.RData"))


tmp<-merge(selection_criteria_disp_pregaba,unique(D3_pregnancy_final[,.(person_id,pregnancy_id, pregnancy_start_date, pregnancy_end_date )]),by="person_id",all.x = T)  
tmp[, pregabadisp_notin_1yb_or_duringpreg := fifelse(!is.na(date) &
                                                    date<=pregnancy_end_date & date>=pregnancy_start_date-days_window , 0, 1)]

tmp[is.na(pregabadisp_notin_1yb_or_duringpreg),pregabadisp_notin_1yb_or_duringpreg:=1]

# tmp[,all_pregabadisp_notin_1yb_or_duringpreg:=min(pregabadisp_notin_1yb_or_duringpreg ,na.rm=T),by=c("pregnancy_id")]
# 
# tmp<-unique(tmp[,.(person_id,date,pregnancy_id,all_pregabadisp_notin_1yb_or_duringpreg)])

tmp[,pregabadisp_notin_1yb_or_duringpreg_per_person:=min(pregabadisp_notin_1yb_or_duringpreg ,na.rm=T),by=c("person_id","date","medicinal_product_id")]

tmp<-unique(tmp[,.(person_id,date,medicinal_product_id,pregabadisp_notin_1yb_or_duringpreg_per_person)])

selection_criteria_disp_pregaba<-merge(selection_criteria_disp_pregaba,tmp,all.x=T,by=c("person_id","date","medicinal_product_id"))

selection_criteria_disp_pregaba[, pregabadisp_notin_studyperiod := fifelse(date>=study_start & date<=study_end , 0, 1) ]


if (!(thisdatasource %in% DAP_with_pregnancy_only)){
  selected_dispensing_pregabalin <- unique(CreateFlowChart(
    dataset = selection_criteria_disp_pregaba,
    listcriteria = c("pregabadisp_notin_studyperiod","spell_not_include_disp_pregaba","not_y1_lb_or_y1_fup"
    ),
    flowchartname = "Flowchart_exclusion_criteria_dispensing_pregaba"))
  
  fwrite(get("Flowchart_exclusion_criteria_dispensing_pregaba"),
         paste0(diroutput, "Flowchart_exclusion_criteria_dispensing_pregaba.csv"))
  
  #save the dispensations included in a spell
  smart_save(selected_dispensing_pregabalin[,.(person_id,date,codvar,indication_code,prescriber_speciality)], dirtemp, override_name = "selected_dispensing_pregabalin", extension = "rds")
  fwrite(selected_dispensing_pregabalin, file=paste0(dirtemp,"selected_dispensing_pregabalin.csv"))
  
  
  print(paste0(nrow(selected_dispensing_pregabalin) ," pregabalin dispensing were selected for the women of childbearing age cohort"))
}


selected_dispensing_pregabalin_pregnancy <- unique(CreateFlowChart(
  dataset = selection_criteria_disp_pregaba,
  listcriteria = c("pregabadisp_notin_studyperiod","spell_not_include_disp_pregaba", "pregabadisp_notin_1yb_or_duringpreg_per_person"),
  flowchartname = "Flowchart_exclusion_criteria_dispensing_pregaba_preg"))

fwrite(get("Flowchart_exclusion_criteria_dispensing_pregaba_preg"),
       paste0(diroutput, "Flowchart_exclusion_criteria_dispensing_pregaba_preg.csv"))

selected_dispensing_pregabalin_pregnancy<-selected_dispensing_pregabalin_pregnancy[, .(person_id,medicinal_product_id,codvar,date , indication_code,prescriber_speciality)]

#save the dispensations included in a spell
smart_save(selected_dispensing_pregabalin_pregnancy[,.(person_id,date,codvar,indication_code,prescriber_speciality)], dirtemp, override_name = "selected_dispensing_pregabalin_pregnancy", extension = "rds")
fwrite(selected_dispensing_pregabalin_pregnancy, file=paste0(dirtemp,"selected_dispensing_pregabalin_pregnancy.csv"))

print(paste0(nrow(selected_dispensing_pregabalin_pregnancy) ," pregabalin dispensing were selected in the one year before or during a pregnancy"))

