# we need to create two groups of meanings: one referring to hospitals HOSP (excluding emergency care) and one referring to primary care PC

meanings_of_this_study<-vector(mode="list")
meanings_of_this_study[["D_primarycare"]]=c("primary_care","gp_visit","primary_care_diagnosis","primary_care_event","gpe","birth_registry")

meanings_of_this_study[["E_inpatient"]]=c("hospital_unknown", "hospitalisation","hospitalisation_not_overnight", "pd","sd", "hospital_diagnosis","hp","hospital_encounter","radiation_hospitalised","perinatal_death_registry_mother","anomalies_mother_registry","death_registry")
meanings_of_this_study[["F_outpatient"]]=c("outpatient_hospital_visit", "outpatient_specialist_visit", "outpatient_for_intime_patient","op", "reason_for_specialist_encounter","specialist_diagnosis" ,"access_to_mental_health_service_primary","access_to_mental_health_service_comorbidity","outpaitent_contact","secondary_care_specialist","access_to_mental_health_service_primary baby and teenager")
meanings_of_this_study[["G_emergency"]]=c("emergency_contact", "emergency_room_diagnosis","hospitalisation_ICU_primary","hospitalisation_ICU_secondary", "ed","CRITICAL_CARE_STAY","hospitalisation_ICU_unspecified", "emergency_room_presentation")
meanings_of_this_study[["Fbis_hosp"]]=c("unknown","unknown_not_primarycare")
meanings_of_this_study[["H_exemptions"]]=c("history_of", "long_term_diagnosis", "exemption")


#
#

# # create two conditions on the meaning_of_event variable, associated to HOSP and to PC as listed above
# 
condmeaning <- list()
for (level1 in c("D_primarycare","E_inpatient","F_outpatient","G_emergency","Fbis_hosp","H_exemptions")) {
  for (meaning in meanings_of_this_study[[level1]]) {
    if (length(condmeaning[[level1]])==0) {condmeaning[[level1]]=paste0("meaning_renamed == '",meanings_of_this_study[[level1]][[1]],"'")

    }else{
      condmeaning[[level1]]=paste0(condmeaning[[level1]], " | meaning_renamed == '",meaning,"'")
    }
  }
}


for (level1 in c("D_primarycare","E_inpatient","F_outpatient","G_emergency","Fbis_hosp","H_exemptions")) {
  for (meaning in meanings_of_this_study[[level1]]) {
    condmeaning[[level1]]=paste0(condmeaning[[level1]], " | meaning_of_visit == '",meaning,"'")
    }
  }



cond_M1<-c()
for (level1 in c("D_primarycare","E_inpatient","F_outpatient","G_emergency","Fbis_hosp","H_exemptions")) {
  cond_M1[[level1]]<-paste0("date_diagn>=date-allowed_window1 & date_diagn<=date+allowed_window2 & !is.na(date_diagn) & (",condmeaning[[level1]],")")
}

cond_S2<-c()
for (level1 in c("D_primarycare","E_inpatient","F_outpatient","G_emergency","Fbis_hosp","H_exemptions")) {
  cond_S2[[level1]]<-paste0("date_diagn>=date-allowed_window1 & date_diagn<=date & !is.na(date_diagn) & (",condmeaning[[level1]],")")
}

cond_S4<-c()
for (level1 in c("D_primarycare","E_inpatient","F_outpatient","G_emergency","Fbis_hosp","H_exemptions")) {
  cond_S4[[level1]]<-paste0("date_diagn>=study_start - years(1) & date_diagn<=study_end & !is.na(date_diagn) & (",condmeaning[[level1]],")") 
}


#-------------------------------------
# fix for ICD10GM

for (conceptset in concept_sets_of_our_study){
  if (concept_set_domains[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study[[conceptset]][["ICD10GM"]] <- concept_set_codes_our_study[[conceptset]][["ICD10"]]
  }
}


# fix for ICD10ES
for (conceptset in concept_sets_of_our_study){
  if (concept_set_domains[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study[[conceptset]][["ICD10ES"]] <- concept_set_codes_our_study[[conceptset]][["ICD10"]]
  }
}

#-------------------------------------
# fix for ICD10CM
for (conceptset in concept_sets_of_our_study){
  if (concept_set_domains[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study[[conceptset]][["ICD10CM"]] <- concept_set_codes_our_study[[conceptset]][["ICD10"]]
  }
}

#-------------------------------------
# fix for ICD9CM
for (conceptset in concept_sets_of_our_study){
  if (concept_set_domains[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study[[conceptset]][["ICD9CM"]] <- concept_set_codes_our_study[[conceptset]][["ICD9"]]
  }
}

#-------------------------------------
# fix for ICPC2
for (conceptset in concept_sets_of_our_study){
  if (concept_set_domains[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study[[conceptset]][["ICPC2P"]] <- concept_set_codes_our_study[[conceptset]][["ICPC2"]]
  }
}



save(concept_set_codes_our_study,file=paste0(direxp,"concept_set_codes_our_study.RData"))
save(concept_set_codes_our_study,file=paste0(dirsmallcountsremoved,"concept_set_codes_our_study.RData"))



