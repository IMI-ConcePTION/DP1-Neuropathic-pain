

load(paste0(dirtemp, "D3_dispensing_during_observation_period_SAP1.RData"))

load(paste0(diroutput, "D4_study_population_gaba.RData"))


D3_indication_SAP1_pregnant_woman_gaba<-merge(D4_study_population_gaba[,.(person_id,pregnancy_id,preg)], D3_dispensing_during_observation_period_SAP1[codvar=="N03AX12",],all.x = T,by="person_id")


D3_indication_SAP1_pregnant_woman_gaba[, component_A:=NA]

D3_indication_SAP1_pregnant_woman_gaba[, component_B:=NA]

D3_indication_SAP1_pregnant_woman_gaba[prescriber_speciality==14 |prescriber_speciality==2 | prescriber_speciality==41 , component_C:="neuropathic pain"]

D3_indication_SAP1_pregnant_woman_gaba[prescriber_speciality==33 |prescriber_speciality==75 , component_C:="anxiety"]

D3_indication_SAP1_pregnant_woman_gaba[, component_D:=NA]

D3_indication_SAP1_pregnant_woman_gaba[, component_E:=NA]

D3_indication_SAP1_pregnant_woman_gaba[, component_F:=NA]


#Bbis
load(paste0(dirconceptsets, "EPILEPSY.RData"))
load(paste0(dirconceptsets, "NEUROPATHIC_PAIN.RData"))
load(paste0(dirconceptsets, "GAD.RData"))

setnames(EPILEPSY,c("date"),c("date_diagn"))
setnames(NEUROPATHIC_PAIN,c("date"),c("date_diagn"))
setnames(GAD,c("date"),c("date_diagn"))

epilepsy_exist = MergeFilterAndCollapse(list(EPILEPSY),
                                        D3_indication_SAP1_pregnant_woman_gaba,
                                        key=c("person_id"),
                                        condition ="date_diagn>=date-365 & date_diagn<=date+365 & !is.na(date_diagn)",
                                        
                                        strata=c("person_id","date_diagn"),
                                        summarystat = list(list(c("exist"),"codvar", "event_present")))

epilepsy_exist[event_present==1,event_present:="epilepsy"]

neuropathicpain_exist = MergeFilterAndCollapse(list(NEUROPATHIC_PAIN),
                                        D3_indication_SAP1_pregnant_woman_gaba,
                                        key=c("person_id"),
                                        condition ="date_diagn>=date-365 & date_diagn<=date+365 & !is.na(date_diagn)",
                                        
                                        strata=c("person_id","date_diagn"),
                                        summarystat = list(list(c("exist"),"codvar", "event_present")))

neuropathicpain_exist[event_present==1,event_present:="neuropathic pain"]



gad_exist = MergeFilterAndCollapse(list(GAD),
                                        D3_indication_SAP1_pregnant_woman_gaba,
                                        key=c("person_id"),
                                        condition ="date_diagn>=date-365 & date_diagn<=date+365 & !is.na(date_diagn)",
                                        
                                        strata=c("person_id","date_diagn"),
                                        summarystat = list(list(c("exist"),"codvar", "event_present")))

gad_exist[event_present==1,event_present:="anxiety"]

diagnosis_exist<-rbind(epilepsy_exist,neuropathicpain_exist,gad_exist)
setnames(diagnosis_exist,"event_present","component_Fbis")

diagnosis_exist[,count_rows_perid:=.N,by=c("person_id","date_diagn")]
diagnosis_exist[count_rows_perid>1,component_Fbis:="more than one"]

D3_indication_SAP1_pregnant_woman_gaba<-merge(D3_indication_SAP1_pregnant_woman_gaba,diagnosis_exist,all.x=T,by.x=c("person_id","date"),by.y=c("person_id","date_diagn") )


D3_indication_SAP1_pregnant_woman_gaba[is.na(component_Fbis),component_Fbis:="none"]


D3_indication_SAP1_pregnant_woman_gaba[, component_G:=NA]

D3_indication_SAP1_pregnant_woman_gaba[, component_H:=NA]


load(paste0(dirconceptsets, "EPILEPSY_DRUGS.RData"))
setnames(EPILEPSY_DRUGS,c("date"),c("date_disp"))

epilepsy_drugs_exist = MergeFilterAndCollapse(list(EPILEPSY_DRUGS),
                                        D3_indication_SAP1_pregnant_woman_gaba,
                                        key=c("person_id"),
                                        condition ="date_disp>=date-365 & date_disp<=date+365 & !is.na(date_disp)",
                                        
                                        strata=c("person_id","date_disp"),
                                        summarystat = list(list(c("exist"),"codvar", "event_present")))
epilepsy_drugs_exist[event_present==1,event_present:="epilepsy"]


load(paste0(dirconceptsets, "NEUROPATHIC_PAIN_DRUGS.RData"))
setnames(NEUROPATHIC_PAIN_DRUGS,c("date"),c("date_disp"))

neuropathicpain_drugs_exist = MergeFilterAndCollapse(list(NEUROPATHIC_PAIN_DRUGS),
                                              D3_indication_SAP1_pregnant_woman_gaba,
                                              key=c("person_id"),
                                              condition ="date_disp>=date-365 & date_disp<=date+365 & !is.na(date_disp)",
                                              
                                              strata=c("person_id","date_disp"),
                                              summarystat = list(list(c("exist"),"codvar", "event_present")))

neuropathicpain_drugs_exist[event_present==1,event_present:="neuropathic pain"]


load(paste0(dirconceptsets, "GAD_DRUGS.RData"))
setnames(GAD_DRUGS,c("date"),c("date_disp"))

gad_drugs_exist = MergeFilterAndCollapse(list(GAD_DRUGS),
                                                     D3_indication_SAP1_pregnant_woman_gaba,
                                                     key=c("person_id"),
                                                     condition ="date_disp>=date-365 & date_disp<=date+365 & !is.na(date_disp)",
                                                     
                                                     strata=c("person_id","date_disp"),
                                                     summarystat = list(list(c("exist"),"codvar", "event_present")))


gad_drugs_exist[event_present==1,event_present:="anxiety"]

drugs_exist<-rbind(epilepsy_drugs_exist,neuropathicpain_drugs_exist,gad_drugs_exist)
setnames(drugs_exist,"event_present","component_I")

drugs_exist[,count_rows_perid:=.N,by=c("person_id","date_diagn")]
drugs_exist[count_rows_perid>1,component_I:="more than one"]

D3_indication_SAP1_pregnant_woman_gaba<-merge(D3_indication_SAP1_pregnant_woman_gaba,drugs_exist,all.x=T,by.x=c("person_id","date"),by.y=c("person_id","date_disp") )


D3_indication_SAP1_pregnant_woman_gaba[is.na(component_I),component_I:="none"]



#pregabalin-----------------------------



load(paste0(dirtemp, "D3_dispensing_during_observation_period_SAP1.RData"))

load(paste0(diroutput, "D4_study_population_prega.RData"))


D3_indication_SAP1_pregnant_woman_prega<-merge(D4_study_population_prega[,.(person_id,pregnancy_id)], D3_dispensing_during_observation_period_SAP1[codvar=="N03AX16",],all.x = T,by="person_id")


D3_indication_SAP1_pregnant_woman_prega[, component_A:=NA]

D3_indication_SAP1_pregnant_woman_prega[, component_B:=NA]

D3_indication_SAP1_pregnant_woman_prega[prescriber_speciality==14 |prescriber_speciality==2 | prescriber_speciality==41 , component_C:="neuropathic pain"]

D3_indication_SAP1_pregnant_woman_prega[prescriber_speciality==33 |prescriber_speciality==75 , component_C:="anxiety"]

D3_indication_SAP1_pregnant_woman_prega[, component_D:=NA]

D3_indication_SAP1_pregnant_woman_prega[, component_E:=NA]

D3_indication_SAP1_pregnant_woman_prega[, component_F:=NA]


#Bbis
load(paste0(dirconceptsets, "EPILEPSY.RData"))
load(paste0(dirconceptsets, "NEUROPATHIC_PAIN.RData"))
load(paste0(dirconceptsets, "GAD.RData"))

setnames(EPILEPSY,c("date"),c("date_diagn"))
setnames(NEUROPATHIC_PAIN,c("date"),c("date_diagn"))
setnames(GAD,c("date"),c("date_diagn"))

epilepsy_exist = MergeFilterAndCollapse(list(EPILEPSY),
                                        D3_indication_SAP1_pregnant_woman_prega,
                                        key=c("person_id"),
                                        condition ="date_diagn>=date-365 & date_diagn<=date+365 & !is.na(date_diagn)",
                                        
                                        strata=c("person_id","date_diagn"),
                                        summarystat = list(list(c("exist"),"codvar", "event_present")))

epilepsy_exist[event_present==1,event_present:="epilepsy"]

neuropathicpain_exist = MergeFilterAndCollapse(list(NEUROPATHIC_PAIN),
                                               D3_indication_SAP1_pregnant_woman_prega,
                                               key=c("person_id"),
                                               condition ="date_diagn>=date-365 & date_diagn<=date+365 & !is.na(date_diagn)",
                                               
                                               strata=c("person_id","date_diagn"),
                                               summarystat = list(list(c("exist"),"codvar", "event_present")))

neuropathicpain_exist[event_present==1,event_present:="neuropathic pain"]



gad_exist = MergeFilterAndCollapse(list(GAD),
                                   D3_indication_SAP1_pregnant_woman_prega,
                                   key=c("person_id"),
                                   condition ="date_diagn>=date-365 & date_diagn<=date+365 & !is.na(date_diagn)",
                                   
                                   strata=c("person_id","date_diagn"),
                                   summarystat = list(list(c("exist"),"codvar", "event_present")))

gad_exist[event_present==1,event_present:="anxiety"]

diagnosis_exist<-rbind(epilepsy_exist,neuropathicpain_exist,gad_exist)
setnames(diagnosis_exist,"event_present","component_Fbis")

diagnosis_exist[,count_rows_perid:=.N,by=c("person_id","date_diagn")]
diagnosis_exist[count_rows_perid>1,component_Fbis:="more than one"]

D3_indication_SAP1_pregnant_woman_prega<-merge(D3_indication_SAP1_pregnant_woman_prega,diagnosis_exist,all.x=T,by.x=c("person_id","date"),by.y=c("person_id","date_diagn") )


D3_indication_SAP1_pregnant_woman_prega[is.na(component_Fbis),component_Fbis:="none"]


D3_indication_SAP1_pregnant_woman_prega[, component_G:=NA]

D3_indication_SAP1_pregnant_woman_prega[, component_H:=NA]


load(paste0(dirconceptsets, "EPILEPSY_DRUGS.RData"))
setnames(EPILEPSY_DRUGS,c("date"),c("date_disp"))

epilepsy_drugs_exist = MergeFilterAndCollapse(list(EPILEPSY_DRUGS),
                                              D3_indication_SAP1_pregnant_woman_prega,
                                              key=c("person_id"),
                                              condition ="date_disp>=date-365 & date_disp<=date+365 & !is.na(date_disp)",
                                              
                                              strata=c("person_id","date_disp"),
                                              summarystat = list(list(c("exist"),"codvar", "event_present")))
epilepsy_drugs_exist[event_present==1,event_present:="epilepsy"]


load(paste0(dirconceptsets, "NEUROPATHIC_PAIN_DRUGS.RData"))
setnames(NEUROPATHIC_PAIN_DRUGS,c("date"),c("date_disp"))

neuropathicpain_drugs_exist = MergeFilterAndCollapse(list(NEUROPATHIC_PAIN_DRUGS),
                                                     D3_indication_SAP1_pregnant_woman_prega,
                                                     key=c("person_id"),
                                                     condition ="date_disp>=date-365 & date_disp<=date+365 & !is.na(date_disp)",
                                                     
                                                     strata=c("person_id","date_disp"),
                                                     summarystat = list(list(c("exist"),"codvar", "event_present")))

neuropathicpain_drugs_exist[event_present==1,event_present:="neuropathic pain"]


load(paste0(dirconceptsets, "GAD_DRUGS.RData"))
setnames(GAD_DRUGS,c("date"),c("date_disp"))

gad_drugs_exist = MergeFilterAndCollapse(list(GAD_DRUGS),
                                         D3_indication_SAP1_pregnant_woman_prega,
                                         key=c("person_id"),
                                         condition ="date_disp>=date-365 & date_disp<=date+365 & !is.na(date_disp)",
                                         
                                         strata=c("person_id","date_disp"),
                                         summarystat = list(list(c("exist"),"codvar", "event_present")))


gad_drugs_exist[event_present==1,event_present:="anxiety"]

drugs_exist<-rbind(epilepsy_drugs_exist,neuropathicpain_drugs_exist,gad_drugs_exist)
setnames(drugs_exist,"event_present","component_I")

drugs_exist[,count_rows_perid:=.N,by=c("person_id","date_disp")]
drugs_exist[count_rows_perid>1,component_I:="more than one"]

D3_indication_SAP1_pregnant_woman_prega<-merge(D3_indication_SAP1_pregnant_woman_prega,drugs_exist,all.x=T,by.x=c("person_id","date"),by.y=c("person_id","date_disp") )


D3_indication_SAP1_pregnant_woman_prega[is.na(component_I),component_I:="none"]


#create timeframe

D3_indication_SAP1_pregnant_woman_gaba[year(date)>=2006 & year(date)<=2012,timeframe:="2006-2012"]
D3_indication_SAP1_pregnant_woman_gaba[year(date)>=2013 & year(date)<=2019,timeframe:="2013-2019"]

D3_indication_SAP1_pregnant_woman_gaba<-unique(D3_indication_SAP1_pregnant_woman_gaba[,.(person_id,pregnancy_id,codvar,timeframe,component_A,component_B,component_C,component_D,component_E,component_F,component_Fbis,component_I)])




