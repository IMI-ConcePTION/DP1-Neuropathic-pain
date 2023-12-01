
#ALGO M1: Main analyses for combining the components results 



if (!(thisdatasource %in% DAP_with_pregnancy_only ) ) {
        days_window<-91
}else{   
        days_window<-365
}

selected_dispensing_gaba<-smart_load("selected_dispensing_gabapentin", dirtemp, extension = "rds")
selected_dispensing_pregaba<-smart_load("selected_dispensing_pregabalin", dirtemp, extension = "rds")

selected_dispensing_any<-rbind(selected_dispensing_gaba,selected_dispensing_pregaba,fill=T)



smart_load("D4_study_population_gaba", diroutput, extension = "rds")
smart_load("D4_study_population_pregaba", diroutput, extension = "rds")
smart_load("D4_study_population_any", diroutput, extension = "rds")


ids<-unique(rbind(D4_study_population_gaba[,.(person_id)],D4_study_population_gaba[,.(person_id)],D4_study_population_pregaba[,.(person_id)] ))


files<-sub('\\.csv$', '', list.files(dirinput))
VISIT_OCCURRENCE<-data.table()
for (i in 1:length(files)) {
        if (str_detect(files[i],"^VISIT_"))  {
                visit_tmp<-unique(fread(paste0(dirinput,files[i],".csv"), colClasses = list( character="person_id"))[,.(person_id,visit_occurrence_id,meaning_of_visit)])
                VISIT_OCCURRENCE<-rbind(VISIT_OCCURRENCE,visit_tmp)
                rm(visit_tmp)
        }
}


# # Create dataset with meaning to recode
# #temp_meanings_event <- meanings_of_this_study_dap[["meaning_of_event"]]
# meanings_event <- lapply(names(meanings_of_this_study),
#                          function(x) data.table(meaning_of_event = meanings_of_this_study[[x]], new = x))
# meanings_event <- data.table::rbindlist(meanings_event)
# if (nrow(meanings_event) == 0) {
#         meanings_event <- data.table::data.table(meaning_of_event = character(), new = character())
# }
# 
# meanings_visit <- lapply(names(meanings_of_this_study),
#                          function(x) data.table(meaning_of_visit = meanings_of_this_study[[x]], new = x))
# meanings_visit <- data.table::rbindlist(meanings_visit)
# if (nrow(meanings_visit) == 0) {
#         meanings_visit <- data.table::data.table(meaning_of_visit = character(), new = character())
# }
# 
# 
# setnames(meanings_event,"meaning_of_event","meaning_renamed")


for (algo in c("M1","S1","S2","S3","S4","S5","S6")) {
        print(paste0("Detecting components for algorithm ",algo))
        
        if (algo=="S1") {
                        events_of_interest<-c("EPILEPSY","GAD","NEUROPATHIC_PAIN","OTHER_USES","POTENTIAL_MISE_ABUSE")
        }else{
                events_of_interest<-c("EPILEPSY","GAD","NEUROPATHIC_PAIN")
        }
        
        if (algo=="S2") {
                cond<-cond_S2
        }else if (algo!="S4" & algo!="S2" ){
                cond<-cond_M1
        }else if(algo=="S4"){
                cond<-cond_S4
                
        }
        
        if (algo=="M1" | algo=="S1" | algo=="S2" | algo=="S5" | algo=="S6" ) {
                allowed_window1<-365
                allowed_window2<-365
        }else if (algo=="S3"){
                allowed_window1<-365*2
                allowed_window2<-365*2
        }
        
        
        # meaning_occurences_all<-data.table()
        
        for (event in events_of_interest) {
                if (algo=="S5" & event=="NEUROPATHIC_PAIN") {
                        load(paste0(dirconceptsets,event, ".RData"))
                        load(paste0(dirconceptsets,event, "_S5.RData"))
                        assign(event,rbind(get(paste0(event, "_S5")), get(paste0(event)),fill=T ))
                }else if (algo=="S6" & event=="NEUROPATHIC_PAIN"){
                        load(paste0(dirconceptsets,event, ".RData"))
                        load(paste0(dirconceptsets,event, "_S6.RData"))
                        assign(event,rbind(get(paste0(event, "_S6")), get(paste0(event)),fill=T ))
                }else {
                        load(paste0(dirconceptsets,event, ".RData"))
                }
                
                setnames(get(event),c("date"),c("date_diagn"))
                
                assign(event,merge(get(event)[person_id %in% ids$person_id,],unique(VISIT_OCCURRENCE[person_id %in% ids$person_id,.(visit_occurrence_id,meaning_of_visit)]),all.x=T,by="visit_occurrence_id"))
                
                # if(algo=="M1"){
                #         get(event)[meanings_event, on = "meaning_renamed", meaning_of_event_recoded := i.new]
                #         get(event)[meanings_visit, on = "meaning_of_visit", meaning_of_visit_recoded := i.new]
                #         
                #         
                #         get(event)[is.na(meaning_of_event_recoded), meaning_of_event_recoded := "UNSPECIFIED"]
                #         get(event)[is.na(meaning_of_visit_recoded), meaning_of_visit_recoded := "UNSPECIFIED"]
                #         
                #         get(event)[,concept:=event]
                #         # Count meaning occurences and save it in direxp
                #         meaning_occurences <- MergeFilterAndCollapse(list( get(event)),
                #                                                      condition = "!is.na(person_id)",
                #                                                      strata = c( "concept","meaning_renamed", "meaning_of_event_recoded",
                #                                                                  "meaning_of_visit", "meaning_of_visit_recoded"),
                #                                                      summarystat = list(c("count", "person_id", "count")))
                #         
                #         
                #         
                #         #get(event)[, c("meaning_renamed", "meaning_of_visit", "visit_occurrence_id") := NULL]
                #         
                #         get(event)[meaning_of_event_recoded == "UNSPECIFIED", meaning_of_event_recoded := meaning_of_visit_recoded]
                #         get(event)[meaning_of_visit_recoded == "UNSPECIFIED", meaning_of_visit_recoded := meaning_of_event_recoded]
                #         
                #         if (nrow( get(event)[meaning_of_visit_recoded != meaning_of_event_recoded, ]) > 0) {
                #                 stop(paste("inconsistent meaning_of_event and meaning_of_visit for at least one person"))
                #         }
                #         
                #         meaning_occurences_all<-rbind(meaning_occurences_all,meaning_occurences)
                #         get(event)[, meaning_renamed := meaning_of_event_recoded]
                #         get(event)[, c("meaning_of_event_recoded", "meaning_of_visit_recoded") := NULL]
                # }
                
        }
        
        
        
        # if(algo=="M1") fwrite(meaning_occurences_all,file=paste0(direxp,"D5_meaning_occurences.csv"))
        
        
        
        
        
        
        #for gabapentin pregabalin and both of them look for the components
        for (dr in c("gaba","pregaba","any")) {
                
                assign(paste0("selected_dispensing_",dr),get(paste0("selected_dispensing_",dr))[person_id %in% get(paste0("D4_study_population_",dr))$person_id,])
                
                
                assign(paste0("D3_indication_SAP1_",dr), MergeFilterAndCollapse(list(get(paste0("selected_dispensing_",dr))),get(paste0("D4_study_population_",dr)),
                                                                                
                                                                                key=c("person_id"),
                                                                                cond="date>=study_start & date<=study_end",
                                                                                strata=c("person_id"),
                                                                                sorting = c("person_id","date"),
                                                                                summarystat = list(list(c("first"),"date", "first_disp"))) )
                
                
                assign(paste0("D3_indication_SAP1_",dr),merge(get(paste0("selected_dispensing_",dr)), get(paste0("D3_indication_SAP1_",dr)) , all.x = T,by="person_id",allow.cartesian = T))
                
                
                #COMPONENT B---------------------
                
                if (thisdatasource=="UOSL") {
                        print("Computing component B")
                        # epilepsy
                        pattern_base <- paste0("^", c("G40","G41"))
                        pattern_no_dot <- paste(gsub("\\.", "", pattern_base), collapse = "|")
                        pattern <- gsub("\\*", ".", pattern_no_dot)
                        get(paste0("D3_indication_SAP1_pregnant_woman_",dr))[stringr::str_detect(indication_code, pattern)  & indication_code_vocabulary=="ICD10",component_B:="epilepsy"]
                        
                        
                        # pattern_base <- paste0("^", c("N88","N07"))
                        # pattern_no_dot <- paste(gsub("\\.", "", pattern_base), collapse = "|")
                        # pattern <- gsub("\\*", ".", pattern_no_dot)
                        # get(paste0("D3_indication_SAP1_pregnant_woman_",dr))[stringr::str_detect(indication_code, pattern)  & indication_code_vocabulary=="ICPC2",component_B:="epilepsy"]
                        
                        
                        # NP
                        pattern_base <- paste0("^", c("R52","G50","G51","G52","G53","G54","G55","G56","G57","G58","G59","G60","G61","G62","G63","G64","G95.85","E10.4","E11.4"))
                        pattern_no_dot <- paste(gsub("\\.", "", pattern_base), collapse = "|")
                        pattern <- gsub("\\*", ".", pattern_no_dot)
                        get(paste0("D3_indication_SAP1_pregnant_woman_",dr))[stringr::str_detect(indication_code, pattern)  & indication_code_vocabulary=="ICD10",component_B:="neurophatic_pain"]
                        
                        
                        # pattern_base <- paste0("^", c("A01","L86"))
                        # pattern_no_dot <- paste(gsub("\\.", "", pattern_base), collapse = "|")
                        # pattern <- gsub("\\*", ".", pattern_no_dot)
                        # get(paste0("D3_indication_SAP1_pregnant_woman_",dr))[stringr::str_detect(indication_code, pattern)  & indication_code_vocabulary=="ICPC2",component_B:="neurophatic_pain"]
                        
                        
                        # GAD
                        pattern_base <- paste0("^", c("F40","F41","F42","F43"))
                        pattern_no_dot <- paste(gsub("\\.", "", pattern_base), collapse = "|")
                        pattern <- gsub("\\*", ".", pattern_no_dot)
                        get(paste0("D3_indication_SAP1_pregnant_woman_",dr))[stringr::str_detect(indication_code, pattern)  & indication_code_vocabulary=="ICD10",component_B:="anxiety"]
                        
                        
                        # pattern_base <- paste0("^", c("P01","P02","P74"))
                        # pattern_no_dot <- paste(gsub("\\.", "", pattern_base), collapse = "|")
                        # pattern <- gsub("\\*", ".", pattern_no_dot)
                        # get(paste0("D3_indication_SAP1_pregnant_woman_",dr))[stringr::str_detect(indication_code, pattern)  & indication_code_vocabulary=="ICPC2",component_B:="anxiety"]
                        
                }
                
                
                #COMPONENT C---------------------
                
                if (thisdatasource=="UOSL") {
                        print("Computing component C")
                        get(paste0("D3_indication_SAP1_",dr))[prescriber_speciality==41 |prescriber_speciality==2 | prescriber_speciality==84 |prescriber_speciality==152 | prescriber_speciality==192 , component_C:="NEUROPATHIC_PAIN"]
                        
                        get(paste0("D3_indication_SAP1_",dr))[prescriber_speciality==58 , component_C:="GAD"]
                }
                
                

                
                #COMPONENT D---------------------
                #start including diagnosis codes
                
                
                if (thisdatasource=="THL" | thisdatasource=="SAIL Databank") {
                        
                        for (event in events_of_interest) {
                                print(paste0("Computing component D for ",event))       
                                
                                assign(paste0(event,"_exist"), MergeFilterAndCollapse(list(get(event)[,.(person_id,date_diagn,meaning_of_visit,meaning_renamed)]),
                                                                                      get(paste0("D3_indication_SAP1_",dr)),
                                                                                      key=c("person_id"),
                                                                                      condition =cond[["D_primarycare"]],
                                                                                      
                                                                                      strata=c("person_id","date"),
                                                                                      sorting = c("person_id","date_diagn"),
                                                                                      summarystat = list(list(c("exist"),"date_diagn", "event_present"))) )
                                
                                
                                assign(paste0(event,"_exist"),get(paste0(event,"_exist"))[,event_present:=as.character(event_present)][,event_present:=event])
                        }
                        
                        if (algo=="S1") { 
                                diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist,POTENTIAL_MISE_ABUSE_exist)
                                diagnosis_exist<-rbind(diagnosis_exist,OTHER_USES_exist)
                        }else{
                                diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist)
                        }
                        
 
                        
                        setnames(diagnosis_exist,"event_present","component_D")
                        
                        diagnosis_exist[,count_rows_perid:=.N,by=c("person_id","date")]
                        diagnosis_exist<-diagnosis_exist[ ,component_Ddetailed:=list(list(unique(component_D))),by=c("person_id","date")]
                        
                        diagnosis_exist[count_rows_perid>1,component_D:="more than one"]
                        diagnosis_exist<-unique(diagnosis_exist[,count_rows_perid:=NULL],by=c("person_id","date","component_D"))
                        
                        assign(paste0("D3_indication_SAP1_",dr),merge(get(paste0("D3_indication_SAP1_",dr)),diagnosis_exist,all.x=T,by.x=c("person_id","date"),by.y=c("person_id","date")))
                        
                }
                
                
                
                #COMPONENT E ------------------
                
                #all DAPs
                
                
                for (event in events_of_interest) {
                        print(paste0("Computing component E for ",event)) 
                        
                        assign(paste0(event,"_exist"), MergeFilterAndCollapse(list(get(event)[,.(person_id,date_diagn,meaning_of_visit,meaning_renamed)]),
                                                                              get(paste0("D3_indication_SAP1_",dr)),
                                                                              key=c("person_id"),
                                                                              condition =cond[["E_inpatient"]],
                                                                              
                                                                              strata=c("person_id","date"),
                                                                              sorting = c("person_id","date_diagn"),
                                                                              summarystat = list(list(c("exist"),"date_diagn", "event_present"))) ) 
                        
                        
                        assign(paste0(event,"_exist"),get(paste0(event,"_exist"))[,event_present:=as.character(event_present)][,event_present:=event])
                }
                
                if (algo=="S1") {
                        diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist,POTENTIAL_MISE_ABUSE_exist)
                        diagnosis_exist<-rbind(diagnosis_exist,OTHER_USES_exist)
                }else{
                        diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist)
                }
                

                
                setnames(diagnosis_exist,"event_present","component_E")
                
                diagnosis_exist[,count_rows_perid:=.N,by=c("person_id","date")]
                diagnosis_exist<-diagnosis_exist[ ,component_Edetailed:=list(list(unique(component_E))),by=c("person_id","date")]
                
                diagnosis_exist[count_rows_perid>1,component_E:="more than one"]
                diagnosis_exist<-unique(diagnosis_exist[,count_rows_perid:=NULL],by=c("person_id","date","component_E"))
                
                
                assign(paste0("D3_indication_SAP1_",dr),merge(get(paste0("D3_indication_SAP1_",dr)),diagnosis_exist,all.x=T,by.x=c("person_id","date"),by.y=c("person_id","date")))
                
                
                
                
                #COMPONENT F ------------------
                if (thisdatasource!="EFEMERIS" & thisdatasource!="FISABIO") {
                        
                        # | meaning_renamed == 'reason_for_specialist_encounter' | meaning_renamed == 'specialist_diagnosis' 
                        
                        for (event in events_of_interest) {
                                print(paste0("Computing component F for ",event)) 
                                
                                assign(paste0(event,"_exist"), MergeFilterAndCollapse(list(get(event)[,.(person_id,date_diagn,meaning_of_visit,meaning_renamed)]),
                                                                                      get(paste0("D3_indication_SAP1_",dr)),
                                                                                      key=c("person_id"),
                                                                                      condition =cond[["F_outpatient"]],
                                                                                      
                                                                                      strata=c("person_id","date"),
                                                                                      sorting = c("person_id","date_diagn"),
                                                                                      summarystat = list(list(c("exist"),"date_diagn", "event_present"))) )
                                
                                
                                assign(paste0(event,"_exist"),get(paste0(event,"_exist"))[,event_present:=as.character(event_present)][,event_present:=event])
                        }
                        
                        if (algo=="S1") {
                                diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist,POTENTIAL_MISE_ABUSE_exist)
                                diagnosis_exist<-rbind(diagnosis_exist,OTHER_USES_exist)
                        }else{
                                diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist)
                        }
                        
                        setnames(diagnosis_exist,"event_present","component_F")
                        
                        diagnosis_exist[,count_rows_perid:=.N,by=c("person_id","date")]
                        diagnosis_exist<-diagnosis_exist[ ,component_Fdetailed:=list(list(unique(component_F))),by=c("person_id","date")]
                        
                        diagnosis_exist[count_rows_perid>1,component_F:="more than one"]
                        diagnosis_exist<-unique(diagnosis_exist[,count_rows_perid:=NULL],by=c("person_id","date","component_F"))
                        
                        assign(paste0("D3_indication_SAP1_",dr),merge(get(paste0("D3_indication_SAP1_",dr)),diagnosis_exist,all.x=T,by.x=c("person_id","date"),by.y=c("person_id","date")))
                }
                
                
                #COMPONENT G ------------------
                if (thisdatasource=="THL"  | thisdatasource=="ARS"  | thisdatasource=="SAIL Databank" | thisdatasource=="FISABIO" ) {
                        
                        for (event in events_of_interest) {
                                print(paste0("Computing component G for ",event)) 
                                
                                assign(paste0(event,"_exist"), MergeFilterAndCollapse(list(get(event)[,.(person_id,date_diagn,meaning_of_visit,meaning_renamed)]),
                                                                                      get(paste0("D3_indication_SAP1_",dr)),
                                                                                      key=c("person_id"),
                                                                                      condition =cond[["G_emergency"]],
                                                                                      
                                                                                      strata=c("person_id","date"),
                                                                                      sorting = c("person_id","date_diagn"),
                                                                                      summarystat = list(list(c("exist"),"date_diagn", "event_present"))) )
                                
                                
                                assign(paste0(event,"_exist"),get(paste0(event,"_exist"))[,event_present:=as.character(event_present)][,event_present:=event])
                        }
                        
                        if (algo=="S1") {
                                diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist,POTENTIAL_MISE_ABUSE_exist)
                                diagnosis_exist<-rbind(diagnosis_exist,OTHER_USES_exist)
                        }else{
                                diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist)
                        }
                        

                        setnames(diagnosis_exist,"event_present","component_G")
                        
                        diagnosis_exist[,count_rows_perid:=.N,by=c("person_id","date")]
                        diagnosis_exist<-diagnosis_exist[ ,component_Gdetailed:=list(list(unique(component_G))),by=c("person_id","date")]
                        
                        diagnosis_exist[count_rows_perid>1,component_G:="more than one"]
                        diagnosis_exist<-unique(diagnosis_exist[,count_rows_perid:=NULL],by=c("person_id","date","component_G"))
                        
                        assign(paste0("D3_indication_SAP1_",dr),merge(get(paste0("D3_indication_SAP1_",dr)),diagnosis_exist,all.x=T,by.x=c("person_id","date"),by.y=c("person_id","date")))
                }
                
                
                
                
                #COMPONENT Fbis------------------
                if (thisdatasource=="THL" | thisdatasource=="ARS" | thisdatasource=="FERR" ) {
                        
                        for (event in events_of_interest) {
                                
                                print(paste0("Computing component Fbis for ",event)) 
                                
                                assign(paste0(event,"_exist"), MergeFilterAndCollapse(list(get(event)[,.(person_id,date_diagn,meaning_of_visit,meaning_renamed)]),
                                                                                      get(paste0("D3_indication_SAP1_",dr)),
                                                                                      key=c("person_id"),
                                                                                      condition =cond[["Fbis_hosp"]],
                                                                                      
                                                                                      strata=c("person_id","date"),
                                                                                      sorting = c("person_id","date_diagn"),
                                                                                      summarystat = list(list(c("exist"),"date_diagn", "event_present"))) )
                                
                                
                                assign(paste0(event,"_exist"),get(paste0(event,"_exist"))[,event_present:=as.character(event_present)][,event_present:=event])
                                
                        }
                        
                        if (algo=="S1") {
                                diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist,POTENTIAL_MISE_ABUSE_exist)
                                diagnosis_exist<-rbind(diagnosis_exist,OTHER_USES_exist)
                        }else{
                                diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist)
                        }
                        
  
                        setnames(diagnosis_exist,"event_present","component_Fbis")
                        
                        diagnosis_exist[,count_rows_perid:=.N,by=c("person_id","date")]
                        diagnosis_exist<-diagnosis_exist[ ,component_Fbisdetailed:=list(list(unique(component_Fbis))),by=c("person_id","date")]
                        
                        diagnosis_exist[count_rows_perid>1,component_Fbis:="more than one"]
                        diagnosis_exist<-unique(diagnosis_exist[,count_rows_perid:=NULL],by=c("person_id","date","component_Fbis"))
                        
                        assign(paste0("D3_indication_SAP1_",dr),merge(get(paste0("D3_indication_SAP1_",dr)),diagnosis_exist,all.x=T,by.x=c("person_id","date"),by.y=c("person_id","date")))
                }
                
                
                
                
                
                #COMPONENT H------------------
                if (thisdatasource=="ARS" | thisdatasource=="SNDS" | thisdatasource=="FERR") {
                        
                        
                        for (event in events_of_interest) {
                                print(paste0("Computing component H for ",event)) 
                                
                                assign(paste0(event,"_exist"), MergeFilterAndCollapse(list(get(event)[,.(person_id,date_diagn,meaning_of_visit,meaning_renamed)]),
                                                                                      get(paste0("D3_indication_SAP1_",dr)),
                                                                                      key=c("person_id"),
                                                                                      condition =cond[["H_exemptions"]],
                                                                                      
                                                                                      strata=c("person_id","date"),
                                                                                      sorting = c("person_id","date_diagn"),
                                                                                      summarystat = list(list(c("exist"),"date_diagn", "event_present"))) )
                                
                                
                                assign(paste0(event,"_exist"),get(paste0(event,"_exist"))[,event_present:=as.character(event_present)][,event_present:=event])
                        }
                        
                        if (algo=="S1") {
                                diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist,POTENTIAL_MISE_ABUSE_exist)
                                diagnosis_exist<-rbind(diagnosis_exist,OTHER_USES_exist)
                        }else{
                                diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist)
                        }
                        

                        setnames(diagnosis_exist,"event_present","component_H")
                        
                        diagnosis_exist[,count_rows_perid:=.N,by=c("person_id","date")]
                        diagnosis_exist<-diagnosis_exist[ ,component_Hdetailed:=list(list(unique(component_H))),by=c("person_id","date")]
                        
                        diagnosis_exist[count_rows_perid>1,component_H:="more than one"]
                        diagnosis_exist<-unique(diagnosis_exist[,count_rows_perid:=NULL],by=c("person_id","date","component_H"))
                        
                        assign(paste0("D3_indication_SAP1_",dr),merge(get(paste0("D3_indication_SAP1_",dr)),diagnosis_exist,all.x=T,by.x=c("person_id","date"),by.y=c("person_id","date")))
                        
                        rm("diagnosis_exist")
                }
                
                
                
                #COMPONENT I------------------
                
                # if(algo=="S2") {
                #         cond_drugs<-"date_disp>=date-allowed_window1 & date_disp<=date & !is.na(date_disp)"
                # }else{
                #         cond_drugs<-"date_disp>=date-allowed_window1 & date_disp<=date+allowed_window2 & !is.na(date_disp)"
                # }
                
                cond_drugs<-"date_disp>=date-365 & date_disp<=date+365 & !is.na(date_disp)"
                
                for (event in c("EPILEPSY","GAD","NEUROPATHIC_PAIN")) {
                        print(paste0("Computing component I for ",event)) 
                        
                        event_complete<-paste0(event,"_DRUGS")
                        #load(paste0(dirconceptsets,event_complete, ".RData"))
                        
                        if (algo=="S5" & event=="NEUROPATHIC_PAIN") {
                                load(paste0(dirconceptsets,event_complete, ".RData"))
                                load(paste0(dirconceptsets,event_complete, "_S5.RData"))
                                assign(event_complete,rbind(get(paste0(event_complete, "_S5")), get(paste0(event_complete)),fill=T ))
                                
                        }else if (algo=="S6" & event_complete=="NEUROPATHIC_PAIN"){
                                load(paste0(dirconceptsets,event_complete, ".RData"))
                                load(paste0(dirconceptsets,event_complete, "_S6.RData"))
                                assign(event_complete,rbind(get(paste0(event_complete, "_S6")), get(paste0(event_complete)),fill=T ))
                        }else {
                                load(paste0(dirconceptsets,event_complete, ".RData"))
                                
                        }
                        
                        setnames(get(event_complete),c("date"),c("date_disp"))
                        
                        
                        assign(paste0(event_complete,"_exist"), MergeFilterAndCollapse(list(unique(get(event_complete)[,.(person_id,date_disp)])),
                                                                                       get(paste0("D3_indication_SAP1_",dr)),
                                                                                       key=c("person_id"),
                                                                                       condition =cond_drugs,
                                                                                       strata=c("person_id","date"),
                                                                                       sorting = c("person_id","date_disp"),
                                                                                       summarystat = list(list(c("exist"),"date_disp", "event_present"))) )
                        
                        
                        assign(paste0(event_complete,"_exist"),get(paste0(event_complete,"_exist"))[,event_present:=as.character(event_present)][,event_present:=event])
                        rm(event_complete)
                }
                
                drugs_exist<-rbind(EPILEPSY_DRUGS_exist,NEUROPATHIC_PAIN_DRUGS_exist,GAD_DRUGS_exist)
                rm("EPILEPSY_DRUGS_exist")
                rm("NEUROPATHIC_PAIN_DRUGS_exist")
                rm("GAD_DRUGS_exist")
                setnames(drugs_exist,"event_present","component_I")
                
                drugs_exist[,count_rows_perid:=.N,by=c("person_id","date")]
                drugs_exist<-drugs_exist[ ,component_Idetailed:=list(list(unique(component_I))),by=c("person_id","date")]
                drugs_exist[count_rows_perid>1,component_I:="more than one"]
                drugs_exist<-unique(drugs_exist[,count_rows_perid:=NULL],by=c("person_id","date","component_I"))
                
                assign(paste0("D3_indication_SAP1_",dr),merge(get(paste0("D3_indication_SAP1_",dr)),drugs_exist,all.x=T,by.x=c("person_id","date"),by.y=c("person_id","date")))
                
                rm("drugs_exist")
        }
        #start from all the dispensing of gabapentin and from the study population 
        

        ######################################
        #shell tables 
        
        assign(paste0("D4_proportion_per_component_",algo) ,data.table())
        
        for (dr in c("gaba","pregaba","any")) {
                shell_base<-get(paste0("D3_indication_SAP1_",dr))[, .SD, .SDcols = patterns("^comp|person_id|first|date")]
                #shell_base[,date2:=NULL][,death_date:=NULL]
                
                shell_base<-unique(shell_base,by=colnames(shell_base[,!grep("detailed", colnames(shell_base)),with=F] ))
 
                smart_save(shell_base, dirtemp, override_name = paste0("D3_components_",algo,"_",dr), extension = "rds")
                shell_base_csv<-copy(shell_base)
                shell_base_csv <- shell_base_csv[, lapply(.SD, function(x) {
                        x[lengths(x) == 0] <- NA
                        return(x)
                })]
                
                fwrite(shell_base_csv, file=paste0(dirtemp,"D3_components_",algo,"_",dr,".csv"))
                
                #for each component we have to go from one row per dispensing to one per pregnancy
                columns_to_exclude<-colnames(shell_base)[(grep("detailed$", colnames(shell_base), perl=TRUE))]
                columns_to_include<-colnames(shell_base)[!(colnames(shell_base) %in% columns_to_exclude)]
                shell_base<-shell_base[,..columns_to_include]
                
                #for each component we have to go from one row per dispensing to one per person id
                for (comp in c("B","C","D","E","F","Fbis","I" ,"G","H")) {
                        name<-paste0("component_",comp)
                        
                        if (name %in% colnames(shell_base)) {
                                assign(paste0("shell_base_",comp),shell_base[,c("person_id",name),with=F])
                                
                                assign(paste0("shell_base_",comp),get((paste0("shell_base_",comp)))[, paste0("count_rows_",comp) := uniqueN(get(paste0("component_",comp)),na.rm=T), by=c("person_id")])
                                
                                assign(paste0("shell_base_",comp),unique(get((paste0("shell_base_",comp)))[!(is.na(get(name)) & get(paste0("count_rows_",comp))==1),]))
                                
                                suppressWarnings(assign(paste0("shell_base_",comp),get((paste0("shell_base_",comp)))[get(paste0("count_rows_",comp))>1,(name):="more than one"]))
                                
                                suppressWarnings(assign(paste0("shell_base_",comp),unique(get((paste0("shell_base_",comp)))[is.na(get(name)),(name):="none"])))
                                
                                assign(paste0("table_",comp,"_",dr),get((paste0("shell_base_",comp)))[,denominator:=nrow(get((paste0("shell_base_",comp))))][, .(numerator = .N), by = c(name,"denominator")])
                                
                                setnames(get(paste0("table_",comp,"_",dr)),name,"indication")
                                
                                if (dr=="gaba") medication_name<-"gabapentin"
                                if (dr=="pregaba") medication_name<-"pregabalin"
                                if (dr=="any") medication_name<-"any gabapentinoids"
                                
                                get(paste0("table_",comp,"_",dr))[,medication:=medication_name][,"component":=comp][,algorithm:=algo]
                                assign(paste0("D4_proportion_per_component_",algo),rbind(get(paste0("D4_proportion_per_component_",algo)),get(paste0("table_",comp,"_",dr)),fill=T))
                        }
                }
                
                setcolorder(get(paste0("D4_proportion_per_component_",algo)), c("algorithm","component", "medication", "indication","numerator","denominator"))
                
                get(paste0("D4_proportion_per_component_",algo))[indication=="GAD",indication:="anxiety"][,indication:=tolower(indication)]
                get(paste0("D4_proportion_per_component_",algo))[,"datasource":=thisdatasource]
        }
        
        
        fwrite(get(paste0("D4_proportion_per_component_",algo)),file=paste0(direxp,"D4_proportion_per_component_",algo,".csv"))
        
        
        assigned_levels <- vector(mode="list")
        # assigned_levels[["Number_diagnosis"]] <- c("Number_diagnostic_codes_detected", "Number_diagnostic_codes_detected2")
        assigned_levels[["Algorithm"]] <- c("algorithm")
        assigned_levels[["Component"]] <- c("component")
        assigned_levels[["Medication"]] <- c("medication")
        assigned_levels[["Indication"]] <- c("indication")
        
        
        
        assign(paste0("D4_proportion_per_component_",algo), Cube(input = get(paste0("D4_proportion_per_component_",algo)),
                                                                 dimensions = c("Algorithm","Component","Medication","Indication"),
                                                                 levels = assigned_levels,
                                                                 measures = c("numerator","denominator"))   )
        
        get(paste0("D4_proportion_per_component_",algo))[Medication_LabelValue!="any gabapentinoids",Medication_LevelOrder:=1]
        
        
        # Find if a level contains at least a value to censor
        summary_threshold <- 5
        tmp <- copy(get(paste0("D4_proportion_per_component_",algo)))
        
        #
        for(measure in c("numerator_sum","denominator_sum")) {
                tmp[, (measure) := fifelse(get(measure) < summary_threshold & get(measure) > 0, F, T)] 
        }
        
        tmp <- tmp[, lapply(.SD, all), by = c("Component_LabelValue","Component_LevelOrder","Algorithm_LevelOrder", "Medication_LevelOrder","Indication_LevelOrder"),
                   .SDcols = c("numerator_sum","denominator_sum")] 
        
        smart_save(tmp, dircheck, override_name = paste("D4_proportion_per_component",algo, "summary_levels", sep = "_"), extension = "csv")
}






