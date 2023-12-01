
#ALGO M1: Main analyses for combining the components results 

if (thisdatasource %in% DAP_with_pregnancy_only ) {
  days_window<-91
}else{   
  days_window<-365
}

selected_dispensing_gaba_pregnancy<-smart_load("selected_dispensing_gabapentin_pregnancy", dirtemp, extension = "rds")
selected_dispensing_pregaba_pregnancy<-smart_load("selected_dispensing_pregabalin_pregnancy", dirtemp, extension = "rds")

selected_dispensing_any_pregnancy<-rbind(selected_dispensing_gaba_pregnancy,selected_dispensing_pregaba_pregnancy,fill=T)



smart_load("D4_study_population_gaba_pregnancy", diroutput, extension = "rds")
smart_load("D4_study_population_pregaba_pregnancy", diroutput, extension = "rds")
smart_load("D4_study_population_any_pregnancy", diroutput, extension = "rds")


ids<-unique(rbind(D4_study_population_gaba_pregnancy[,.(person_id)],D4_study_population_pregaba_pregnancy[,.(person_id)],D4_study_population_any_pregnancy[,.(person_id)] ))


load(paste0(dirpregnancyinput, "D3_pregnancy_final.RData"))


files<-sub('\\.csv$', '', list.files(dirinput))
VISIT_OCCURRENCE<-data.table()
for (i in 1:length(files)) {
  if (str_detect(files[i],"^VISIT_OC"))  {
    visit_tmp<-unique(fread(paste0(dirinput,files[i],".csv"),colClasses = list( character="person_id"))[,.(person_id,visit_occurrence_id,meaning_of_visit)])
    VISIT_OCCURRENCE<-rbind(VISIT_OCCURRENCE,visit_tmp)
    rm(visit_tmp)
  }
}


for (algo in c("V3")) {
  print(paste0("Detecting components for algorithm ",algo))
  
  if (algo=="S1") {
    if (thisdatasource=="EFEMERIS" | thisdatasource=="SAIL Databank" | thisdatasource=="SNDS" | thisdatasource=="UOSL") {
      events_of_interest<-c("EPILEPSY","GAD","NEUROPATHIC_PAIN","OTHER_USES","POTENTIAL_MISE_ABUSE")
    }else{
      events_of_interest<-c("EPILEPSY","GAD","NEUROPATHIC_PAIN","POTENTIAL_MISE_ABUSE")
    }
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
  
  if (algo=="V3" ) {
    allowed_window1<-365
    allowed_window2<-365
  }else if (algo=="S3"){
    allowed_window1<-365*2
    allowed_window2<-365*2
  }
  
  
  
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
  }
  
  #for gabapentin pregabalin and both of them look for the components
  for (dr in c("gaba","pregaba","any")) {
    
    assign(paste0("selected_dispensing_",dr,"_pregnancy"),get(paste0("selected_dispensing_",dr,"_pregnancy"))[person_id %in% get(paste0("D4_study_population_",dr,"_pregnancy"))$person_id,])
    
    assign(paste0("D3_indication_SAP1_pregnant_woman_",dr),merge(get(paste0("selected_dispensing_",dr,"_pregnancy")), get(paste0("D4_study_population_",dr,"_pregnancy")) , all.y = T,by="person_id",allow.cartesian = T))
    
    assign(paste0("D3_indication_SAP1_pregnant_woman_",dr),merge(get(paste0("D3_indication_SAP1_pregnant_woman_",dr)),D3_pregnancy_final[,.(person_id,pregnancy_id,pregnancy_start_date,pregnancy_end_date )], all.x = T,by=c("person_id","pregnancy_id")))
    
    assign(paste0("D3_indication_SAP1_pregnant_woman_",dr),get(paste0("D3_indication_SAP1_pregnant_woman_",dr))[date<=pregnancy_end_date & date>=pregnancy_start_date-days_window , ])
    


    
    #COMPONENT E ------------------
    
    #all DAPs
    
    for (event in events_of_interest) {
      print(paste0("Computing component E for ",event)) 
      
      assign(paste0(event,"_exist"), MergeFilterAndCollapse(list(get(event)[,.(person_id,date_diagn,meaning_of_visit,meaning_renamed)]),
                                                            get(paste0("D3_indication_SAP1_pregnant_woman_",dr)),
                                                            key=c("person_id"),
                                                            condition =cond[["E_inpatient"]],
                                                            
                                                            strata=c("person_id","pregnancy_id","date"),
                                                            sorting = c("pregnancy_id","date_diagn"),
                                                            summarystat = list(list(c("exist"),"date_diagn", "event_present"))) ) 
      
      
      assign(paste0(event,"_exist"),get(paste0(event,"_exist"))[,event_present:=as.character(event_present)][,event_present:=event])
    }
    
    if (algo=="S1") {
      diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist,POTENTIAL_MISE_ABUSE_exist)
    }else{
      diagnosis_exist<-rbind(EPILEPSY_exist,NEUROPATHIC_PAIN_exist,GAD_exist)
    }
    
    if ((thisdatasource=="EFEMERIS" | thisdatasource=="SAIL Databank" | thisdatasource=="SNDS" | thisdatasource=="UOSL") & algo=="S1")  diagnosis_exist<-rbind(diagnosis_exist,OTHER_USES_exist)
    
    setnames(diagnosis_exist,"event_present","component_E")
    
    diagnosis_exist[,count_rows_perid:=.N,by=c("pregnancy_id","date")]
    diagnosis_exist<-diagnosis_exist[ ,component_Edetailed:=list(list(unique(component_E))),by=c("pregnancy_id","date")]
    diagnosis_exist[count_rows_perid>1,component_E:="more than one"]
    diagnosis_exist<-unique(diagnosis_exist[,count_rows_perid:=NULL],by=c("pregnancy_id","date","component_E"))
    
    assign(paste0("D3_indication_SAP1_pregnant_woman_",dr),merge(get(paste0("D3_indication_SAP1_pregnant_woman_",dr)),diagnosis_exist,all.x=T,by.x=c("person_id","pregnancy_id","date"),by.y=c("person_id","pregnancy_id","date")))
    
    
    
    #COMPONENT I------------------
    
    cond_drugs<-"date_disp>=date-365 & date_disp<=date+365 & !is.na(date_disp) & Meaning_of_drug_record=='outpatient_pharmacy_management' & meaning_of_visit=='treatment_episode'"

    
    for (event in c("EPILEPSY","GAD","NEUROPATHIC_PAIN")) {
      print(paste0("Computing component I for ",event)) 
      
      event_complete<-paste0(event,"_DRUGS")
      
      
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
      
      assign(event_complete,unique(get(event_complete)[,.(person_id,date_disp,visit_occurrence_id)]))
      assign(event_complete,merge(get(event_complete),unique(VISIT_OCCURRENCE[,.(visit_occurrence_id,meaning_of_visit)]),all.x=T,by="visit_occurrence_id"))

      
      
      assign(paste0(event_complete,"_exist"), MergeFilterAndCollapse(list(get(event_complete)),
                                                                     get(paste0("D3_indication_SAP1_pregnant_woman_",dr)),
                                                                     key=c("person_id"),
                                                                     condition =cond_drugs,
                                                                     strata=c("person_id","pregnancy_id","date"),
                                                                     sorting = c("pregnancy_id","date_disp"),
                                                                     summarystat = list(list(c("exist"),"date_disp", "event_present"))) )
      
      
      assign(paste0(event_complete,"_exist"),get(paste0(event_complete,"_exist"))[,event_present:=as.character(event_present)][,event_present:=event])
      rm(event_complete)
    }
    
    drugs_exist<-rbind(EPILEPSY_DRUGS_exist,NEUROPATHIC_PAIN_DRUGS_exist,GAD_DRUGS_exist)
    rm("EPILEPSY_DRUGS_exist")
    rm("NEUROPATHIC_PAIN_DRUGS_exist")
    rm("GAD_DRUGS_exist")
    setnames(drugs_exist,"event_present","component_I")
    
    drugs_exist[,count_rows_perid:=.N,by=c("pregnancy_id","date")]
    drugs_exist<-drugs_exist[ ,component_Idetailed:=list(list(unique(component_I))),by=c("pregnancy_id","date")]
    drugs_exist[count_rows_perid>1,component_I:="more than one"]
    drugs_exist<-unique(drugs_exist[,count_rows_perid:=NULL],by=c("pregnancy_id","date","component_I"))
    
    assign(paste0("D3_indication_SAP1_pregnant_woman_",dr),merge(get(paste0("D3_indication_SAP1_pregnant_woman_",dr)),drugs_exist,all.x=T,by.x=c("person_id","pregnancy_id","date"),by.y=c("person_id","pregnancy_id","date")))
    
    rm("drugs_exist")
  }

  
  ######################################
  #shell tables 
  
  assign(paste0("D4_proportion_per_component_pregnancy_",algo) ,data.table())
  
  for (dr in c("gaba","pregaba","any")) {
    shell_base<-get(paste0("D3_indication_SAP1_pregnant_woman_",dr))[, .SD, .SDcols = patterns("^comp|person_id|pregnancy_")]
    shell_base<-unique(shell_base,by=colnames(shell_base[,!grep("detailed", colnames(shell_base)),with=F] ))
    
    smart_save(shell_base, dirtemp, override_name = paste0("D3_components_pregnancy_",algo,"_",dr,"_validation_FISABIO"), extension = "rds")
    
    shell_base_csv<-copy(shell_base)
    
    shell_base_csv <- shell_base_csv[, lapply(.SD, function(x) {
      x[lengths(x) == 0] <- NA
      return(x)
    })]
    
    fwrite(shell_base_csv, file=paste0(dirtemp,"D3_components_pregnancy_",algo,"_",dr,"_validation_FISABIO",".csv"))
    
    

    #for each component we have to go from one row per dispensing to one per pregnancy
    columns_to_exclude<-colnames(shell_base)[(grep("detailed$", colnames(shell_base), perl=TRUE))]
    columns_to_include<-colnames(shell_base)[!(colnames(shell_base) %in% columns_to_exclude)]
    shell_base<-shell_base[,..columns_to_include]
    
    #for each component we have to go from one row per dispensing to one per pregnancy
    for (comp in c("B","C","D","E","F","Fbis","I" ,"G","H")) {
      name<-paste0("component_",comp)
      
      if (name %in% colnames(shell_base)) {
        assign(paste0("shell_base_",comp),shell_base[,c("person_id","pregnancy_id",name),with=F])
        
        assign(paste0("shell_base_",comp),get((paste0("shell_base_",comp)))[, paste0("count_rows_",comp) := uniqueN(get(paste0("component_",comp)),na.rm=T), by=c("pregnancy_id")])
        
        assign(paste0("shell_base_",comp),unique(get((paste0("shell_base_",comp)))[!(is.na(get(name)) & get(paste0("count_rows_",comp))==1),]))
        
        suppressWarnings(assign(paste0("shell_base_",comp),get((paste0("shell_base_",comp)))[get(paste0("count_rows_",comp))>1,(name):="more than one"]))
        
        suppressWarnings(assign(paste0("shell_base_",comp),unique(get((paste0("shell_base_",comp)))[is.na(get(name)),(name):="none"])))
        
        assign(paste0("table_",comp,"_",dr),get((paste0("shell_base_",comp)))[,denominator:=nrow(get((paste0("shell_base_",comp))))][, .(numerator = .N), by = c(name,"denominator")])
        
        setnames(get(paste0("table_",comp,"_",dr)),name,"indication")
        
        if (dr=="gaba") medication_name<-"gabapentin"
        if (dr=="pregaba") medication_name<-"pregabalin"
        if (dr=="any") medication_name<-"any gabapentinoids"
        
        get(paste0("table_",comp,"_",dr))[,medication:=medication_name][,"component":=comp][,algorithm:=algo]
        assign(paste0("D4_proportion_per_component_pregnancy_",algo),rbind(get(paste0("D4_proportion_per_component_pregnancy_",algo)),get(paste0("table_",comp,"_",dr)),fill=T))
      }
    }
    
    setcolorder(get(paste0("D4_proportion_per_component_pregnancy_",algo)), c("algorithm","component", "medication", "indication","numerator","denominator"))
    
    get(paste0("D4_proportion_per_component_pregnancy_",algo))[indication=="GAD",indication:="anxiety"][,indication:=tolower(indication)]
    get(paste0("D4_proportion_per_component_pregnancy_",algo))[,"datasource":=thisdatasource]
  }
  
  
  fwrite(get(paste0("D4_proportion_per_component_pregnancy_",algo)),file=paste0(direxp,"D4_proportion_per_component_pregnancy_",algo,"_validation_FISABIO",".csv"))
  
  
  
  assigned_levels <- vector(mode="list")
  # assigned_levels[["Number_diagnosis"]] <- c("Number_diagnostic_codes_detected", "Number_diagnostic_codes_detected2")
  assigned_levels[["Algorithm"]] <- c("algorithm")
  assigned_levels[["Component"]] <- c("component")
  assigned_levels[["Medication"]] <- c("medication")
  assigned_levels[["Indication"]] <- c("indication")
  
  
  
  assign(paste0("D4_proportion_per_component_pregnancy_",algo), Cube(input = get(paste0("D4_proportion_per_component_pregnancy_",algo)),
                                                           dimensions = c("Algorithm","Component","Medication","Indication"),
                                                           levels = assigned_levels,
                                                           measures = c("numerator","denominator"))   )
  
  get(paste0("D4_proportion_per_component_pregnancy_",algo))[Medication_LabelValue!="any gabapentinoids",Medication_LevelOrder:=1]
  
  
  # Find if a level contains at least a value to censor
  summary_threshold <- 5
  tmp <- copy(get(paste0("D4_proportion_per_component_pregnancy_",algo)))
  
  #
  for(measure in c("numerator_sum","denominator_sum")) {
    tmp[, (measure) := fifelse(get(measure) < summary_threshold & get(measure) > 0, F, T)] 
  }
  
  tmp <- tmp[, lapply(.SD, all), by = c("Component_LabelValue","Component_LevelOrder","Algorithm_LevelOrder", "Medication_LevelOrder","Indication_LevelOrder"),
             .SDcols = c("numerator_sum","denominator_sum")] 
  
  # smart_save(tmp, dircheck, override_name = paste("D4_proportion_per_component_pregnancy",algo, "validation_FISABIO_summary_levels", sep = "_"), extension = "csv")
  
  fwrite(tmp,file=paste0(dircheck,"D4_proportion_per_component_pregnancy_",algo, "_validation_FISABIO_summary_levels",".csv"))
  
}


