###################  GABAPENTIN
#keep only dispensations overlapping the spells and with at least 1 year of distance from entry spell and 1 year from exit spell
load(paste0(dirconceptsets,"GABAPENTIN.RData"))
load(paste0(dirconceptsets,"PREGABALIN.RData"))

ANY_GABA<-rbind(PREGABALIN,GABAPENTIN,fill=T)

GABAPENTIN<-unique(GABAPENTIN[!is.na(date)][, .(person_id,medicinal_product_id,codvar,date)][,drug:="gabapentin"])
PREGABALIN<-unique(PREGABALIN[!is.na(date)][, .(person_id,medicinal_product_id,codvar,date)][,drug:="pregabalin"])


ANY_GABA<-ANY_GABA[!is.na(date)][, .(person_id,medicinal_product_id,codvar,date)][,drug:="anygabapentinoids"]

gabapentinoids<-rbind(PREGABALIN,GABAPENTIN,ANY_GABA)

gabapentinoids[,date2:=date]
setkeyv(gabapentinoids,c("person_id","date","date2"))
# Load person_id in study_populations
smart_load("D4_study_population_pregnancy", dirtemp, extension = extension)

#checks to remove
D4_study_population_pregnancy<-D4_study_population_pregnancy[pregnancy_start_date<=pregnancy_end_date & !is.na(pregnancy_end_date),]
###############

D4_study_population_pregnancy<-D4_study_population_pregnancy[!is.na(cohort_entry_date),]

#create window: months0_12_before_pregnancy<-From LMP date – 365 days to LMP date – 1 day
D4_study_population_pregnancy[,LMP_minus1:=pregnancy_start_date-1][,Oneyear_before_preg:=pregnancy_start_date-365]

#months0_3_before_pregnancy<-From LMP date – 90 days (75 DAYS FOR EFEMERIS) to LMP date – 1 day*
window2_days<-90
if(thisdatasource=="EFEMERIS") window2_days<-75

D4_study_population_pregnancy[,Threemonths_before_preg:=pregnancy_start_date-window2_days]

#First_trimester<-From LMP date to LMP date + 97 days
D4_study_population_pregnancy[,First_trimester_end:=pregnancy_start_date+97][First_trimester_end>pregnancy_end_date,First_trimester_end:=NA]

#Second_trimester<-From LMP date + 98 days to LMP date + 195 days
D4_study_population_pregnancy[,Second_trimester_start:=pregnancy_start_date+98][,Second_trimester_end:=pregnancy_start_date+195][Second_trimester_end>pregnancy_end_date,Second_trimester_end:=NA][Second_trimester_start>pregnancy_end_date,Second_trimester_start:=NA]

#Third_trimester<-From LMP date + 196 days to end date of pregnancy
D4_study_population_pregnancy[,Third_trimester_start:=pregnancy_start_date+196][Third_trimester_start>pregnancy_end_date,Third_trimester_start:=NA]

#delivery date + 1 
D4_study_population_pregnancy[,Delivery_date_plus1:=pregnancy_end_date+1]

#0 – 3 months after pregnancy 
D4_study_population_pregnancy[,After_pregnancy_3months:=pregnancy_end_date+90]


D4_study_population_pregnancy<-D4_study_population_pregnancy[calendar_year_LMP>2005,]



#hell Table 6. Annual prevalence of gabapentinoids prescription/dispensation per 1,000 pregnancies, with 95% CI, in data source X (N= Number of pregnancies in the time period, n= Number of pregnancies exposed in the time period).

#definitions of windows
  window1<-list(list("Oneyear_before_preg","LMP_minus1"))
  window2<-list(list("Threemonths_before_preg","LMP_minus1"))
  window3<-"pregnancy_start_date"  #not used 
  window4<-list(list("pregnancy_start_date","pregnancy_end_date"))
  window5<-list(list("pregnancy_start_date","First_trimester_end"))
  window6<-list(list("Second_trimester_start","Second_trimester_end"))
  window7<-list(list("Third_trimester_start","pregnancy_end_date"))
  window8<-list(list("Delivery_date_plus1","After_pregnancy_3months"))
  


  period_prevalence<-data.table()
  
for (suffix in c(1,2,4,5,6,7,8) ){
  print(paste0("window ",suffix))
  window<-get(paste0("window",suffix))
  
  if(suffix==1) num_name<-"num_before_1y"
  if(suffix==2) num_name<-"num_before_3m"
  if(suffix==4) num_name<-"num_during"
  if(suffix==5) num_name<-"num_trim1"
  if(suffix==6) num_name<-"num_trim2"
  if(suffix==7) num_name<-"num_trim3"
  if(suffix==8) num_name<-"num_after"

  assign(paste0("D4_study_population_pregnancy",suffix), D4_study_population_pregnancy[!is.na(get(window[[1]][[1]])) & !is.na(get(window[[1]][[2]])),])
  
  
  assign(paste0("period_prevalence",suffix),CountPrevalence(get(paste0("D4_study_population_pregnancy",suffix)), gabapentinoids,
                                             UoO_id = "pregnancy_id", key=c("person_id"),
                                             Start_date = "cohort_entry_date",
                                             End_date = "cohort_exit_date", 
                                             Name_condition = "drug", Date_condition = "date",
                                             Type_prevalence = "period", Periods_of_time = window,
                                             Start_study_time = study_start, End_study_time = study_end,
                                             Conditions = unique(gabapentinoids[, drug]),
                                             Strata=c("calendar_year_LMP"),
                                             Aggregate = T))
  
  
  setorder(get(paste0("period_prevalence",suffix)),"calendar_year_LMP")
  assign(paste0("period_prevalence",suffix),get(paste0("period_prevalence",suffix))[,timeframe:=NULL])
  assign(paste0("period_prevalence",suffix,"aggregated"),get(paste0("period_prevalence",suffix))[,lapply(.SD ,sum),by=c("calendar_year_LMP"), .SDcols=c("prev_anygabapentinoids","prev_gabapentin","prev_pregabalin" ,"in_population"  )  ])
  
  assign(paste0("period_prevalence",suffix,"aggregated"),get(paste0("period_prevalence",suffix,"aggregated"))[,window:=num_name])
  
  period_prevalence<-rbind(period_prevalence,get(paste0("period_prevalence",suffix,"aggregated"))[,window:=as.factor(window)],fill=TRUE)
}

period_prevalence_long <- melt(period_prevalence, measure = c("prev_anygabapentinoids","prev_gabapentin","prev_pregabalin"   ) ,
                                value.name = "numerator", variable.name = "drug",
                                variable.factor = F)
  
period_prevalence_long[,drug:=substr(drug, 6, nchar(drug))]
  
period_prevalence_long1 <- data.table::dcast(period_prevalence_long, calendar_year_LMP + drug ~ window , fill = period_prevalence_long$numerator, drop = T, value.var = "numerator")
  
period_prevalence_long[,window:=gsub("num", "den", window)]
period_prevalence_long2 <- data.table::dcast(period_prevalence_long, calendar_year_LMP + drug ~ window , fill = period_prevalence_long$in_population, drop = T, value.var = "in_population")

period_prevalence_final<-merge(period_prevalence_long1,period_prevalence_long2,by=c("calendar_year_LMP","drug"))
  
period_prevalence_final[,datasource:=thisdatasource]

col_order<-c("calendar_year_LMP", "drug","num_before_1y","num_before_3m" ,"num_during","num_trim1","num_trim2","num_trim3","num_after",       "den_before_1y","den_before_3m","den_during","den_trim1", "den_trim2","den_trim3","den_after","datasource")

setcolorder(period_prevalence_final,col_order)
fwrite(period_prevalence_final,file=paste0(direxp,"Shell_table_6.csv"))




#Shell Table 4. Prevalence of exposure to a given medication per 1,000 pregnancies, with 95% CI, in data source X (N= Number of pregnancies in the time period, n= Number of pregnancies exposed to the given medication in the time period)

drugs<-data.table()

concept_sets_of_our_study<-c("TRAMADOL","LIDOCAINE_PLASTER","MORPHINE","OXYCODONE","TAPENTADOL","ZICONOTIDE","OTHER_STRONG_OPIOIDS","AMITRIPTYLINE","CLOMIPRAMINE"
,"IMIPRAMINE","VENLAFAXINE","DULOXETINE","BOTULINUM_TOXIN_A","NORTRIPTYLINE"
,"CARBAMAZEPINE","METHADONE","ANTIEPILEPTICS","CLOBAZAM","BENZODIAZEPINE_DERIVATIVES",
"ANTIDEPRESSANTS","GABAPENTIN","PREGABALIN")
for (concept in concept_sets_of_our_study) {
  load(paste0(dirconceptsets,concept,".RData"))

  assign(concept,unique(get(concept)[!is.na(date)][, .(person_id,medicinal_product_id,codvar,date)][,drug:=concept]))
  if(nrow(get(concept))>0) drugs<-rbind(drugs,get(concept))
}




period_prevalence<-data.table()

for (suffix in c(1,2,4,5,6,7,8) ){
  print(paste0("window ",suffix))
  window<-get(paste0("window",suffix))

  assign(paste0("D4_study_population_pregnancy",suffix), D4_study_population_pregnancy[!is.na(get(window[[1]][[1]])) & !is.na(get(window[[1]][[2]])),])

  if (suffix==1 | suffix==2 ) {
    add<-get(paste0("D4_study_population_pregnancy",suffix))[1, .(person_id,pregnancy_id,pregnancy_start_date)][,add_date:=pregnancy_start_date+30]
    
    vars_to_add <- data.table(person_id = add[1, person_id], date = add[1, add_date]  ,
                              drug = c(concept_sets_of_our_study), 
                              codvar = "DO NOT USE")
    
    assign(paste0("drugs",suffix),rbind(drugs, vars_to_add,fill=T))
  }
  
  
  if (suffix==4  | suffix==5  |suffix==6  |suffix==7  | suffix==8 ) {
    add<-get(paste0("D4_study_population_pregnancy",suffix))[1, .(person_id,pregnancy_id,pregnancy_start_date)][,add_date:=pregnancy_start_date-30]
    
    vars_to_add <- data.table(person_id = add[1, person_id], date = add[1, add_date]  ,
                              drug = c(concept_sets_of_our_study), 
                              codvar = "DO NOT USE")
    
    assign(paste0("drugs",suffix),rbind(drugs, vars_to_add,fill=T))
  }
  
  if(suffix==1) num_name<-"num_before_1y"
  if(suffix==2) num_name<-"num_before_3m"
  if(suffix==4) num_name<-"num_during"
  if(suffix==5) num_name<-"num_trim1"
  if(suffix==6) num_name<-"num_trim2"
  if(suffix==7) num_name<-"num_trim3"
  if(suffix==8) num_name<-"num_after"
  
  assign(paste0("period_prevalence",suffix),CountPrevalence(get(paste0("D4_study_population_pregnancy",suffix)), get(paste0("drugs",suffix)),
                                                            UoO_id = "pregnancy_id", key=c("person_id"),
                                                            Start_date = "cohort_entry_date",
                                                            End_date = "cohort_exit_date", 
                                                            Name_condition = "drug", Date_condition = "date",
                                                            Type_prevalence = "period", Periods_of_time = window,
                                                            Start_study_time = study_start, End_study_time = study_end,
                                                            Conditions = unique(get(paste0("drugs",suffix))[, drug]),
                                                            #Strata=c("calendar_year_LMP"),
                                                            Aggregate = F))


  assign(paste0("period_prevalence",suffix,"aggregated"),get(paste0("period_prevalence",suffix))[,lapply(.SD ,sum), .SDcols=c( "prev_TRAMADOL"  ,         "prev_LIDOCAINE_PLASTER"   ,       "prev_MORPHINE"          ,         "prev_OXYCODONE"                , "prev_TAPENTADOL"      ,   "prev_ZICONOTIDE"       ,  "prev_OTHER_STRONG_OPIOIDS"   ,    "prev_AMITRIPTYLINE"   ,"prev_CLOMIPRAMINE"  ,    "prev_IMIPRAMINE"      ,           "prev_VENLAFAXINE"        ,        "prev_DULOXETINE"   , "prev_BOTULINUM_TOXIN_A"  ,   "prev_NORTRIPTYLINE"  ,            "prev_CARBAMAZEPINE" ,             "prev_METHADONE"      ,  "prev_ANTIEPILEPTICS"     ,        "prev_CLOBAZAM"     ,  "prev_BENZODIAZEPINE_DERIVATIVES", "prev_ANTIDEPRESSANTS",    "prev_GABAPENTIN"  ,     "prev_PREGABALIN" ,"in_population"  )  ])
  get(paste0("period_prevalence",suffix))[,timeframe:=NULL][,window:=num_name]


  period_prevalence<-rbind(period_prevalence,get(paste0("period_prevalence",suffix)))
}

period_prevalence_long <- melt(period_prevalence, measure = c( "prev_TRAMADOL"  ,         "prev_LIDOCAINE_PLASTER"   ,       "prev_MORPHINE"          ,         "prev_OXYCODONE"                , "prev_TAPENTADOL"      ,   "prev_ZICONOTIDE"       ,  "prev_OTHER_STRONG_OPIOIDS"   ,    "prev_AMITRIPTYLINE"   ,"prev_CLOMIPRAMINE"  ,    "prev_IMIPRAMINE"      ,           "prev_VENLAFAXINE"        ,        "prev_DULOXETINE"   , "prev_BOTULINUM_TOXIN_A"  ,   "prev_NORTRIPTYLINE"  ,            "prev_CARBAMAZEPINE" ,             "prev_METHADONE"      ,  "prev_ANTIEPILEPTICS"     ,        "prev_CLOBAZAM"     ,  "prev_BENZODIAZEPINE_DERIVATIVES", "prev_ANTIDEPRESSANTS",    "prev_GABAPENTIN"  ,     "prev_PREGABALIN"  ) ,
                               value.name = "numerator", variable.name = "drug",
                               variable.factor = F)

period_prevalence_long[,drug:=substr(drug, 6, nchar(drug))]

period_prevalence_long1 <- data.table::dcast(period_prevalence_long, drug ~ window , fill = period_prevalence_long$numerator, drop = T, value.var = "numerator")

period_prevalence_long[,window:=gsub("num", "den", window)]
period_prevalence_long2 <- data.table::dcast(period_prevalence_long, drug ~ window , fill = period_prevalence_long$in_population, drop = T, value.var = "in_population")

period_prevalence_final<-merge(period_prevalence_long1,period_prevalence_long2,by=c("drug"))

period_prevalence_final[,datasource:=thisdatasource]

fwrite(period_prevalence_final,file=paste0(direxp,"Shell_table_4.csv"))



#Shell Table 4Bis. Pattern of exposure to a given medication per 1,000 pregnancies, with 95% CI, in data source X (N= Number of pregnancies in the time period, n= Number of pregnancies exposed to the given medication in the time period)

#definitions of windows bis
window1<-list(list("Oneyear_before_preg","LMP_minus1"))
window2<-list(list("pregnancy_start_date","First_trimester_end"))
window3<-list(list("Second_trimester_start","pregnancy_end_date"))
window4<-list(list("Oneyear_before_preg","First_trimester_end"))

drugs<-gabapentinoids
period_prevalence<-data.table()

for (suffix in c(1,2,3,4) ){
  print(paste0("window ",suffix))
  window<-get(paste0("window",suffix))
  
  assign(paste0("D4_study_population_pregnancy",suffix), D4_study_population_pregnancy[!is.na(get(window[[1]][[1]])) & !is.na(get(window[[1]][[2]])),])
  
  if (suffix==1 ) {
    add<-get(paste0("D4_study_population_pregnancy",suffix))[1, .(person_id,pregnancy_id,pregnancy_start_date)][,add_date:=pregnancy_start_date+100]
    
    vars_to_add <- data.table(person_id = add[1, person_id], date = add[1, add_date]  ,
                              drug = c(concept_sets_of_our_study), 
                              codvar = "DO NOT USE")
    
    assign(paste0("drugs",suffix),rbind(drugs, vars_to_add,fill=T))
  }
  
  
  if (suffix==2  | suffix==3 ) {
    add<-get(paste0("D4_study_population_pregnancy",suffix))[1, .(person_id,pregnancy_id,pregnancy_start_date)][,add_date:=pregnancy_start_date-30]
    
    vars_to_add <- data.table(person_id = add[1, person_id], date = add[1, add_date]  ,
                              drug = c(concept_sets_of_our_study), 
                              codvar = "DO NOT USE")
    
    assign(paste0("drugs",suffix),rbind(drugs, vars_to_add,fill=T))
  }
  
  if(suffix==1) num_name<-"num_before"
  if(suffix==2) num_name<-"num_trim1"
  if(suffix==3) num_name<-"num_trim2-3"
  if(suffix==4) num_name<-"num_before_trim1"
  
  assign(paste0("period_prevalence",suffix),CountPrevalence(get(paste0("D4_study_population_pregnancy",suffix)), get(paste0("drugs",suffix)),
                                                            UoO_id = "pregnancy_id", key=c("person_id"),
                                                            Start_date = "cohort_entry_date",
                                                            End_date = "cohort_exit_date", 
                                                            Name_condition = "drug", Date_condition = "date",
                                                            Type_prevalence = "period", Periods_of_time = window,
                                                            Start_study_time = study_start, End_study_time = study_end,
                                                            Conditions = unique(get(paste0("drugs",suffix))[, drug]),
                                                            #Strata=c("calendar_year_LMP"),
                                                            Aggregate = T))
  
  
  assign(paste0("period_prevalence",suffix,"aggregated"),get(paste0("period_prevalence",suffix))[,lapply(.SD ,sum), .SDcols=c("prev_anygabapentinoids","prev_gabapentin","prev_pregabalin" ,"in_population")])
  get(paste0("period_prevalence",suffix))[,timeframe:=NULL][,window:=num_name]
  
  
  period_prevalence<-rbind(period_prevalence,get(paste0("period_prevalence",suffix)))
}

period_prevalence_long <- melt(period_prevalence, measure = c("prev_anygabapentinoids","prev_gabapentin","prev_pregabalin") ,value.name = "numerator", variable.name = "drug",
                               variable.factor = F)

period_prevalence_long[,drug:=substr(drug, 6, nchar(drug))]

period_prevalence_long1 <- data.table::dcast(period_prevalence_long, drug ~ window , fill = period_prevalence_long$numerator, drop = T, value.var = "numerator")

period_prevalence_long[,window:=gsub("num", "den", window)]
period_prevalence_long2 <- data.table::dcast(period_prevalence_long, drug ~ window , fill = period_prevalence_long$in_population, drop = T, value.var = "in_population")

period_prevalence_final<-merge(period_prevalence_long1,period_prevalence_long2,by=c("drug"))

period_prevalence_final[,datasource:=thisdatasource]

fwrite(period_prevalence_final,file=paste0(direxp,"Shell_table_4bis.csv"))

