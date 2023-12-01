
#combine the components results to create the algorithms

for (algo in c("V1","V2")) {
  assign(paste0("D4_proportion_per_algorithm_",algo) ,data.table())
  
  for (dr in c("gaba","pregaba","any")) {
    smart_load(paste0("D3_components_M1","_",dr), dirtemp, extension = "rds")
    
    y_end<-as.character(get(paste0("D3_components_M1","_",dr))[,max(year(first_disp))])
    
    if (dr=="gaba") medication_name<-"gabapentin"
    if (dr=="pregaba") medication_name<-"pregabalin"
    if (dr=="any") medication_name<-"any gabapentinoids"
    
    copy_tmp<-copy(get(paste0("D3_components_M1","_",dr)))
    copy_tmp[,Year_first_disp:=paste0("2006-",y_end)]
    
    assign(paste0("D3_components_",algo,"_",dr),get(paste0("D3_components_M1","_",dr))[,Year_first_disp:= fifelse(year(first_disp)>=2006 & year(first_disp)<=2012 , "2006-2012", paste0("2013-",y_end))])
    
    assign(paste0("D3_components_",algo,"_",dr),rbind(get(paste0("D3_components_",algo,"_",dr)),copy_tmp))
    
    if (algo=="V2") {
      get(paste0("D3_components_",algo,"_",dr))[,indicationB_C:="NULL"]
      
      comp_list<-list("E","F","Fbis","G","H","I")
      comp<-comp_list[[1]]
      
      name<-paste0("component_",comp,"detailed")
      if (name %in% colnames(get(paste0("D3_components_",algo,"_",dr)))) {
        get(paste0("D3_components_",algo,"_",dr))[,indication2:=get(name)]
      }else{
        get(paste0("D3_components_",algo,"_",dr))[,indication2:=list(list(c("NULL")))]
      }
      
      
      for (i in 1:length(comp_list)) {
        if(i==length(comp_list)){
          comp2<-comp_list[[i]]
        }else{
          comp2<-comp_list[[i+1]]
        }
        
        
        name2<-paste0("component_",comp2,"detailed")
        if (name2 %in% colnames(get(paste0("D3_components_",algo,"_",dr)))) {
          get(paste0("D3_components_",algo,"_",dr))[indication2=="NULL",indication2:=get(name2)]
          get(paste0("D3_components_",algo,"_",dr))[ get(name2)!="NULL" ,indication2:=mapply(c, indication2, get(name2), SIMPLIFY = F)]
          
          
          # assign(paste0("D3_components_",algo,"_",dr),unique(get(paste0("D3_components_",algo,"_",dr))[!is.na(indicationB_C),indicationB_C:=1][is.na(indicationB_C),indicationB_C:=0][,indicationB_C:=max(indicationB_C),by=c("person_id","Year_first_disp")][indicationB_C==1,indication2:=NA]))
          # 
          # 
          # if( "indication" %in% colnames(get(paste0("D3_components_",algo,"_",dr)))) {
          #   get(paste0("D3_components_",algo,"_",dr))[is.na(indication),indication:=indication2]
          # }else{
          #   get(paste0("D3_components_",algo,"_",dr))[,indication:=indication2]
          # }
        }
      }
      
      
      assign(paste0("D3_components_",algo,"_",dr),get(paste0("D3_components_",algo,"_",dr))[,indication2:=lapply(indication2,unique)])
      
      #put together information on B and C (indicationB_C) and indication2: if indicationB_C is not empty, they have the priority so remove any info on the other components
      assign(paste0("D3_components_",algo,"_",dr),get(paste0("D3_components_",algo,"_",dr))[indicationB_C!="NULL",indicationB_C:=1][indicationB_C=="NULL",indicationB_C:=0][,indicationB_C:=max(as.numeric(indicationB_C)),by=c("person_id","Year_first_disp")][indicationB_C==1,indication2:="NULL"])
      
      #create variable indication for daps with no primaty care, fill it with indication2. Complete variable indication, which is filled for DAPs with component B and/or C)
      if( "indication" %in% colnames(get(paste0("D3_components_",algo,"_",dr)))) {
        get(paste0("D3_components_",algo,"_",dr))[indication=="NULL",indication:=indication2]
      }else{
        get(paste0("D3_components_",algo,"_",dr))[,indication:=indication2]
      }
      
    }else{
      get(paste0("D3_components_",algo,"_",dr))[,indication:=get("component_Ddetailed")]
    }
    
    
    #CREATE BINARY VARIABLES to flag if the indication contains "EPILEPSY", ""GAD" or "NP
    get(paste0("D3_components_",algo,"_",dr))[,"epilepsy":=0][mapply(`%in%`, "EPILEPSY", indication),"epilepsy":=1]
    get(paste0("D3_components_",algo,"_",dr))[,"anxiety":=0][mapply(`%in%`, "GAD", indication),"anxiety":=1]
    get(paste0("D3_components_",algo,"_",dr))[,"neuropathic_pain":=0][mapply(`%in%`, "NEUROPATHIC_PAIN", indication),"neuropathic_pain":=1]
    get(paste0("D3_components_",algo,"_",dr))[,"potential_mise_abuse":=0][mapply(`%in%`, "POTENTIAL_MISE_ABUSE", indication),"potential_mise_abuse":=1]
    get(paste0("D3_components_",algo,"_",dr))[,"other_uses":=0][mapply(`%in%`, "OTHER_USES", indication),"other_uses":=1]
    
    
    names<-c("epilepsy","anxiety","neuropathic_pain","potential_mise_abuse","other_uses")
    get(paste0("D3_components_",algo,"_",dr))[,(names):=lapply(.SD,max),.SDcols=names,by=c("person_id","Year_first_disp")]
    get(paste0("D3_components_",algo,"_",dr))[,sum:=rowSums(.SD),.SDcols=c("epilepsy","anxiety","neuropathic_pain","potential_mise_abuse","other_uses")]
    
    
    #extract character from indication list of list
    suppressWarnings(get(paste0("D3_components_",algo,"_",dr))[sum>1,indicationfinal:="more than one"][sum==1,indicationfinal:=indication][,indication:=NULL])
    setnames(get(paste0("D3_components_",algo,"_",dr)),"indicationfinal","indication")
    
    
    
    
    #the number of rows not correspond to the number of person id (it is higer because some pregnancies may have different indication detected by different dispensing)
    assign(paste0("D3_components_",algo,"_",dr),unique(get(paste0("D3_components_",algo,"_",dr))[,.(person_id,Year_first_disp,indication,epilepsy,anxiety,neuropathic_pain,potential_mise_abuse,other_uses,sum)][ ,N_rows:=uniqueN(indication,na.rm=T),by=c("person_id","Year_first_disp")][N_rows>1 & indication=="NULL",remove_row:=1]))
    
    assign(paste0("D3_components_",algo,"_",dr),unique(get(paste0("D3_components_",algo,"_",dr))[is.na(remove_row),][,N_rows:=uniqueN(indication,na.rm=T),by=c("person_id","Year_first_disp")][N_rows>1,indication:="more than one"]))
    
    
    
    assign(paste0("D3_components_",algo,"_",dr),get(paste0("D3_components_",algo,"_",dr))[!(N_rows==1 & is.na(indication)),][N_rows==0,indication:="none"])
    fwrite(get(paste0("D3_components_",algo,"_",dr)), file=paste0(diroutput,"D3_components_",algo,"_",dr,".csv"))
    
    
    
    ###
    assign("part1",unique(get(paste0("D3_components_",algo,"_",dr))[,.(person_id,Year_first_disp,indication)]))
    
    part1<-part1[,denominator:=.N,by = c("Year_first_disp")][, .(numerator = .N), by = c("indication","denominator","Year_first_disp")][,algorithm:=algo][,datasource:=thisdatasource][,medication:=medication_name]
    
    
    assign("part2_epilepsy",get(paste0("D3_components_",algo,"_",dr))[,denominator:=.N,by = c("Year_first_disp")][sum>1, .(numerator = .N), by = c("epilepsy","denominator","Year_first_disp")][,algorithm:=algo][,datasource:=thisdatasource][,medication:=medication_name][epilepsy==1,][,epilepsy:=as.character(epilepsy)][,epilepsy:="Including epilepsy"])
    setnames(part2_epilepsy,"epilepsy","indication")
    
    assign("part2_anxiety",get(paste0("D3_components_",algo,"_",dr))[,denominator:=.N,by = c("Year_first_disp")][sum>1, .(numerator = .N), by = c("anxiety","denominator","Year_first_disp")][,algorithm:=algo][,datasource:=thisdatasource][,medication:=medication_name][anxiety==1,][,anxiety:=as.character(anxiety)][,anxiety:="Including anxiety"])
    setnames(part2_anxiety,"anxiety","indication")
    
    assign("part2_np",get(paste0("D3_components_",algo,"_",dr))[,denominator:=.N,by = c("Year_first_disp")][sum>1, .(numerator = .N), by = c("neuropathic_pain","denominator","Year_first_disp")][,algorithm:=algo][,datasource:=thisdatasource][,medication:=medication_name][neuropathic_pain==1,][,neuropathic_pain:=as.character(neuropathic_pain)][,neuropathic_pain:="Including neuropathic pain"])
    setnames(part2_np,"neuropathic_pain","indication")
    
    
    assign("part2_potential_mise_abuse",get(paste0("D3_components_",algo,"_",dr))[,denominator:=.N,by = c("Year_first_disp")][sum>1, .(numerator = .N), by = c("potential_mise_abuse","denominator","Year_first_disp")][,algorithm:=algo][,datasource:=thisdatasource][,medication:=medication_name][potential_mise_abuse==1,][,potential_mise_abuse:=as.character(potential_mise_abuse)][,potential_mise_abuse:="Including potential mise abuse"])
    setnames(part2_potential_mise_abuse,"potential_mise_abuse","indication")
    
    assign("part2_other_uses",get(paste0("D3_components_",algo,"_",dr))[,denominator:=.N,by = c("Year_first_disp")][sum>1, .(numerator = .N), by = c("other_uses","denominator","Year_first_disp")][,algorithm:=algo][,datasource:=thisdatasource][,medication:=medication_name][other_uses==1,][,other_uses:=as.character(other_uses)][,other_uses:="Including other uses"])
    setnames(part2_other_uses,"other_uses","indication")
    
    assign(paste0("D3_components_",algo,"_",dr,"_total"),rbind(part1,part2_epilepsy,part2_anxiety,part2_np,part2_potential_mise_abuse,part2_other_uses,fill=T))
    
    # assign(paste0("D3_components_",algo,"_",dr),get(paste0("D3_components_",algo,"_",dr))[,denominator:=.N,by = c("Year_first_disp")][, .(numerator = .N), by = c("indication","denominator","Year_first_disp")][,algorithm:=algo][,datasource:=thisdatasource][,medication:=medication_name])
    
    assign(paste0("D4_proportion_per_algorithm_",algo),rbind(get(paste0("D4_proportion_per_algorithm_",algo)),get(paste0("D3_components_",algo,"_",dr,"_total")),fill=T))
    
  }
  
  
  setcolorder(get(paste0("D4_proportion_per_algorithm_",algo)), c("algorithm", "medication","Year_first_disp", "indication","numerator","denominator","datasource"))
  
  get(paste0("D4_proportion_per_algorithm_",algo))[indication=="GAD",indication:="anxiety"][,indication:=tolower(indication)]
  get(paste0("D4_proportion_per_algorithm_",algo))[,"datasource":=thisdatasource]
  
  fwrite(get(paste0("D4_proportion_per_algorithm_",algo)),file=paste0(direxp,"D4_proportion_per_algorithm_",algo,".csv"))
  
  
  
  assigned_levels <- vector(mode="list")
  # assigned_levels[["Number_diagnosis"]] <- c("Number_diagnostic_codes_detected", "Number_diagnostic_codes_detected2")
  assigned_levels[["Algorithm"]] <- c("algorithm")
  assigned_levels[["Year_first_disp"]] <- c("Year_first_disp")
  assigned_levels[["Medication"]] <- c("medication")
  assigned_levels[["Indication"]] <- c("indication")
  
  
  
  assign(paste0("D4_proportion_per_algorithm_",algo), Cube(input = get(paste0("D4_proportion_per_algorithm_",algo)),
                                                           dimensions = c("Algorithm","Year_first_disp","Medication","Indication"),
                                                           levels = assigned_levels,
                                                           measures = c("numerator","denominator"))   )
  
  get(paste0("D4_proportion_per_algorithm_",algo))[Medication_LabelValue!="any gabapentinoids",Medication_LevelOrder:=1]

  get(paste0("D4_proportion_per_algorithm_",algo))[Year_first_disp_LabelValue!="2006-2020",Year_first_disp_LevelOrder:=1]
  
  # Find if a level contains at least a value to censor
  summary_threshold <- 5
  tmp <- copy(get(paste0("D4_proportion_per_algorithm_",algo)))
  
  #
  for(measure in c("numerator_sum","denominator_sum")) {
    tmp[, (measure) := fifelse(get(measure) < summary_threshold & get(measure) > 0, F, T)] 
  }
  
  tmp <- tmp[, lapply(.SD, all), by = c( "Year_first_disp_LevelOrder","Medication_LevelOrder", "Algorithm_LevelOrder","Indication_LevelOrder"),
             .SDcols = c("numerator_sum","denominator_sum")] 
  
  # smart_save(tmp, dircheck, override_name = paste("D4_proportion_per_algorithm",algo, "summary_levels", sep = "_"), extension = "csv")
  fwrite(tmp,file=paste0(dircheck,"D4_proportion_per_algorithm_",algo, "_summary_levels",".csv"))
}







