# the output of this step are several lists and lists of lists to be used by CreateConceptsetDatasets

# concept_set_domains
# level 1 name of the conceptset (including in the name whether it is narrow or possible)
# level 2 domain

# concept_set_codes_our_study,
# level 1 name of the conceptset (including in the name whether it is narrow or possible)
# level 2 coding system
# level 3 list of codes incuded in that conceptset for that coding system

# concept_set_codes_our_study_excl
# level 1 name of the conceptset (including in the name whether it is narrow or possible)
# level 2 coding system
# level 3 list of codes to be excluded from that conceptset for that coding system

# the lists of all conceptsets 
# concept_sets_of_our_study

# input: the VAC4EU spreadsheets, restricted to the conceptsets associated with this study

OUT_codelist <- fread(paste0(thisdir,"/p_parameters/archive_parameters/codelist_DP1_EFEMERIS.csv"))
OUT_codelist <- OUT_codelist[, .(coding_system, code, event_abbreviation)]
#OUT_codelist <- OUT_codelist[type != "PrA"]
OUT_codelist <- OUT_codelist[code != "", ][, event_abbreviation := toupper(event_abbreviation)]

# TODO check next codelist
# OUT_codelist <- OUT_codelist[event_abbreviation %not in% c("CANCERSCREEN", "DIALHAEMODIAL")]
# OUT_codelist <- OUT_codelist[code == "A01.05", tags := "narrow"]

concept_sets_of_our_study<-unique(OUT_codelist$event_abbreviation)

concept_set_codes_our_study <- lapply(split(OUT_codelist, by = "event_abbreviation", keep.by = F),
                                      split, by = "coding_system", keep.by = F)

concept_set_codes_our_study <- lapply(concept_set_codes_our_study, sapply, unlist, use.names = F, simplify = F)

     
 concept_set_codes_our_study_excl <- vector(mode="list")
 
 if (thisdatasource=="SAIL" |thisdatasource=="EFEMERIS" | thisdatasource=="SNDS") {
   concept_set_codes_our_study_excl[["EPILEPSY_DRUGS"]][["ATC"]] <- c("N03AX16", "N03AX12","N03AF01")
 }else{
   concept_set_codes_our_study_excl[["EPILEPSY_DRUGS"]][["ATC"]] <- c("N03AX16", "N03AX12")
   
 }

 
 if (thisdatasource=="SAIL" |thisdatasource=="EFEMERIS" | thisdatasource=="SNDS") {
   concept_set_codes_our_study_excl[["GAD"]][["ATC"]] <- c("N06AA", "N06AX21") 
 }else{
   concept_set_codes_our_study_excl[["GAD"]][["ATC"]] <- c("N06AA", "N06AX") 
   
 }



concept_set_domains<- vector(mode="list")
for (concept in names(concept_set_codes_our_study)) {
  if (!grepl("DRUGS", concept) & concept!="GABAPENTIN" & concept!="PREGABALIN") {
    concept_set_domains[[concept]] = "Diagnosis"
  }else{
    concept_set_domains[[concept]] = "Medicines"
  }
}


rm(concept)
rm(OUT_codelist)

