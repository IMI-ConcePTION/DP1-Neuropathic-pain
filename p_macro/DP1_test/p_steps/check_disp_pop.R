#check------------------------------------------------------------------------------------

#start from all the dispensing of gabapentin and from the study population

smart_load("D4_study_population_gaba_pregnancy", diroutput)
print(paste0(length(unique(D4_study_population_gaba_pregnancy$pregnancy_id)) ," pregnancies were selected"))

smart_load("selected_dispensing_gabapentin_pregnancy", dirtemp)
print(paste0(nrow(selected_dispensing_gabapentin_pregnancy) ," gabapentin dispensing were selected"))

# check_gaba<-merge(selected_dispensing_gabapentin_pregnancy,D4_study_population_gaba_pregnancy, all = F,by="person_id",allow.cartesian = T)

check<-selected_dispensing_gabapentin_pregnancy[person_id %in% D4_study_population_gaba_pregnancy$person_id,]

print(paste0(nrow(check) ," gabapentin dispensing were selected after link with population"))

########pregabalin
smart_load("D4_study_population_pregaba_pregnancy", diroutput)
print(paste0(length(unique(D4_study_population_pregaba_pregnancy$pregnancy_id)) ," pregnancies were selected"))


smart_load("selected_dispensing_pregabalin_pregnancy", dirtemp)
print(paste0(nrow(selected_dispensing_pregabalin_pregnancy) ," pregabalin dispensing were selected"))

# check_pregaba<-merge(selected_dispensing_pregabalin_pregnancy,D4_study_population_pregaba_pregnancy, all = F,by="person_id",allow.cartesian = T)

check2<-selected_dispensing_pregabalin_pregnancy[person_id %in% D4_study_population_pregaba_pregnancy$person_id,]

print(paste0(nrow(check2) ," pregabalin dispensing were selected after link with population"))

################################################ end