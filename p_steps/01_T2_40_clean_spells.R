# CLEAN THE SPELLS

# input: D3_output_spells_category
# output: D3_clean_spells

load(paste0(dirtemp,"D3_PERSONS.RData"))

load(paste0(dirtemp,"D3_output_spells_category.RData"))

person_spell <- merge(D3_output_spells_category, D3_PERSONS, all.x = T, by = "person_id")

person_spell <- person_spell[, .(person_id, birth_date, death_date, entry_spell_category_crude = entry_spell_category,
                                 exit_spell_category_crude = exit_spell_category, op_meaning, num_spell)]

person_spell[, entry_spell_category := data.table::fifelse(birth_date < entry_spell_category_crude - 60,
                                                           entry_spell_category_crude,
                                                           birth_date)]
person_spell[, exit_spell_category := pmin(exit_spell_category_crude, death_date, na.rm = T)]

person_spell[, op_start_date_cleaned := data.table::fifelse(entry_spell_category != entry_spell_category_crude, 0, 1)]
person_spell[, op_end_date_cleaned := data.table::fifelse(exit_spell_category != exit_spell_category_crude, 0, 1)]
person_spell[, starts_at_birth := data.table::fifelse(entry_spell_category == birth_date, 1, 0)]
person_spell[, starts_after_ending := data.table::fifelse(entry_spell_category < exit_spell_category, 0, 1)]

# find spells that do not overlap the study period
person_spell[, no_overlap_study_period := fifelse(
  entry_spell_category > study_end | exit_spell_category < study_start, 1, 0)]

# find spells that are shorter than x days
person_spell[, less_than_x_days_and_not_starts_at_birth := fifelse(
  correct_difftime(pmin(exit_spell_category, study_end), entry_spell_category) <= min_spell_lenght & starts_at_birth == 0, 1, 0)]
  
 
  #add a criteria that identify the specific spell of interest
person_spell[starts_after_ending == 0 & no_overlap_study_period == 0 &
               less_than_x_days_and_not_starts_at_birth == 0 ,
             flag := 0]

person_spells_first_dispensation<-person_spell[flag==0,.(person_id,entry_spell_category,exit_spell_category)]

#importa concetto farmaci
load(paste0(dirconceptsets,"GABAPENTIN.RData"))
load(paste0(dirconceptsets,"PREGABALIN.RData"))

GABAPENTINOIDS<-rbind(GABAPENTIN,PREGABALIN) 
rm(PREGABALIN,GABAPENTIN)
GABAPENTINOIDS<-GABAPENTINOIDS[,.(person_id,date)]

GABAPENTINOIDS[,date2:=date]
setkeyv(GABAPENTINOIDS,colnames(GABAPENTINOIDS))

setkeyv(person_spells_first_dispensation,c("person_id","entry_spell_category","exit_spell_category"))

select_firstspell_containing_disp<-unique(foverlaps(person_spells_first_dispensation,GABAPENTINOIDS))
rm(GABAPENTINOIDS)

setorderv(select_firstspell_containing_disp,c("person_id","entry_spell_category"))
select_firstspell_containing_disp<-select_firstspell_containing_disp[,min_spell_start:=min(entry_spell_category),by="person_id"]

select_firstspell_containing_disp<-select_firstspell_containing_disp[,is_the_study_spell:=data.table::fifelse(!is.na(date) & entry_spell_category==min_spell_start, 1,0)]

select_firstspell_containing_disp<-unique(select_firstspell_containing_disp[,date2:=NULL][,min_spell_start:=NULL])

# select_firstspell_containing_disp<-select_firstspell_containing_disp[selected_spell==1,]

person_spell<-merge(person_spell,select_firstspell_containing_disp,by=c("person_id","entry_spell_category","exit_spell_category"),all.x = T)

person_spell<-person_spell[is.na(is_the_study_spell),is_the_study_spell:=0]
# 
nameoutput1 <- "D3_clean_spells"
assign(nameoutput1, person_spell)
save(nameoutput1, file = paste0(dirtemp, nameoutput1, ".RData"), list = nameoutput1)

nameoutput1 <- "select_firstspell_containing_disp"
assign(nameoutput1, person_spell)
save(nameoutput1, file = paste0(dirtemp, nameoutput1, ".RData"), list = nameoutput1)






