#-------------------------------
# Script for IMI ConcePTION Demonstration Projects 1

# authors: Rosa Gini, Claudia Bartolini, Olga Paoletti, Davide Messina, Giorgio Limoncella
# based on previous scripts 

# changelog v 2.2 , 17 November 2023:
#-update of the codelistaddition of ICD9 codes in the codelist
#-correction of createspells for FISABIO 

# changelog v 2.1 , 19 October 2023:
#-update of the codelist

# changelog v 2.0 , 07 September 2023:
#-update of the meanings list
#-implementation of the cube function to create summary tables describing the presence of small counts in all the possible aggregation levels 
#-addition of codes to identify component B (indication indicator) for UOSL

# changelog v 1.4 , 20 May 2023:
# - correction of duplicated rows for THL and additiion of detailed information on "more than one" category

# changelog v 1.3 , 22 March 2023:
# - addition of the validation algorithm (V1, V2 and V3)

# changelog v 1.2 , 10 March 2023:
# - debug of sensitivity analysisbased on DAP's reports

# changelog v 1.1 , 07 March 2023:
# - debug of Main analysis 

# First release
# v 1.0 -  20 February 2023


rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#-------------------------------
# @DAP: please modify the following parameter
# dirinput: set it to the directory where your CDM instance is stored
dirinput <-  c("/home/rosgin/FEPI/ConcePTION/CDMInstances/DataCharacterisationUpdate210928/")

# dirpregnancyinput: set it to the directory where your CDM instance is stored
#dirpregnancyinput <- "/home/rosgin/FEPI/ConcePTION/StudyScripts/ConcePTIONAlgorithmPregnancies-release_4.1/g_output/"
dirpregnancyinput <- "/home/rosgin/FEPI/ConcePTION/StudyScripts/pregnancy_20230825_v4.3/g_output/"


#----------------
#LOAD PARAMTETERS
#----------------

source(paste0(thisdir,"/p_parameters/01_parameters_program.R")) #GENERAL
source(paste0(thisdir,"/p_parameters/02_parameters_CDM.R")) #CDM
source(paste0(thisdir,"/p_parameters/03_concept_sets.R")) #CONCEPTSETS
source(paste0(thisdir,"/p_parameters/05_subpopulations_restricting_meanings.R")) 
source(paste0(thisdir,"/p_parameters/06_algorithms.R")) #ALGORITHMS
source(paste0(thisdir,"/p_parameters/07_study_design.R")) #STUDY DESIGN
source(paste0(thisdir,"/p_parameters/99_saving_all_parameters.R")) #SAVING AND CLEANING PARAMETERS


#----------------
# RUN STEPS
#----------------

# 01 RETRIEVE RECORDS FRM CDM

# CHECK CORRECT DATE OF BIRTH AND DEATH
launch_step("p_steps/01_T2_1_create_persons.R")

#COMPUTE SPELLS OF TIME FROM OBSERVATION_PERIODS
launch_step("p_steps/01_T2_2_apply_CreateSpells.R")

# APPLY THE FUNCTION CreateConceptSetDatasets TO CREATE ONE DATASET PER CONCEPT SET CONTAINING ONLY RECORDS WITH CODES OF INTEREST
launch_step("p_steps/01_T2_3_CreateConceptSetDatasets.R")

# CLEAN THE SPELLS
launch_step("p_steps/01_T2_4_clean_spells.R")

#filter the dispensing according to inclusion criteria
launch_step("p_steps/01_T2_5_select_dispensing.R")


# CREATE EXCLUSION CRITERIA for persons/spells
if (!(thisdatasource %in% DAP_with_pregnancy_only)){
  launch_step("p_steps/01_T2_6_selection_criteria_from_PERSON_to_study_population.R")
  launch_step("p_steps/02_T3_1_create_study_population.R")
}


# CREATE EXCLUSION CRITERIA for persons/spells
launch_step("p_steps/01_T2_7_selection_criteria_from_PERSON_to_study_population_pregnancy.R")
launch_step("p_steps/02_T3_2_create_study_population_pregnancy.R")



#create components---------------------
if (!(thisdatasource %in% DAP_with_pregnancy_only)){
  launch_step("/p_steps/03_T2_1_create_components.R")
}

launch_step("/p_steps/03_T2_2_create_components_pregnancy.R")

# #create components for the validation
# if (thisdatasource=="RDRU_FISABIO") {
#   launch_step("/p_steps/03_T2_3_create_components_pregnancy_validation_FISABIO.R")
# }

##### Create Algorithms-----------------
if (!(thisdatasource %in% DAP_with_pregnancy_only)){
  launch_step("/p_steps/04_T2_1_create_algorithms.R") 
}

launch_step("/p_steps/04_T2_2_create_algorithms_pregnancy.R")


##### Add validation algorithms for THL and SAIL-------------
if (thisdatasource=="SAIL Databank") {
  launch_step("/p_steps/05_T2_1_create_algorithms_validation.R") 
}


if (thisdatasource=="SAIL Databank" | thisdatasource=="THL") {
  launch_step("/p_steps/05_T2_2_create_algorithms_pregnancy_validation.R")
}

# if (thisdatasource=="RDRU_FISABIO") {
#   launch_step("/p_steps/05_T2_3_create_algorithms_pregnancy_validation_FISABIO.R") 
# }



##### mask small counts and store them in the folder g_export_smallcountsremoved
launch_step("/p_steps/06_T2_masking_small_counts.R")
