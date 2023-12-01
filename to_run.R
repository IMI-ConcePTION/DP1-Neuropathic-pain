#-------------------------------
# Script for IMI ConcePTION Demonstration Projects 1 DU

# authors: Rosa Gini, Claudia Bartolini, Olga Paoletti, Davide Messina, Giorgio Limoncella
# based on previous scripts 





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
dirpregnancyinput <- "/home/rosgin/FEPI/ConcePTION/StudyScripts/pregnancy_20231031_v5.1.3/g_output/"


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


# CREATE EXCLUSION CRITERIA for persons/spells
if (!(thisdatasource %in% DAP_with_pregnancy_only)){
  launch_step("p_steps/01_T2_6_selection_criteria_from_PERSON_to_study_population.R")
  launch_step("p_steps/02_T3_1_create_study_population.R")
}


# CREATE EXCLUSION CRITERIA for persons/spells
launch_step("p_steps/01_T2_7_selection_criteria_from_PERSON_to_study_population_pregnancy.R")
launch_step("p_steps/02_T3_2_create_study_population_pregnancy.R")


#filter the dispensing according to inclusion criteria
launch_step("p_steps/03_T2_1_create_study_population_baseline_variables.R")


launch_step("p_steps/03_T2_2_pregnancy_characteristics_byexposurewindow.R")


