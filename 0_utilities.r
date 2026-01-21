# 0_utilities.r for Peraza. Evolving Outcomes in Geriatric Patients with Orthopedic Polyfractures: the NTDB 2008-2023  
# Written by Vitto Resnick

# Establish initial utilities

# ==== Install & Load Libraries ==== 
# Make sure to install and/or load in the following libraries.

#install.packages('bit64')
#install.packages(
#  "https://cran.r-project.org/src/contrib/icd.data_1.0.tar.gz",
#  repos = NULL,
#  type = "source"
#)
#install.packages('devtools')
#install.packages("readr")

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)
library(cobalt)
library(WeightIt)
library(survey)
library(bit64)
library(cowplot)
library(forcats)
library(patchwork)
library(FactoMineR)
library(factoextra)
library(devtools)
library(cobalt)   # love.plot + bal.tab
library(scales)
library(margins)
library(PSweight)
library(readr)
library(icd.data)
data("icd9cm_hierarchy", package = "icd.data")
data("icd10cm2016", package = "icd.data")

set.seed(1) #for MCA

# ==== Define any lists (in order of use) ====
negatives = c("no","0","false","")

# All logical columns, complications, comorbidities, outcomes
logi_cols <- c(
  "Acute_Kidney_Injury","Acute_Lung_Injury_ARDS","Alcohol_Use_Disorder_Alcoholism",
  "Drug_or_Alcohol_Withdrawal_Syndrome","Bleeding_Disorder","Cardiac_Arrest_with_Resuscitative_Efforts",
  "CLABSI","Chronic_Renal_Failure_CKD","Cirrhosis_","Congenital_Anomalies","Congestive_Heart_Failure_CHF",
  "Current_Smoker","Currently_Receiving_Chemotherapy_for_Cancer","Deep_SSI",
  "Deep_Vein_Thrombosis_DVT_Thrombophlebitis","Dementia_","Diabetes_Mellitus","Disseminated_Cancer",
  "Extremity_Compartment_Syndrome","Organ_Space_SSI","Osteomyelitis_","Prematurity_37_Weeks",
  "Pulmonary_Embolism","Severe_Sepsis","Steroid_Use","Stroke_CVA",
  "Superficial_Incisional_SSI","Unplanned_ICU","Unplanned_Intubation","Unplanned_OR",
  "Functionally_Dependent_Health_Status","Hypertension_Requiring_Medication",
  "Long_ICU_LOS_3d","Long_LOS_to_Final_Discharge_5d"
)

# All overlapped comorbidities
comorbidities <- c(
  "Alcohol_Use_Disorder_Alcoholism",
  "Bleeding_Disorder",
  "Currently_Receiving_Chemotherapy_for_Cancer",
  "Congenital_Anomalies",
  "Congestive_Heart_Failure_CHF",
  "Current_Smoker",
  "Chronic_Renal_Failure_CKD",
  "CVA",
  "Diabetes_Mellitus",
  "Disseminated_Cancer",
  "Functionally_Dependent_Health_Status",
  "History_Angina_Pectoris",
  "History_MI_pc",
  "History_PVD",
  "Hypertension_Requiring_Medication",
  "Prematurity_37_Weeks",
  "Respiratory_COPD",
  "Steroid_Use",
  "Cirrhosis_",
  "Dementia_",
  "Substance_Abuse_Disorder",
  "Other_Comorbidity"
)

# All complications, overlapped and non-overlapping
complications <- c(
  #Overlapped
  "Acute_Kidney_Injury","Acute_Lung_Injury_ARDS",
  "Drug_or_Alcohol_Withdrawal_Syndrome",
  "Bleeding_Disorder",
  "Cardiac_Arrest_with_Resuscitative_Efforts",
  "CLABSI","Deep_SSI","Deep_Vein_Thrombosis_DVT_Thrombophlebitis",
  "MI_hc",
  "Extremity_Compartment_Syndrome","Organ_Space_SSI","Osteomyelitis_","Other_Hospital_Complication",
  "Pulmonary_Embolism",#"Pneumonia_",
  "Pressure_Ulcer","Severe_Sepsis","Stroke_CVA",
  "Superficial_Incisional_SSI","UTI","Unplanned_ICU","Unplanned_Intubation","Unplanned_OR",
  #Non-overlapping
  "Any.Complication",
  "Abdominal_Compartment_Syndrome",
  "Abdominal_fascia_left_open",
  "Base_deficit",
  "Bleeding_",
  "Coagulopathy_",
  "Coma_",
  "Graft_prosthesis_flap_failure",
  "Intracranial_pressure", 
  "Systemic_Sepsis",
  "Wound_Disruption",
  "Delirium_")

# Complications only in dataset 1
dt1_only_hc = c("Abdominal_Compartment_Syndrome",
                "Abdominal_fascia_left_open",
                "Base_deficit",
                "Bleeding_",
                "Coagulopathy_",
                "Coma_",
                "Graft_prosthesis_flap_failure",
                "Intracranial_pressure", 
                "Systemic_Sepsis",
                "Wound_Disruption",
                "Pneumonia_")
# Complications only in dataset 2
dt2_only_hc = c("Delirium_",
                "Ventilator_Pneumonia")

# Mapping of ICD 10 codes to fracture localizations
LE_10_fracture_map <- c(
  "S32.3"  = "ilium",
  "S32.4"  = "acetabulum",
  "S32.5"  = "pubis",
  "S32.6"  = "ischium",
  "S32.81" = "pelvis_w_dis",
  "S32.82" = "pelvis_wo_dis",
  "S32.83"  = "other_pelvis",
  "S32.84"  = "other_pelvis",
  "S32.85"  = "other_pelvis",
  "S32.86"  = "other_pelvis",
  "S32.87"  = "other_pelvis",
  "S32.88" = "other_pelvis",
  "S32.89" = "other_pelvis",
  "S32.9"  = "other_pelvis",
  "S32.8"  = "other_pelvis",
  
  "S72.0"   = "femoral_neck_head",
  "S72.10"  = "femur_tro_apophyseal",
  "S72.11"  = "femur_tro_apophyseal",
  "S72.12"  = "femur_tro_apophyseal",
  "S72.13"  = "femur_tro_apophyseal",
  "S72.14"  = "femur_intertro",
  "S72.19"  = "femur_tro_apophyseal",###Not sure (6 patients!)
  
  "S72.2"   = "femur_subtro",
  "S72.3"   = "femoral_shaft",
  "S72.4"   = "femur_lowerend",
  "S72.8"   = "femur_other",
  "S72.9"   = "femur_other",
  
  
  "S82.0"  = "patella",
  "S82.1"  = "tibia_fibula", #tibia
  "S82142A"= "tibia_fibula", #tibia
  "S82.2"  = "tibia_fibula",  #tibia
  "S82.3"  = "tibia_fibula",  #tibia
  "S82.4"  = "tibia_fibula", #fibula
  "S82.5"  = "ankle",
  "S82.6"  = "ankle",
  #No 82.7
  "S82.81" = "tibia_fibula", #fibula
  "S82.82" = "tibia_fibula", #fibula
  "S82.83" = "tibia_fibula", #fibula
  "S82.84" = "ankle",
  "S82.85" = "ankle",
  "S82.86" = "tibia_fibula", #fibula #Maisonneuve's fracture
  "S82.87" = "tibia_fibula",  #tibia. #pilon fracture of tibia
  "S82.88" = "other_LE",
  "S82.89" = "other_LE",
  "S82.9"  = "other_LE",
  
  "S89.0"  = "tibia_fibula",#tibia
  "S89.1"  = "tibia_fibula",#tibia
  "S89.2"  = "tibia_fibula",#fibula
  "S89.3"  = "tibia_fibula",#fibula
  
  "S89.8"  = "other_LE",
  "S89.9"  = "other_LE",
  
  "S92"    = "foot_toe",
  
  "S42.0"  = "clavicle",
  "S42.1"  = "scapula",
  "S42.2"  = "humerus",
  "S42.3"  = "humerus",
  "S42.4"  = "humerus",
  "S52"    = "radius_ulna",
  "S62"    = "carpal",
  "S42.9" = "other_UE"
)

# Mapping fracture localizations to new columns
name_map <- c(
  acetabulum    = "LE_acetabulum",
  pubis         = "LE_pubis",
  ilium         = "LE_ilium",
  ischium       = "LE_ischium",
  pelvis_w_dis  = "LE_pelvis_w_dis",
  pelvis_wo_dis = "LE_pelvis_wo_dis",
  other_pelvis  = "LE_other_pelvis",
  
  femoral_neck_head  = "LE_FNeckHead",
  femur_tro_apophyseal ="LE_FTroApo",
  femur_intertro  = "LE_FIntertro",
  femur_subtro  = "LE_FSubtro",
  femoral_shaft  = "LE_FShaft",
  femur_lowerend  = "LE_FLowerEnd",
  femur_other = "LE_FOther",
  
  patella       = "LE_patella",
  tibia_fibula  = "LE_tibia_fibula",
  ankle         = "LE_ankle",
  foot_toe      = "LE_foot_toe",
  other_LE      = "LE_other",
  
  clavicle      = "UE_clavicle",
  scapula       = "UE_scapula",
  humerus       = "UE_humerus",
  radius_ulna   = "UE_radius_ulna",
  carpal        = "UE_carpal",
  other_UE      = "UE_other"
)

# Columns to keep in final stitched dataset (in order)
keep_cols_1 <- c(
  "inc_key","excluded","exclusion_reason",
  "Guidelines","PreDataset","PostDataset",
  "n_LE","n_UE",#"has_827",
  "New_InjType",
  
  "Age","AgeGroup","Geri","SuperGeri",
  "Sex","Male","Female",
  "Race","White","Black","Other_Race",
  "Hispanic_Ethnicity",
  "Primary_Payment_Method","Gov_Care_Caid","Private_Ins","No_Insurance","Unknown_Pay",
  
  "Alcohol_Use_Disorder_Alcoholism","Bleeding_Disorder","Currently_Receiving_Chemotherapy_for_Cancer",
  "Congenital_Anomalies","Congestive_Heart_Failure_CHF","Current_Smoker","Chronic_Renal_Failure_CKD",
  "CVA","Diabetes_Mellitus","Disseminated_Cancer","Functionally_Dependent_Health_Status",
  "History_Angina_Pectoris","History_MI_pc","History_PVD","Hypertension_Requiring_Medication",
  "Prematurity_37_Weeks","Respiratory_COPD","Steroid_Use","Cirrhosis_","Dementia_",
  "Substance_Abuse_Disorder","Other_Comorbidity","mFI_5",
  
  "Mechanism","Primary_ECode","ECode_Description",
  "Fall","Ground_level_fall","MVT","MVT_Motorcyclist","MVT_Occupant","MVT_Other",
  "MVT_Pedal_cyclist","MVT_Pedestrian","MVT_Unspecified","Machinery","Struck_by_against",
  "Pedal_cyclist_other","Pedestrian_other","Transport_other","Mech_Other",
  
  "LE_Dcode1","injury_location","LE_acetabulum","LE_pubis","LE_ilium","LE_ischium",
  "LE_pelvis_w_dis","LE_pelvis_wo_dis","LE_other_pelvis",
  "LE_femur","LE_FNeckHead","LE_FTroApo","LE_FIntertro","LE_FSubtro",
  "LE_FShaft","LE_FLowerEnd","LE_FOther",
  "LE_patella","LE_tibia_fibula","LE_foot_toe","LE_ankle","LE_other",
  "UE_Dcode1","UE_clavicle","UE_scapula","UE_humerus","UE_radius_ulna","UE_carpal","UE_other",
  
  "ISS_raw","ISS","ISS_sub15","ISS_16_25","ISS_over25","ISS_unknown",
  "GCS_raw","GCS","GCS_sub8","GCS_9_12","GCS_13_15","GCS_unknown",
  "ACS_Verification_Level","ACS_1","ACS_2","ACS_34","ACS_unknown",
  
  "Death",
  "Hospital_Discharge_Disposition","Discharge_Labels","Discharge_Rehab", #"Discharge_Another_Hospital",
  "Discharge_SNF","Discharge_ICF","Discharge_Home","Discharge_Other","Discharge_Unknown",
  
  "Long_ICU_LOS_3d","Long_LOS_to_Final_Discharge_5d",
  
  "Any_Complication","Num_Complication",
  "Acute_Kidney_Injury","Acute_Lung_Injury_ARDS",
  "Cardiac_Arrest_with_Resuscitative_Efforts","Pressure_Ulcer","Deep_SSI",
  "Drug_or_Alcohol_Withdrawal_Syndrome","Deep_Vein_Thrombosis_DVT_Thrombophlebitis",
  "Extremity_Compartment_Syndrome","MI_hc","Organ_Space_SSI",#"Pneumonia_",
  "Pulmonary_Embolism",
  "Stroke_CVA","Superficial_Incisional_SSI","Unplanned_Intubation","UTI","CLABSI",
  "Osteomyelitis_","Unplanned_OR","Unplanned_ICU","Severe_Sepsis","Other_Hospital_Complication","Other_Complication_nMissing",
  
  "Abdominal_Compartment_Syndrome","Abdominal_fascia_left_open","Base_deficit",
  "Bleeding_","Coagulopathy_","Coma_","Graft_prosthesis_flap_failure",
  "Intracranial_pressure","Systemic_Sepsis","Wound_Disruption",
  "Pneumonia_",
  
  "Delirium_",  "Ventilator_Pneumonia"
)






# For Table 1 and 2: variable levels with unique columns, lacking unknowns
variables_tables <- list(
  "Geri","SuperGeri",
  "Female", "Male", 
  "Black","White","Other_Race","Hispanic_Ethnicity",
  "Alcohol_Use_Disorder_Alcoholism","Bleeding_Disorder","Currently_Receiving_Chemotherapy_for_Cancer",
  "Chronic_Renal_Failure_CKD","Cirrhosis_","Congenital_Anomalies","Congestive_Heart_Failure_CHF",
  "CVA","Dementia_","Diabetes_Mellitus","Disseminated_Cancer","Functionally_Dependent_Health_Status",
  "History_Angina_Pectoris","History_MI_pc","History_PVD","Hypertension_Requiring_Medication",
  "Prematurity_37_Weeks","Respiratory_COPD","Current_Smoker","Steroid_Use",
  "Substance_Abuse_Disorder","Other_Comorbidity",
  #"ISS_sub15","ISS_16_25","ISS_over25",
  #"GCS_sub8","GCS_9_12","GCS_13_15",
  "Death","Discharge_Home","Discharge_SNF","Discharge_Rehab",
  "Discharge_ICF","Discharge_Other","Discharge_Unknown",
  "Long_ICU_LOS_3d","Long_LOS_to_Final_Discharge_5d",
  "Any_Complication",
  "Abdominal_Compartment_Syndrome","Abdominal_fascia_left_open",
  "Acute_Kidney_Injury","Acute_Lung_Injury_ARDS",
  "Base_deficit","Bleeding_",
  "Cardiac_Arrest_with_Resuscitative_Efforts","CLABSI",
  "Coagulopathy_","Coma_",
  "Deep_SSI","Deep_Vein_Thrombosis_DVT_Thrombophlebitis","Delirium_",
  "Drug_or_Alcohol_Withdrawal_Syndrome","Extremity_Compartment_Syndrome",
  "Graft_prosthesis_flap_failure","Intracranial_pressure",
  "MI_hc","Organ_Space_SSI","Osteomyelitis_","Pneumonia_",
  "Pressure_Ulcer","Pulmonary_Embolism",
  "Severe_Sepsis","Stroke_CVA","Superficial_Incisional_SSI","Systemic_Sepsis","Unplanned_Intubation",
  "Unplanned_ICU","Unplanned_OR","UTI","Ventilator_Pneumonia",
  "Wound_Disruption","Other_Hospital_Complication"
)

# Table 1: Comorbidity columns
comorbs_cols <- c(
  "Alcohol_Use_Disorder_Alcoholism","Bleeding_Disorder",
  "Currently_Receiving_Chemotherapy_for_Cancer",
  "Chronic_Renal_Failure_CKD","Cirrhosis_","Congenital_Anomalies",
  "Congestive_Heart_Failure_CHF","CVA","Dementia_",
  "Diabetes_Mellitus","Disseminated_Cancer",
  "Functionally_Dependent_Health_Status",
  "History_Angina_Pectoris","History_MI_pc","History_PVD",
  "Hypertension_Requiring_Medication","Prematurity_37_Weeks",
  "Respiratory_COPD","Current_Smoker","Steroid_Use",
  "Substance_Abuse_Disorder","Other_Comorbidity"
)

# Table 1: Mechanisms
variables_mech <- list(
  "Fall","Ground_level_fall","MVT","MVT_Motorcyclist","MVT_Occupant","MVT_Other",
  "MVT_Pedal_cyclist","MVT_Pedestrian","MVT_Unspecified","Machinery","Struck_by_against",
  "Pedal_cyclist_other","Pedestrian_other","Transport_other","Mech_Other")

# Table 2: Non-mutually exclusive outcome columns
table2b_cols = c("Long_ICU_LOS_3d","Long_LOS_to_Final_Discharge_5d",
                 "Any_Complication",
                 "Abdominal_Compartment_Syndrome","Abdominal_fascia_left_open",
                 "Acute_Kidney_Injury","Acute_Lung_Injury_ARDS",
                 "Base_deficit","Bleeding_",
                 "Cardiac_Arrest_with_Resuscitative_Efforts","CLABSI",
                 "Coagulopathy_","Coma_",
                 "Deep_SSI","Deep_Vein_Thrombosis_DVT_Thrombophlebitis","Delirium_",
                 "Drug_or_Alcohol_Withdrawal_Syndrome","Extremity_Compartment_Syndrome",
                 "Graft_prosthesis_flap_failure","Intracranial_pressure",
                 "MI_hc","Organ_Space_SSI","Osteomyelitis_","Pneumonia_","Pressure_Ulcer","Pulmonary_Embolism",
                 "Severe_Sepsis","Stroke_CVA","Superficial_Incisional_SSI","Systemic_Sepsis","Unplanned_Intubation",
                 "Unplanned_ICU","Unplanned_OR","UTI","Ventilator_Pneumonia",
                 "Wound_Disruption","Other_Hospital_Complication")

# Covariates
covariate_names_full <- c(
  #Demographics:
  "AgeGroup","Sex","Race","Hispanic_Ethnicity","Primary_Payment_Method",
  
  #Trauma:
  "Mechanism","New_InjType","ISS","GCS","ACS_Verification_Level",
  
  #Comorbidities:
  "Alcohol_Use_Disorder_Alcoholism",
  "Bleeding_Disorder",
  "Currently_Receiving_Chemotherapy_for_Cancer",
  "Congenital_Anomalies",
  "Congestive_Heart_Failure_CHF",
  "Current_Smoker",
  "Chronic_Renal_Failure_CKD",
  "CVA",
  "Diabetes_Mellitus",
  "Disseminated_Cancer",
  "Functionally_Dependent_Health_Status",
  "History_Angina_Pectoris",
  "History_MI_pc",
  "History_PVD",
  "Hypertension_Requiring_Medication",
  "Prematurity_37_Weeks",
  "Respiratory_COPD",
  "Steroid_Use",
  "Cirrhosis_",
  "Dementia_",
  "Substance_Abuse_Disorder",
  "Other_Comorbidity"
)

# Demographic stratifying variables for regression
demo_vars_reg <- c(
  "AgeGroup",
  "Sex",
  "Race",
  "Hispanic_Ethnicity",
  "Primary_Payment_Method"
)

# Primary outcome variables for regression
outcome_vars_reg <- c(
  "Death",
  "Long_ICU_LOS_3d",
  "Long_LOS_to_Final_Discharge_5d",
  "Any_Complication"
)

# Individual complications for regression
complications_reg <- c(
  "Any_Complication",
  "Acute_Kidney_Injury","Acute_Lung_Injury_ARDS",
  "Cardiac_Arrest_with_Resuscitative_Efforts","Pressure_Ulcer","Deep_SSI",
  "Drug_or_Alcohol_Withdrawal_Syndrome","Deep_Vein_Thrombosis_DVT_Thrombophlebitis",
  "Extremity_Compartment_Syndrome","MI_hc","Organ_Space_SSI",#"Pneumonia_",
  "Pulmonary_Embolism",
  "Stroke_CVA","Superficial_Incisional_SSI","Unplanned_Intubation","UTI","CLABSI",
  "Osteomyelitis_","Unplanned_OR","Unplanned_ICU","Severe_Sepsis","Other_Complication_nMissing"
)

injury_levels <- c("1L", "1L1U", "1LMU", "ML", "ML1U", "MLMU")

# ==== Define any functions (in order of use) ====

coerce_logi <- function(D, cols) {
  exist <- intersect(cols, names(D))
  if (length(exist)) D[, (exist) := lapply(.SD, as.logical), .SDcols = exist]
  D
}

map_injury_location <- function(code, map = LE_10_fracture_map) {
  code <- as.character(code)
  n <- length(code)
  out <- rep(NA_character_, n)
  if (!n) return(out)
  
  # Trim/empty handling
  nz <- !is.na(code) & nzchar(code)
  if (!any(nz)) return(out)
  
  # --- ICD-10 "S..." branch (vectorized longest-prefix) ---
  is_S <- nz & startsWith(code, "S")
  if (any(is_S)) {
    codes <- names(map)
    ord   <- order(nchar(codes), decreasing = TRUE)
    codes <- codes[ord]
    labels <- unname(map[ord])
    
    # fill longest prefix first
    for (i in seq_along(codes)) {
      m <- is_S & is.na(out) & startsWith(code, codes[i])
      if (any(m)) out[m] <- labels[i]
      if (all(!is_S | !is.na(out))) break
    }
  }
  
  # --- ICD-9 numeric branch (fully vectorized) ---
  is_num <- nz & is.na(out) & !is_S
  if (any(is_num)) {
    x  <- code[is_num]
    cc <- sub("^\\s+|\\s+$", "", x)
    #cc <- sub("^([0-9]+\\.?[0-9]*).*", "\\1", cc)
    #cc <- sub("\\.?0+$", "", cc)
    cc <- sub("^([0-9]+\\.?[0-9]*).*", "\\1", cc)
    cc <- sub("\\.0+$", "", cc)
    
    num <- suppressWarnings(as.numeric(cc))
    
    lab <- rep(NA_character_, length(x))
    
    # pelvis special subcodes within 808.[*]
    in_808 <- !is.na(num) & num >= 808 & num < 809
    if (any(in_808)) {
      cc808 <- cc[in_808]
      lab[in_808 & cc808 %in% c("808","808.0","808.1")] <- "acetabulum"
      lab[in_808 & cc808 %in% c("808.2","808.3")]       <- "pubis"
      lab[in_808 & cc808 %in% c("808.41","808.51")]     <- "ilium"
      lab[in_808 & cc808 %in% c("808.42","808.52")]     <- "ischium"
      lab[in_808 & cc808 %in% c("808.43","808.53")]     <- "pelvis_w_dis"
      lab[in_808 & cc808 %in% c("808.44","808.54")]     <- "pelvis_wo_dis"
      lab[in_808 & is.na(lab)]                          <- "other_pelvis"
    }
    
    pick <- function(cond, val) { i <- which(cond & is.na(lab)); if (length(i)) lab[i] <<- val }
    pick(!is.na(num) & num >= 810   & num < 811,   "clavicle")
    pick(!is.na(num) & num >= 811   & num < 812,   "scapula")
    pick(!is.na(num) & num >= 812   & num < 813,   "humerus")
    pick(!is.na(num) & num >= 813   & num < 814,   "radius_ulna")
    pick(!is.na(num) & num >= 814   & num < 815,   "carpal")
    pick(!is.na(num) & num >= 815   & num < 819.9, "other_UE")
    
    pick(!is.na(num) & num >= 820   & num < 821.2,   "femoral_neck_head")
    
    pick(!is.na(num) & num %in% c(820.2, 820.3, 820.20, 820.30), "femur_tro_apophyseal")
    pick(!is.na(num) & num %in%                c(820.21, 820.31), "femur_intertro")
    pick(!is.na(num) & num %in%                c(820.22, 820.32), "femur_subtro")
    
    pick(!is.na(num) & num %in% c(821.00, 821.10), "femur_other")
    pick(!is.na(num) & num %in% c(821.01, 821.11), "femoral_shaft")
    pick(!is.na(num) & num >= 821.2   & num < 821.4,   "femur_lowerend")
    
    pick(!is.na(num) & num >= 822   & num < 823,   "patella")
    pick(!is.na(num) & num >= 823   & num < 824,   "tibia_fibula")
    pick(!is.na(num) & num >= 824   & num < 825,   "ankle")
    pick(!is.na(num) & num >= 825   & num < 827,   "foot_toe")
    pick(!is.na(num) & num >= 827   & num < 829.99,"other_LE")
    
    out[is_num] <- lab
  }
  out
}

normalize_icd <- function(x) {
  x <- toupper(trimws(x))
  x <- gsub("[^A-Z0-9]", "", x)
  ifelse(x == "", NA_character_, x)
}




icd9_candidates <- function(code) {
  s <- normalize_icd(code)
  if (is.na(s)) return(character())
  
  out <- c(s)
  
  if (grepl("^[0-9]", s)) {
    out <- c(out, substr(s, 1, 3))
  } else if (startsWith(s, "V")) {
    out <- c(out, substr(s, 1, 3))
  } else if (startsWith(s, "E")) {
    if (nchar(s) >= 4) {
      out <- c(out, substr(s, 1, 4))
    } else if (nchar(s) >= 3) {
      out <- c(out, substr(s, 1, 3))
    }
  }
  
  unique(out)
}
lookup_icd9 <- function(code) {
  for (cand in icd9_candidates(code)) {
    if (!is.na(icd9_map[cand])) {
      return(icd9_map[cand])
    }
  }
  NA_character_
}


lookup_icd10_truncate <- function(code) {
  s <- normalize_icd(code)
  if (is.na(s)) return(NA_character_)
  
  # Iteratively truncate from right
  while (nchar(s) >= 3) {
    if (!is.na(icd10_map[s])) {
      return(icd10_map[s])
    }
    s <- substr(s, 1, nchar(s) - 1)
  }
  
  NA_character_
}

detect_icd_version <- function(code) {
  if (is.na(code)) return(NA_character_)
  c0 <- substr(code, 1, 1)
  if (c0 %in% c("E", "V") || grepl("^[0-9]", c0)) "ICD9" else "ICD10"
}



icd_to_description <- function(code) {
  if (is.na(code)) return(list(NA, NA))
  
  ver <- detect_icd_version(code)
  
  if (ver == "ICD9") {
    desc <- lookup_icd9(code)
    return(list("ICD9", desc))
  }
  
  if (ver == "ICD10") {
    # exact or truncated ICD-10
    desc <- lookup_icd10_truncate(code)
    if (!is.na(desc)) {
      return(list("ICD10", desc))
    }
    
    # fallback in case misclassified
    desc9 <- lookup_icd9(code)
    if (!is.na(desc9)) {
      return(list("ICD9", desc9))
    }
    
    return(list("ICD10", NA))
  }
  
  list(NA, NA)
}

extract_long_icd <- function(dt, id_col) {
  rbindlist(list(
    dt[, .(id = get(id_col),
           ICD = unlist(strsplit(as.character(LE_Dcode), ","))),
       by = seq_len(nrow(dt))][, seq_len := NULL],
    
    dt[, .(id = get(id_col),
           ICD = unlist(strsplit(as.character(UE_Dcode), ","))),
       by = seq_len(nrow(dt))][, seq_len := NULL]
  ), use.names = TRUE, fill = TRUE)[
    !is.na(ICD)
  ][, ICD := trimws(ICD)][nzchar(ICD)]
}


clean_code <- function(code) {
  code <- as.character(code)
  code <- toupper(trimws(code))
  code[code %in% c("", "NA", "NAN", "NONE")] <- NA_character_
  code <- gsub("[^A-Z0-9\\.]", "", code)
  code[code == ""] <- NA_character_
  code
}


fmt <- function(x){ 
  if(x>9999){formatC(x, format = "d", big.mark = " ")} 
  else{x}}

fmt_freq <- function(n, total) {
  pct <- round(100 * n / total, 1)
  paste0(fmt(n), " / ", fmt(total), " (", pct, ")")
}


var_wise_Chi <- function(v) {
  tab <- matrix(
    c(dt[Guidelines=="Pre"  & get(v)==TRUE,  .N],
      dt[Guidelines=="Post" & get(v)==TRUE,  .N],
      dt[Guidelines=="Pre"  & get(v)==FALSE, .N],
      dt[Guidelines=="Post" & get(v)==FALSE, .N]
    ),nrow = 2, byrow = TRUE
  )
  
  rownames(tab) <- c("Yes", "No")
  colnames(tab) <- c("Pre", "Post")
  
  test <- tryCatch(
    chisq.test(tab),
    warning = function(w) chisq.test(tab, simulate.p.value = TRUE),
    error   = function(e) fisher.test(tab)
  )
  
  data.table(Variable   = v,
             Pre_Yes    = tab["Yes","Pre"],Post_Yes   = tab["Yes","Post"],
             Pre_No     = tab["No" ,"Pre"],Post_No    = tab["No" ,"Post"],
             Test       = test$method,Statistic  = unname(test$statistic),
             P_value    = test$p.value)
}

# Generate unadjusted logistic regression model
get_unadjusted_or <- function(data, outcome, treat_var) {
  ## Drop missing
  d <- data[!is.na(get(outcome)) & !is.na(get(treat_var))]
  
  ## Ensure factor + reference
  d[, (treat_var) := factor(get(treat_var))]
  d[, (treat_var) := relevel(get(treat_var), ref = "Pre")]
  
  ## Fit crude model
  fit <- glm(
    as.formula(paste(outcome, "~", treat_var)),
    data = d,
    family = binomial()
  )
  
  sm <- summary(fit)
  summary(fit)
  coef_name <- paste0(treat_var, "Post")
  
  est <- sm$coefficients[coef_name, "Estimate"]
  se  <- sm$coefficients[coef_name, "Std. Error"]
  z   <- sm$coefficients[coef_name, "z value"]
  p   <- sm$coefficients[coef_name, "Pr(>|z|)"]
  
  data.table(
    OR_unadj  = exp(est),
    LCL_unadj = exp(est - 1.96 * se),
    UCL_unadj = exp(est + 1.96 * se),
    p_unadj   = p
  )
}

# Generate covariate-adjusted logistic regression model with overlap weighting
run_ps_overlap_logit_psweight <- function(data,outcome,covariates,
                                          treat_var = "Guidelines",strat_var = "All",stratum   = "All"
) {
  data <- as.data.table(data)
  ## Drop missing treatment / outcome
  data <- data[!is.na(get(outcome)) & !is.na(get(treat_var))]
  
  ## Ensure treatment factor with Pre reference
  #data[, (treat_var) := factor(get(treat_var))]
  #data[, (treat_var) := relevel(get(treat_var), ref = "Pre")]
  
  unadj <- get_unadjusted_or(data, outcome, treat_var)
  
  ## Drop collapsed / rare covariates
  valid_covs <- covariates[sapply(covariates, function(v) {
    x <- data[[v]]
    length(unique(x[!is.na(x)])) >= 2})]
  
  #valid_covs <- keep_covariates_by_freq(
  #  data, valid_covs, min_prop = min_prop, verbose = verbose)
  
  ## PS formula
  #rhs_cov <- paste(sprintf("`%s`", valid_covs), collapse = " + ")
  
  valid_covs_safe <- make.names(valid_covs, unique = TRUE)
  setnames(data, valid_covs, valid_covs_safe)
  
  rhs_cov <- paste(valid_covs_safe, collapse = " + ")
  #ps_form <- as.formula(paste(treat_var, "~", rhs_cov))
  
  ps_form <- as.formula(paste(treat_var, "~", rhs_cov))
  
  #balance = SumStat(ps.formula = ps_form, data = data, weight = c("IPW", "overlap", "treated"))
  #plot(balance, type = "density")
  #plot(balance, type = "hist")
  #plot(balance, type = "balance", metric = "PSD")
  
  
  
  if (length(valid_covs) == 0) {
    message("No valid covariates for PS model: ",
            outcome, " | ", strat_var, " = ", stratum)
    return(NULL)
  }
  
  if (!inherits(ps_form, "formula")) {
    stop("ps_form is not a formula")
  }
  
  if (length(all.vars(ps_form)) < 2) {
    stop("Invalid PS formula: ", deparse(ps_form))
  }
  
  ## Run PSweight with overlap weights
  psw <- tryCatch(
    PSweight(
      ps.formula = ps_form, yname = outcome, data = data,
      weight     = "overlap",     # <-- OW / ATO
      family     = "binomial",    # binary outcome
      ps.method  = "glm"), error = function(e) e)
  if (inherits(psw, "error")) {
    message("PSweight failed for: ", outcome, " | ", strat_var, " = ", stratum)
    message(psw$message)
    return(NULL)
  }
  
  ## Extract causal OR: Post vs Pre
  summ <- summary(psw, contrast = c(1, -1),  # Pre -> Post
                  type = "OR",CI = TRUE)
  
  bal <- tryCatch(
    SumStat(
      ps.formula = ps_form,
      data       = data,
      weight     = "overlap",
      method  = "glm"
    ),
    error = function(e) {
      message("SumStat failed for: ", outcome, " | ", strat_var, " = ", stratum)
      message("Formula: ", deparse(ps_form))
      message(e$message)
      return(NULL)
    }
  )
  
  if (!is.null(bal)) {
    print(bal$summary$balance)
    print(bal$summary$overall)
  }
  
  logOR_est <- summ$estimates[1]
  se        <- summ$estimates[2]
  z         <- summ$estimates[3]
  logLCL    <- summ$estimates[4]
  logUCL    <- summ$estimates[5]
  
  pvl = 2*pnorm(q=abs(z),lower.tail=FALSE)
  
  cbind(data.table(
    StratVar = strat_var, Stratum = stratum, Outcome = outcome,
    Pre_Frequency  = fmt_freq(data[Guidelines=="Pre" &get(outcome)==TRUE, .N],data[Guidelines=="Pre", .N]),
    Post_Frequency = fmt_freq(data[Guidelines=="Post"&get(outcome)==TRUE,.N],data[Guidelines=="Post",.N])
  ),
  unadj,
  data.table(
    OR  = exp(logOR_est),
    LCL = exp(logLCL),
    UCL = exp(logUCL),
    p.value = pvl, 
    N = nrow(data),
    covariates_used = paste(valid_covs, collapse = ", "),
    Note = "Overlap-weighted OR (ATO)"))
}

# Significance thresholding for regression models 
apply_significance <- function(data) {
  data <- as.data.table(data)
  data[, sig := case_when( # Add significance stars used as `sig`
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE            ~ ""
  )]
  data[, q.value := p.adjust(p.value, method = "BH"), by = Outcome]
  data[, sig_fdr := case_when( # Significance stars based on FDR
    q.value < 0.001 ~ "***",
    q.value < 0.01  ~ "**",
    q.value < 0.05  ~ "*",
    TRUE            ~ ""
  )]
  data[, sig_class := fifelse(
    q.value < 0.001, "FDR-corrected P < 0.001",
    fifelse(p.value < 0.05, "FDR-corrected P < 0.05", "NS")
  )]
  data[, sig_class := factor(
    sig_class, levels = c("NS", "FDR-corrected P < 0.05", "FDR-corrected P < 0.001")
  )]
}

