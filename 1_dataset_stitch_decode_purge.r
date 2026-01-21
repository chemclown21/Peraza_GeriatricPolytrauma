# 1_dataset_stitch_decode_purge.r for Peraza. Evolving Outcomes in Geriatric Patients with Orthopedic Polyfractures: the NTDB 2008-2023  
# Written by Vitto Resnick

# Dataset cleaning, stitching, decoding, purging via exclusion criteria

# ==== Loading Data ==== 

files_directory = '/Users/vresnick/Documents/GitHub/Peraza2026_GeriatricPolyTrauma'
setwd(files_directory)
outdir = "outputs/datasets" #Set directory for outputing files.

#Load in pre-revision dataset
dt1 <- fread("original_datasets/most_current_DBs/vitto_08_14.csv")
setnames(dt1, make.names(colnames(dt1))) #Converts spaces to periods

#Load in post-revision dataset
dt2 <- fread("original_datasets/most_current_DBs/vitto_17_23.csv")
setnames(dt2, make.names(colnames(dt2))) #Converts spaces to periods

#Read in this dictionary file for decoding mechanism numbers to strings
mech_dict <- fread("dicts/mech_dict.csv", colClasses = c(key = "double", value = "character"))
#i.e. call 3, for Fall

#print dimensions
dim(dt1)
head(dt1[,1:6])
dim(dt2)
head(dt2[,1:6])

# ==== Patient Demographics ==== 
# inc_key
dt1[, inc_key := as.character(inc_key)]
dt2[, inc_key := as.character(inc_key)]

# Which dataset is the patient from
dt1[, Guidelines := "Pre"]
dt2[, Guidelines := "Post"]
dt1[,PreDataset := Guidelines=="Pre"]
dt1[,PostDataset := Guidelines=="Post"]
dt2[,PreDataset := Guidelines=="Pre"]
dt2[,PostDataset := Guidelines=="Post"]

# Age
dt1[, Age := fifelse(age == -99, 90L, as.integer(age))] #convert -99 to 90 (90+)
dt2[, Age := fifelse(ageyears == -99, 90L, as.integer(ageyears))] #Note dt2 has no -99

# Sex
dt1[, Sex := fcase(SEX %in% c("Not Known BIU 2", "Not Known/Not Recorded BIU 2"),NA_character_, 
                   is.na(SEX), NA_character_,
                   default = as.character(SEX))]
dt2[, Sex := fcase(is.na(SEX), NA_character_,
                   SEX %in% c("Male",1,"1.0"),"Male", 
                   SEX %in% c("Female",2,"2.0"),"Female", 
                   SEX %in% c(3,"3.0"),"Nonbinary", 
                   SEX %in% c(""),NA_character_, 
                   default = as.character(SEX))]

dt1[, Male := Sex == "Male"]
dt1[, Female := Sex == "Female"]
dt2[, Male := Sex == "Male"]
dt2[, Female := Sex == "Female"]

# Race
dt2[, Race := fcase(Race.Complete %in% c("White, Hispanic","White, non-Hispanic"),"White", 
                    Race.Complete %in% c("Black or African American"),"Black", 
                    Race.Complete %in% c("Pacific Islander","American Indian","Unknown","Asian","Other"),"Other", 
                    default = "Missing!")]
dt1[, White := Race == "White"]
dt1[, Black := Race == "Black"]
dt1[, Other_Race := Race == "Other"]
dt2[, White := Race == "White"]
dt2[, Black := Race == "Black"]
dt2[, Other_Race := Race == "Other"]

# Hispanic_Ethnicity
dt1[, Hispanic_Ethnicity := fcase(Ethnicity %in% c("No"),FALSE,
                                  Ethnicity %in% c("Hispanic Ethnicity"),TRUE)]
dt2[, Hispanic_Ethnicity := fcase(Race.Complete %in% c("White, Hispanic"),TRUE, 
                                  default = FALSE)]

# ==== Injury ==== 
# ISS raw score
dt1[, ISS_raw := issais]
dt2[, ISS_raw := fcoalesce(iss, iss_05)]
# ISS ranges
dt1[, ISS := fcase(is.na(issais),"ISS unknown",
                   issais >= 1  & issais <= 15,"ISS <=15",
                   issais >= 16 & issais <= 25,"ISS 16-25",
                   issais > 25                ,"ISS>25",
                   default = "ISS unknown")]
dt2[, ISS := fcase(is.na(ISS_raw),"ISS unknown",
                   ISS_raw >= 1  & ISS_raw <= 15,"ISS <=15",
                   ISS_raw >= 16 & ISS_raw <= 25,"ISS 16-25",
                   ISS_raw >  25,                "ISS>25",
                   default = "ISS unknown")]

# GCS raw score
dt1[, GCS_raw := gcstot]
dt2[, GCS_raw := totalgcs]
# GCS ranges
dt1[, GCS := fcase(is.na(gcstot),"GCS unknown",
                   gcstot >= 3  & gcstot <= 8 ,"GCS <=8",
                   gcstot >= 9  & gcstot <= 12,"GCS 9-12",
                   gcstot >= 13 & gcstot <= 15, "GCS 13-15",
                   default = "GCS unknown")]
dt2[, GCS := fcase(is.na(totalgcs),"GCS unknown",
                   totalgcs >= 3  & totalgcs <= 8 ,"GCS <=8",
                   totalgcs >= 9  & totalgcs <= 12,"GCS 9-12",
                   totalgcs >= 13 & totalgcs <= 15, "GCS 13-15",
                   default = "GCS unknown")]

# Mechanism (MOI)
dt1[, Mechanism := fcase(
  grepl("^$", mechanism), "Unspecified",
  is.na(mechanism),"Unspecified",
  default = as.character(mechanism)   # otherwise copy original
)]

setkey(mech_dict, key)
dt2[mech_dict, Mechanism := i.value, on = .(mechanism = key)]
dt2[, Mechanism := fcase(
  is.na(mechanism),"Unspecified",
  default = as.character(Mechanism)   # otherwise copy original
)]

# ECodes
dt1[, Primary_ECode := ecode]
dt2[, Primary_ECode := primaryecodeicd10]
dt1[, ECode_Description := ecodedes]
dt2[, ECode_Description := ecode_desc]

# ==== Clinical Info ==== 
# ACS Level
dt1[, ACS_raw := ACSLEVEL]
dt2[, ACS_raw := VERIFICATIONLEVEL]


# Hospital_Discharge_Disposition
dt1[, Hospital_Discharge_Disposition := fcase(
  grepl("Expired", hospdisp, fixed = TRUE, ignore.case = TRUE),"Deceased/Expired",
  grepl("Discharged/Transferred to inpatient rehab or designated unit", hospdisp, fixed = TRUE, ignore.case = TRUE),           "Discharged/Transferred to inpatient rehab or designated unit",
  grepl("Discharged/Transferred to a short-term general hospital for inpatient", hospdisp, fixed = TRUE, ignore.case = TRUE),  "Discharged/Transferred to a short-term general hospital for inpatient care",
  grepl("Discharged/Transferred to Long Term Care Hospital", hospdisp, fixed = TRUE, ignore.case = TRUE),                      "Discharged/Transferred to Long Term Care Hospital (LTCH)",
  grepl("Discharged/transferred to a psychiatric hospital or psychiatric distin", hospdisp, fixed = TRUE, ignore.case = TRUE), "Discharged/Transferred to a psychiatric hospital or psychiatric distinct part unit of a hospital",
  grepl("Discharged/Transferred to Skilled Nursing Facility", hospdisp, fixed = TRUE, ignore.case = TRUE),                     "Discharged/Transferred to Skilled Nursing Facility (SNF)",
  grepl("Discharged/Transferred to an Intermediate Care Facility", hospdisp, fixed = TRUE, ignore.case = TRUE),                "Discharged/Transferred to an Intermediate Care Facility (ICF)",
  grepl("Discharged/Transferred to an Intermediate Care Facility (ICF)", hospdisp, fixed = TRUE, ignore.case = TRUE),          "Discharged/Transferred to an Intermediate Care Facility (ICF)",
  grepl("Discharge/Transferred to home under care of organized home health serv", hospdisp, fixed = TRUE, ignore.case = TRUE), "Discharged/Transferred to home under care of organized home health service",
  grepl("Discharged to home or self-care (routine discharge)", hospdisp, fixed = TRUE, ignore.case = TRUE),                    "Discharged to home or self-care (routine discharge)",
  grepl("Discharged/Transferred to hospice care", hospdisp, fixed = TRUE, ignore.case = TRUE),                                 "Discharged/Transferred to hospice care",
  grepl("Left against medical advice", hospdisp, fixed = TRUE, ignore.case = TRUE),                                            "Left against medical advice or discontinued care",
  grepl("Left against medical advice or discontinued care", hospdisp, fixed = TRUE, ignore.case = TRUE),                       "Left against medical advice or discontinued care",
  grepl("Discharged/Transferred to another type of institution not defined else", hospdisp, fixed = TRUE, ignore.case = TRUE), "Discharged/Transferred to another type of institution not defined elsewhere",
  grepl("Discharged/Transferred to court/law enforcement", hospdisp, fixed = TRUE, ignore.case = TRUE),                        "Discharged/Transferred to court/law enforcement.",
  grepl("Not Known BIU 2", hospdisp, fixed = TRUE, ignore.case = TRUE),                                                        "Not Known/Not Recorded BIU 2",
  grepl("Not Known/Not Recorded BIU 2", hospdisp, fixed = TRUE, ignore.case = TRUE),                                           "Not Known/Not Recorded BIU 2",
  default = as.character(hospdisp)   # otherwise copy original
)]

dt2[, Hospital_Discharge_Disposition := fcase(
  grepl("^$", hospdischargedisposition), "Not Known/Not Recorded BIU 2",
  grepl("\\<1\\.0\\>", hospdischargedisposition), "Discharged/Transferred to a short-term general hospital for inpatient care", #good
  grepl("\\<2\\.0\\>", hospdischargedisposition), "Discharged/Transferred to an Intermediate Care Facility (ICF)", #good
  grepl("\\<3\\.0\\>", hospdischargedisposition), "Discharged/Transferred to home under care of organized home health service", #good
  grepl("\\<4\\.0\\>", hospdischargedisposition), "Left against medical advice or discontinued care", #good
  grepl("\\<5\\.0\\>", hospdischargedisposition), "Deceased/Expired", #bad
  grepl("Deceased/expired", hospdischargedisposition, fixed = TRUE, ignore.case = TRUE), "Deceased/Expired",
  grepl("\\<6\\.0\\>", hospdischargedisposition), "Discharged to home or self-care (routine discharge)", #good
  grepl("\\<7\\.0\\>", hospdischargedisposition), "Discharged/Transferred to Skilled Nursing Facility (SNF)",#
  grepl("\\<8\\.0\\>", hospdischargedisposition), "Discharged/Transferred to hospice care",
  grepl("\\<10\\.0\\>", hospdischargedisposition), "Discharged/Transferred to court/law enforcement.",
  grepl("\\<11\\.0\\>", hospdischargedisposition), "Discharged/Transferred to inpatient rehab or designated unit",
  grepl("\\<12\\.0\\>", hospdischargedisposition), "Discharged/Transferred to Long Term Care Hospital (LTCH)",
  grepl("\\<13\\.0\\>", hospdischargedisposition), "Discharged/Transferred to a psychiatric hospital or psychiatric distinct part unit of a hospital",
  grepl("\\<14\\.0\\>", hospdischargedisposition), "Discharged/Transferred to another type of institution not defined elsewhere",
  default = as.character(hospdischargedisposition)   # otherwise copy original
)]

#Death
dt2[, Death := fcase(
  grepl("Deceased/Expired", Hospital_Discharge_Disposition), TRUE,
  default = FALSE)]

#Payment Method
dt1[, Primary_Payment_Method := fcase( #let's just add the word insurance and then it matches the new DB!
  grepl("Private/Commercial", payment,fixed = TRUE), "Private/Commercial Insurance",
  default = as.character(payment)   # otherwise copy original
)]

dt2[, Primary_Payment_Method := fcase(
  grepl("Medicare", primarymethodpayment,fixed = TRUE), "Medicare, Medicaid, or Government",
  grepl("Medicaid", primarymethodpayment,fixed = TRUE), "Medicare, Medicaid, or Government",
  grepl("Other Government", primarymethodpayment,fixed = TRUE), "Medicare, Medicaid, or Government",
  
  grepl("Private/Commercial Insurance", primarymethodpayment,fixed = TRUE), "Private/Commercial Insurance",
  #typo!!!
  grepl("Private/Commerical Insurance", primarymethodpayment,fixed = TRUE), "Private/Commercial Insurance",
  
  grepl("Self-Pay", primarymethodpayment,fixed = TRUE), "No insurance/self-pay",
  grepl("Not Billed (for any reason)", primarymethodpayment,fixed = TRUE), "No insurance/self-pay",
  
  grepl("Other", primarymethodpayment,fixed = TRUE), "Unknown Payer",
  grepl("^$", primarymethodpayment), "Unknown Payer",
  default = "uh oh!"   # otherwise copy original
)]
dt1[, Gov_Care_Caid := Primary_Payment_Method == "Medicare, Medicaid, or Government"]
dt1[, Private_Ins := Primary_Payment_Method == "Private/Commercial Insurance"]
dt1[, No_Insurance := Primary_Payment_Method == "No insurance/self-pay"]
dt1[, Unknown_Pay := Primary_Payment_Method == "Unknown Payer"]

dt2[, Gov_Care_Caid := Primary_Payment_Method == "Medicare, Medicaid, or Government"]
dt2[, Private_Ins := Primary_Payment_Method == "Private/Commercial Insurance"]
dt2[, No_Insurance := Primary_Payment_Method == "No insurance/self-pay"]
dt2[, Unknown_Pay := Primary_Payment_Method == "Unknown Payer"]



# Long_ICU_LOS_3d, prolonged ICU LOS
dt1[, Long_ICU_LOS_3d := fcase(icudays>3,TRUE,
                               is.na(icudays),FALSE, #Treat NA as 0
                               icudays %in% c(-2, "-2"), NA,
                               default = FALSE)]
#dt2[, Long_ICU_LOS_3d := fcase(totaliculos %in% c(">3d"),TRUE,default = FALSE)]
dt2[, Long_ICU_LOS_3d := fcase(totaliculos>3,TRUE,
                               is.na(totaliculos),FALSE, #Treat NA as 0
                               totaliculos %in% c(-2, "-2"), NA,
                               default = FALSE)]

#Long_LOS_to_Final_Discharge_5d, prolonged hospital LOS
dt1[, Long_LOS_to_Final_Discharge_5d := fcase(losdays>5,TRUE,
                                              is.na(losdays),FALSE, #Treat NA as 0
                                              losdays %in% c(-2, -29, "-2", "-29"), NA,
                                              default = FALSE)]
#dt2[, Long_LOS_to_Final_Discharge_5d := fcase(finaldischargedays %in% c(">5d"),TRUE,default = FALSE)]
dt2[, Long_LOS_to_Final_Discharge_5d := fcase(finaldischargedays>5,TRUE,
                                              is.na(finaldischargedays),FALSE, #Treat NA as 0
                                              finaldischargedays %in% c(-2, -29, "-2", "-29"), NA,
                                              default = FALSE)]




# ==== Comorbidities ==== 

#Comorbidity 1: Other_Comorbidity
dt1[, Other_Comorbidity := fcase(is.na(Other),FALSE,
                                 Other %in% c(1, "1"),TRUE,default = FALSE)]
dt2[, Other_Comorbidity := !tolower(as.character(Other_pc)) %in% c("","0","false","no")]


#Comorbidity 2: Alcohol_Use_Disorder_Alcoholism
dt1[, Alcohol_Use_Disorder_Alcoholism := fcase(is.na(Alcoholism),FALSE,
                                               Alcoholism %in% c(2, "2"),TRUE,default = FALSE)]
dt2[, Alcohol_Use_Disorder_Alcoholism := !tolower(as.character(Alcohol.Use.Disorder)) %in% c("","0","false","no")]

#dt1 #3: Ascities.within.30.days

#Comorbidity 4: Bleeding_Disorder
dt1[, Bleeding_Disorder := fcase(is.na(Bleeding.Disorder),FALSE,
                                 Bleeding.Disorder %in% c(4, "4"),TRUE, default = FALSE)]
dt2[, Bleeding_Disorder := !tolower(as.character(Bleeding.Disorder)) %in% c("","0","false","no")]

#Comorbidity 5: Currently_Receiving_Chemotherapy_for_Cancer
dt1[, Currently_Receiving_Chemotherapy_for_Cancer := fcase(is.na(Chemotherapy),FALSE,
                                                           Chemotherapy %in% c(5, "5"),TRUE, default = FALSE)]
dt2[, Currently_Receiving_Chemotherapy_for_Cancer := !tolower(as.character(Currently.Receiving.Chemotherapy.for.Cancer)) %in% c("","0","false")]

#Comorbidity 6: Congenital_Anomalies 
dt1[, Congenital_Anomalies := fcase(is.na(Congenital.Anomalies),FALSE,
                                    Congenital.Anomalies %in% c(6, "6"),TRUE, default = FALSE)]
dt2[, Congenital_Anomalies := fcase(Congenital.Anomalies %in% c(1, "1"),TRUE, default = FALSE)]

#Comorbidity 7: Congestive_Heart_Failure_CHF
dt1[, Congestive_Heart_Failure_CHF := fcase(is.na(CHF),FALSE,
                                            CHF %in% c(7, "7"),TRUE, default = FALSE)]
dt2[, Congestive_Heart_Failure_CHF := !tolower(as.character(Congestive.Heart.Failure)) %in% c("","0","false","no")]

#Comorbidity 8: Current_Smoker
dt1[, Current_Smoker := fcase(is.na(Smoker),FALSE,
                              Smoker %in% c(8, "8"),TRUE, default = FALSE)]
dt2[, Current_Smoker := !tolower(as.character(Current.Smoker)) %in% c("","0","false","no")]

#Comorbidity 9: Chronic_Renal_Failure_CKD
dt1[, Chronic_Renal_Failure_CKD := fcase(is.na(CKD),FALSE,
                                         CKD %in% c(9, "9"),TRUE, default = FALSE)]
dt2[, Chronic_Renal_Failure_CKD := !tolower(as.character(Chronic.Renal.Failure)) %in% c("","0","false","no")]

#Comorbidity 10: Cerebrovascular Accident (CVA) with residual neurological deficit
dt1[, CVA := fcase(is.na(CVA.residual.neuro.defect),FALSE,
                   CVA.residual.neuro.defect %in% c(10, "10"),TRUE, default = FALSE)]
dt2[, CVA := !tolower(as.character(Cerebrovascular.Accident)) %in% c("","0","false","no")]

#Comorbidity 11: Diabetes_Mellitus
dt1[, Diabetes_Mellitus := fcase(is.na(Diabetes.mellitus),FALSE,
                                 Diabetes.mellitus %in% c(11, "11"),TRUE, default = FALSE)]
dt2[, Diabetes_Mellitus := !tolower(as.character(Diabetes.Mellitus)) %in% c("","0","false","no")]

#Comorbidity 12: Disseminated_Cancer
dt1[, Disseminated_Cancer := fcase(is.na(Disseminated.cancer),FALSE,
                                   Disseminated.cancer %in% c(12, "12"),TRUE, default = FALSE)]
dt2[, Disseminated_Cancer := !tolower(as.character(Disseminated.Cancer)) %in% c("","0","false","no")]

#dt1 #13: Do.Not.Resuscitate..DNR..status
#dt1 #14: Esophageal.varies

#Comorbidity 15: Functionally_Dependent_Health_Status
dt1[, Functionally_Dependent_Health_Status := fcase(is.na(Functionaly.Dependent.health.status),FALSE,
                                                    Functionaly.Dependent.health.status %in% c(15, "15"),TRUE,default = FALSE)]
dt2[, Functionally_Dependent_Health_Status := !tolower(as.character(Functionaly.Dependent.Health.Status)) %in% c("no","0","false","")]


#Comorbidity 16: History_Angina_Pectoris
dt1[, History_Angina_Pectoris := fcase(is.na(History.of.angina.within.pas.1.month),FALSE,
                                       History.of.angina.within.pas.1.month %in% c(16, "16"),TRUE,default = FALSE)]
dt2[, History_Angina_Pectoris := !tolower(as.character(Angina.Pectoris)) %in% c("no","0","false","")]


#Comorbidity 17: History of Myocardial Infarction Comorbidity (History_MI_pc)
dt1[, History_MI_pc := fcase(is.na(History.of.myocardial.infarction),FALSE,
                             History.of.myocardial.infarction %in% c(17, "17"),TRUE,default = FALSE)]
dt2[, History_MI_pc := !tolower(as.character(Myocardial.Infarction_pc)) %in% c("no","0","false","")]

#Comorbidity 18: History of PVD
dt1[, History_PVD := fcase(is.na(History.of.PVD),FALSE,
                           History.of.PVD %in% c(18, "18"),TRUE,default = FALSE)]
dt2[, History_PVD := !tolower(as.character(Peripheral.Arterial.Disease)) %in% c("no","0","false","")]


#Comorbidity 19: Hypertension_Requiring_Medication
dt1[, Hypertension_Requiring_Medication := fcase(is.na(Hypertension.requiring.medication),FALSE,
                                                 Hypertension.requiring.medication %in% c(19, "19"),TRUE,default = FALSE)]
dt2[, Hypertension_Requiring_Medication := !tolower(as.character(Hypertension)) %in% c("no","0","false","")]

#dt1 #20: Impaired.sensorium

#Comorbidity 21: Prematurity_37_Weeks
dt1[, Prematurity_37_Weeks := fcase(is.na(Prematurity),FALSE,
                                    Prematurity %in% c(21, "21"),TRUE, default = FALSE)]
dt2[, Prematurity_37_Weeks := !tolower(as.character(Prematurity)) %in% c("no","0","false","")]

#dt1 #22: Obesity

#Comorbidity 23: Respiratory Disease & Chronic Obstructive Pulmonary Disease
dt1[, Respiratory_COPD := fcase(is.na(Respiratory.Disease),FALSE,
                                Respiratory.Disease %in% c(23, "23"),TRUE, default = FALSE)]
dt2[, Respiratory_COPD := !tolower(as.character(Chronic.Obstructive.Pulmonary.Disease)) %in% c("no","0","false","")]

#Comorbidity 24: Steroid_Use
dt1[, Steroid_Use := fcase(is.na(Steroid.Use),FALSE,
                           Steroid.Use %in% c(24, "24"),TRUE, default = FALSE)]
dt2[, Steroid_Use := !tolower(as.character(Steroid.Use)) %in% c("no","0","false","")]

#Comorbidity 25: Cirrhosis
dt1[, Cirrhosis_ := fcase(is.na(Cirrhosis),FALSE,
                          Cirrhosis %in% c(25, "25"),TRUE, default = FALSE)]
dt2[, Cirrhosis_ := !tolower(as.character(Cirrhosis)) %in% c("","0","false","no")]

#Comorbidity 26: Dementia
dt1[, Dementia_ := fcase(is.na(Dementia),FALSE,
                         Dementia %in% c(26, "26"),TRUE, default = FALSE)]
dt2[, Dementia_ := !tolower(as.character(Dementia)) %in% c("","0","false","no")]

#dt1 #27: Major.pyschiatric.illness

#Comorbidity 28: Substance_Abuse_Disorder 
dt1[, Substance_Abuse_Disorder := fcase(is.na(Drug.use.disorder),FALSE,
                                        Drug.use.disorder %in% c(28, "28"),TRUE, default = FALSE)]
dt2[, Substance_Abuse_Disorder := !tolower(as.character(Substance.Abuse.Disorder)) %in% c("","0","false","no")]

#dt1 : Prehospital.cardiac.arrest.with.CPR



# ==== Complications ==== 

#Complication 1: Other_Hospital_Complication
dt1[, Other_Hospital_Complication := fcase(is.na(Other.Complication),FALSE,
                                           Other.Complication %in% c(1, "1"),
                                           TRUE, default = FALSE)]
dt2[, Other_Hospital_Complication := fcase(is.na(Other_hc),FALSE,
                                           tolower(as.character(Other_hc)) %in% negatives,FALSE,
                                           default = TRUE)]

#Complication 4: Acute_Kidney_Injury
dt1[, Acute_Kidney_Injury := fcase(is.na(Acute.Kidney.Injury),FALSE,Acute.Kidney.Injury %in% c(4, "4"),TRUE, default = FALSE)]
#dt2[, Acute_Kidney_Injury := !tolower(as.character(Acute.Kidney.Injury)) %in% c("no","0","false")]
dt2[, Acute_Kidney_Injury := fcase(is.na(Acute.Kidney.Injury),FALSE,
                                   tolower(as.character(Acute.Kidney.Injury)) %in% negatives,FALSE,
                                   default = TRUE)]


#Complication 5: Acute_Lung_Injury_ARDS
dt1[, Acute_Lung_Injury_ARDS := fcase(is.na(Acute.lung.injury.ARDS),FALSE,Acute.lung.injury.ARDS %in% c(5, "5"),TRUE, default = FALSE)]
#dt2[, Acute_Lung_Injury_ARDS := !tolower(as.character(ARDS)) %in% c("no","0","false")]
dt2[, Acute_Lung_Injury_ARDS := fcase(is.na(ARDS),FALSE,
                                      tolower(as.character(ARDS)) %in% negatives,FALSE,
                                      default = TRUE)]

#Complication 8: Cardiac_Arrest_with_Resuscitative_Efforts
dt1[, Cardiac_Arrest_with_Resuscitative_Efforts := fcase(is.na(Cardiac.arrest.with.resuscitative.efforts),FALSE,Cardiac.arrest.with.resuscitative.efforts %in% c(8, "8"),TRUE, default = FALSE)]
#dt2[, Cardiac_Arrest_with_Resuscitative_Efforts := !tolower(as.character(Cardiac.Arrest)) %in% c("no","0","false")]
dt2[, Cardiac_Arrest_with_Resuscitative_Efforts := fcase(is.na(Cardiac.Arrest),FALSE,
                                                         tolower(as.character(Cardiac.Arrest)) %in% negatives,FALSE,
                                                         default = TRUE)]

#Complication 11: Pressure Ulcer
dt1[, Pressure_Ulcer := fcase(is.na(Decubitus.ulcer),FALSE,Decubitus.ulcer %in% c(11, "11"),TRUE, default = FALSE)]
#dt2[, Pressure_Ulcer := !tolower(as.character(Pressure.Ulcer)) %in% c("no","0","false")]
dt2[, Pressure_Ulcer := fcase(is.na(Pressure.Ulcer),FALSE,
                              tolower(as.character(Pressure.Ulcer)) %in% negatives,FALSE,
                              default = TRUE)]

#Complication 12: Deep Surgical Site Infection (Deep_SSI)
dt1[, Deep_SSI := fcase(is.na(Deep.surgical.site.infection),FALSE,Deep.surgical.site.infection %in% c(12, "12"),TRUE, default = FALSE)]
#dt2[, Deep_SSI := !tolower(as.character(Deep.Surgical.Site.Infection)) %in% c("no","0","false")]
dt2[, Deep_SSI := fcase(is.na(Deep.Surgical.Site.Infection),FALSE,
                        tolower(as.character(Deep.Surgical.Site.Infection)) %in% negatives,FALSE,
                        default = TRUE)]

#Complication 13: Drug_or_Alcohol_Withdrawal_Syndrome
dt1[, Drug_or_Alcohol_Withdrawal_Syndrome := fcase(is.na(Drug.or.alcohol.withdrawal.syndrome),FALSE,Drug.or.alcohol.withdrawal.syndrome %in% c(13, "13"),TRUE, default = FALSE)]
#dt2[, Drug_or_Alcohol_Withdrawal_Syndrome := !tolower(as.character(Alcohol.Withdrawal.Syndrome)) %in% c("no","0","false")]
dt2[, Drug_or_Alcohol_Withdrawal_Syndrome := fcase(is.na(Alcohol.Withdrawal.Syndrome),FALSE,
                                                   tolower(as.character(Alcohol.Withdrawal.Syndrome)) %in% negatives,FALSE,
                                                   default = TRUE)]

#Complication 14: Deep_Vein_Thrombosis_DVT_Thrombophlebitis
dt1[, Deep_Vein_Thrombosis_DVT_Thrombophlebitis := fcase(is.na(DVT.thrombophlebitis),FALSE,DVT.thrombophlebitis %in% c(14, "14"),TRUE, default = FALSE)]
#dt2[, Deep_Vein_Thrombosis_DVT_Thrombophlebitis := !tolower(as.character(Deep.Vein.Thrombosis)) %in% c("no","0","false")]
dt2[, Deep_Vein_Thrombosis_DVT_Thrombophlebitis := fcase(is.na(Deep.Vein.Thrombosis),FALSE,
                                                         tolower(as.character(Deep.Vein.Thrombosis)) %in% negatives,FALSE,
                                                         default = TRUE)]


#Complication 15: Extremity_Compartment_Syndrome
dt1[, Extremity_Compartment_Syndrome := fcase(is.na(Extremity.compartment.syndrome),FALSE,Extremity.compartment.syndrome %in% c(15, "15"),TRUE, default = FALSE)]
#dt2[, Extremity_Compartment_Syndrome := !tolower(as.character(Extremity.Compartment.Syndrome)) %in% c("no","0","false","")]
dt2[, Extremity_Compartment_Syndrome := fcase(is.na(Extremity.Compartment.Syndrome),FALSE,
                                              tolower(as.character(Extremity.Compartment.Syndrome)) %in% negatives,FALSE,
                                              default = TRUE)]

#Complication 18: Myocardial Infarction Hospital Complication (MI_hc)
dt1[, MI_hc := fcase(is.na(Myocardial.infarction),FALSE,Myocardial.infarction %in% c(18, "18"),TRUE, default = FALSE)]
#dt2[, MI_hc := !tolower(as.character(Myocardial.Infarction_hc)) %in% c("no","0","false","")]
dt2[, MI_hc := fcase(is.na(Myocardial.Infarction_hc),FALSE,
                     tolower(as.character(Myocardial.Infarction_hc)) %in% negatives,FALSE,
                     default = TRUE)]

#Complication 19: Organ/Space Surgical site infection (Organ_Space_SSI)
dt1[, Organ_Space_SSI := fcase(is.na(Organ.space.surgical.site.infection),FALSE,Organ.space.surgical.site.infection %in% c(19, "19"),TRUE, default = FALSE)]
#dt2[, Organ_Space_SSI := !tolower(as.character(Organ.Space.SSI)) %in% c("no","0","false","")]
dt2[, Organ_Space_SSI := fcase(is.na(Organ.Space.SSI),FALSE,
                               tolower(as.character(Organ.Space.SSI)) %in% negatives,FALSE,
                               default = TRUE)]

#Complication 20: Pneumonia (keep separate!)
dt1[, Pneumonia_ := fcase(is.na(Pneumonia),FALSE,Pneumonia %in% c(20, "20"),TRUE, default = FALSE)]
#dt2[, Pneumonia_ := !tolower(as.character(Ventilator.Associated.Pneumonia)) %in% c("no","0","false","")]
dt2[, Ventilator_Pneumonia := fcase(is.na(Ventilator.Associated.Pneumonia),FALSE,
                          tolower(as.character(Ventilator.Associated.Pneumonia)) %in% negatives,FALSE,
                          default = TRUE)]

#Complication 21: Pulmonary_Embolism
dt1[, Pulmonary_Embolism := fcase(is.na(Pulmonary.embolism),FALSE,Pulmonary.embolism %in% c(21, "21"),TRUE, default = FALSE)]
#dt2[, Pulmonary_Embolism := !tolower(as.character(Pulmonary.Embolism)) %in% c("no","0","false","")]
dt2[, Pulmonary_Embolism := fcase(is.na(Pulmonary.Embolism),FALSE,
                                  tolower(as.character(Pulmonary.Embolism)) %in% negatives,FALSE,
                                  default = TRUE)]

#Complication 22: Stroke_CVA
dt1[, Stroke_CVA := fcase(is.na(Stroke.CVA),FALSE,Stroke.CVA %in% c(22, "22"),TRUE, default = FALSE)]
#dt2[, Stroke_CVA := !tolower(as.character(Stroke.CVA)) %in% c("no","0","false","")]
dt2[, Stroke_CVA := fcase(is.na(Stroke.CVA),FALSE,
                          tolower(as.character(Stroke.CVA)) %in% negatives,FALSE,
                          default = TRUE)]

#Complication 23: Superficial Incisional Surgical Site Infection (Superficial_Incisional_SSI)
dt1[, Superficial_Incisional_SSI := fcase(is.na(Superficial.surgical.site.infection),FALSE,Superficial.surgical.site.infection %in% c(23, "23"),TRUE, default = FALSE)]
#dt2[, Superficial_Incisional_SSI := !tolower(as.character(Superficial.Incisional.SSI)) %in% c("no","0","false","")]
dt2[, Superficial_Incisional_SSI := fcase(is.na(Superficial.Incisional.SSI),FALSE,
                                          tolower(as.character(Superficial.Incisional.SSI)) %in% negatives,FALSE,
                                          default = TRUE)]

#Complication 25: Unplanned_Intubation
dt1[, Unplanned_Intubation := fcase(is.na(Unplanned.intubation),FALSE,Unplanned.intubation %in% c(25, "25"),TRUE,default = FALSE)]
#dt2[, Unplanned_Intubation := !tolower(as.character(Unplanned.Intubation)) %in% c("no","0","false","")]
dt2[, Unplanned_Intubation := fcase(is.na(Unplanned.Intubation),FALSE,
                                    tolower(as.character(Unplanned.Intubation)) %in% negatives,FALSE,
                                    default = TRUE)]

#Complication 27: Urinary Tract Infection
dt1[, UTI := fcase(is.na(Urinary.tract.infection),FALSE,Urinary.tract.infection %in% c(27, "27"),TRUE,default = FALSE)]
#dt2[, UTI := !tolower(as.character(Catheter.Associated.Urinary.Tract.Infection)) %in% c("no","0","false","")]
dt2[, UTI := fcase(is.na(Catheter.Associated.Urinary.Tract.Infection),FALSE,
                   tolower(as.character(Catheter.Associated.Urinary.Tract.Infection)) %in% negatives,FALSE,
                   default = TRUE)]

#Complication 28: Central Line-associated Bloodstream Infection (CLABSI)
dt1[, CLABSI := fcase(is.na(Catheter.related.blood.stream.infection),FALSE,Catheter.related.blood.stream.infection %in% c(28, "28"),TRUE, default = FALSE)]
#dt2[, CLABSI := !tolower(as.character(Central.Line.associated.Bloodstream.Infection)) %in% c("no","0","false")]
dt2[, CLABSI := fcase(is.na(Central.Line.associated.Bloodstream.Infection),FALSE,
                      tolower(as.character(Central.Line.associated.Bloodstream.Infection)) %in% negatives,FALSE,
                      default = TRUE)]

#Complication 29: Osteomyelitis
dt1[, Osteomyelitis_ := fcase(is.na(Osteomyelitis),FALSE,Osteomyelitis %in% c(29, "29"),TRUE, default = FALSE)]
#dt2[, Osteomyelitis_ := !tolower(as.character(Osteomyelitis)) %in% c("no","0","false","")]
dt2[, Osteomyelitis_ := fcase(is.na(Osteomyelitis),FALSE,
                              tolower(as.character(Osteomyelitis)) %in% negatives,FALSE,
                              default = TRUE)]

#Complication 30: Unplanned return/visit to the OR (Unplanned_OR)
dt1[, Unplanned_OR := fcase(is.na(Unplanned.return.to.the.OR),FALSE,Unplanned.return.to.the.OR %in% c(30, "30"),TRUE,default = FALSE)]
#dt2[, Unplanned_OR := !tolower(as.character(Unplanned.Visit.to.OR)) %in% c("no","0","false","")]
dt2[, Unplanned_OR := fcase(is.na(Unplanned.Visit.to.OR),FALSE,
                            tolower(as.character(Unplanned.Visit.to.OR)) %in% negatives,FALSE,
                            default = TRUE)]

#Complication 31: Unplanned Return/Admission to the ICU (Unplanned_ICU)
dt1[, Unplanned_ICU := fcase(is.na(Unplanned.return.to.the.ICU),FALSE,Unplanned.return.to.the.ICU %in% c(31, "31"),TRUE, default = FALSE)]
#dt2[, Unplanned_ICU := !tolower(as.character(Unplanned.admission.to.ICU)) %in% c("no","0","false","")]
dt2[, Unplanned_ICU := fcase(is.na(Unplanned.admission.to.ICU),FALSE,
                             tolower(as.character(Unplanned.admission.to.ICU)) %in% negatives,FALSE,
                             default = TRUE)]

#Complication 32: Severe_Sepsis
dt1[, Severe_Sepsis := fcase(is.na(Severe.Sepsis),FALSE,Severe.Sepsis %in% c(32, "32"),TRUE, default = FALSE)]
#dt2[, Severe_Sepsis := !tolower(as.character(Severe.Sepsis)) %in% c("no","0","false","")]
dt2[, Severe_Sepsis := fcase(is.na(Severe.Sepsis),FALSE,
                             tolower(as.character(Severe.Sepsis)) %in% negatives,FALSE,
                             default = TRUE)]

#dt1 unmatched unique complications
#Abdominal.Compartment.Syndrome
dt1[, Abdominal_Compartment_Syndrome := fcase(is.na(Abdominal.Compartment.Syndrome),FALSE,Abdominal.Compartment.Syndrome %in% c(2, "2"),TRUE, default = FALSE)]
#Abdominal.fascia.left.open
dt1[, Abdominal_fascia_left_open := fcase(is.na(Abdominal.fascia.left.open),FALSE,Abdominal.fascia.left.open %in% c(3, "3"),TRUE, default = FALSE)]
#Base.deficit
dt1[, Base_deficit := fcase(is.na(Base.deficit),FALSE,Base.deficit %in% c(6, "6"),TRUE, default = FALSE)]
#Bleeding
dt1[, Bleeding_ := fcase(is.na(Bleeding),FALSE,Bleeding %in% c(7, "7"),TRUE, default = FALSE)]
#Coagulopathy
dt1[, Coagulopathy_ := fcase(is.na(Coagulopathy),FALSE,Coagulopathy %in% c(9, "9"),TRUE, default = FALSE)]
#Coma
dt1[, Coma_ := fcase(is.na(Coma),FALSE,Coma %in% c(10, "10"),TRUE, default = FALSE)]
#Graft.prosthesis.flap.failure
dt1[, Graft_prosthesis_flap_failure := fcase(is.na(Graft.prosthesis.flap.failure),FALSE,Graft.prosthesis.flap.failure %in% c(16, "16"),TRUE, default = FALSE)]
#Intracranial.pressure
dt1[, Intracranial_pressure := fcase(is.na(Intracranial.pressure),FALSE,Intracranial.pressure %in% c(17, "17"),TRUE, default = FALSE)]
#Systemic.Sepsis
dt1[, Systemic_Sepsis := fcase(is.na(Systemic.Sepsis),FALSE,Systemic.Sepsis %in% c(24, "24"),TRUE, default = FALSE)]
#Wound.Disruption
dt1[, Wound_Disruption := fcase(is.na(Wound.Disruption),FALSE,Wound.Disruption %in% c(26, "26"),TRUE, default = FALSE)]


# dt2 unmatched unique complications
dt2[, Delirium_ := Delirium==TRUE]


# ==== Stitching ==== 

# Make sure columns are logical in both datasets
dt1 <- coerce_logi(dt1, logi_cols)
dt2 <- coerce_logi(dt2, logi_cols)

# Combine datasets
dt_all <- rbindlist(list(dt1, dt2), use.names = TRUE, fill = TRUE)

# ISS
dt_all[, ISS_sub15 := ISS == "ISS <=15"]
dt_all[, ISS_16_25 := ISS == "ISS 16-25"]
dt_all[, ISS_over25 := ISS == "ISS>25"]
dt_all[, ISS_unknown := ISS == "ISS unknown"]

# GCS
dt_all[, GCS_sub8 := GCS == "GCS <=8"]
dt_all[, GCS_9_12 := GCS == "GCS 9-12"]
dt_all[, GCS_13_15 := GCS == "GCS 13-15"]
dt_all[, GCS_unknown := GCS == "GCS unknown"]

# ACS
dt_all[, ACS_Verification_Level := fcase(
  ACS_raw %in% c("I - Level I Trauma Center","1","I"),"Level I",
  ACS_raw %in% c("II - Level II Trauma Center","2","II"),"Level II",
  ACS_raw %in% c("III - Level III Trauma Center","3","III",
                 "IV - Level IV Trauma Center","4","IV"),"Level III or IV",
  ACS_raw %in% c("","Not Applicable","Not Verified/Designated"),"Not Applicable",
  default = "ERROR!")]

dt_all[, ACS_1 := ACS_Verification_Level == "Level I"]
dt_all[, ACS_2 := ACS_Verification_Level == "Level II"]
dt_all[, ACS_34 := ACS_Verification_Level == "Level III or IV"]
dt_all[, ACS_unknown := ACS_Verification_Level == "Not Applicable"]

# Mechanism_Categories
dt_all[, Fall := Mechanism == mech_dict[3L, value]]
dt_all[, Ground_level_fall := !is.na(ECode_Description) &
         grepl("same level", ECode_Description,
               ignore.case = TRUE)]
dt_all[, MVT_Motorcyclist := Mechanism ==mech_dict[9L, value]]
dt_all[, MVT_Occupant := Mechanism ==mech_dict[8L, value]]
dt_all[, MVT_Other := Mechanism ==mech_dict[13L, value]]
dt_all[, MVT_Pedal_cyclist := Mechanism ==mech_dict[10L, value]]
dt_all[, MVT_Pedestrian:= Mechanism ==mech_dict[11L, value]]
dt_all[, MVT_Unspecified := Mechanism ==mech_dict[12L, value]]
dt_all[, Machinery := Mechanism == mech_dict[7L, value]]
dt_all[, Struck_by_against := Mechanism ==mech_dict[21L, value]]
dt_all[, Pedal_cyclist_other := Mechanism ==mech_dict[14L, value]]
dt_all[, Pedestrian_other := Mechanism ==mech_dict[15L, value]]
dt_all[, Transport_other:= Mechanism ==mech_dict[16L, value]]

dt_all[,MVT:= 
         Mechanism=="MVT Motorcyclist" | 
         Mechanism=="MVT Occupant" | 
         Mechanism=="MVT Other" |
         Mechanism=="MVT Pedal cyclist" |
         Mechanism=="MVT Pedestrian" |
         Mechanism=="MVT Unspecified"]
dt_all[,Mech_Other:= Fall!=TRUE & MVT!=TRUE]

# clean up rehab discharge data 
dt_all[, Discharge_Rehab := fcase(Hospital_Discharge_Disposition %in% c(
  "Discharged/Transferred to another type of rehabilitation or long term",
  "Discharged/Transferred to inpatient rehab or designated unit"), 
  TRUE, default = FALSE)]

dt_all[, Discharge_Another_Hospital := fcase(Hospital_Discharge_Disposition %in% c(
  "Discharged/Transferred to a short-term general hospital for inpatient care",
  "Discharged/Transferred to Long Term Care Hospital (LTCH)",
  "Discharged/Transferred to another acute care hospital using EMS",
  "Discharged/Transferred to a psychiatric hospital or psychiatric distinct part unit of a hospital"
), TRUE, default = FALSE)]

dt_all[, Discharge_SNF := fcase(Hospital_Discharge_Disposition %in% c(
  "Discharged/Transferred to Skilled Nursing Facility (SNF)"), TRUE, default = FALSE)]

dt_all[, Discharge_ICF := fcase(Hospital_Discharge_Disposition %in% c(
  "Discharged/Transferred to an Intermediate Care Facility (ICF)"), TRUE, default = FALSE)]

dt_all[, Discharge_Home := fcase(Hospital_Discharge_Disposition %in% c(
  "Discharged to home or self-care (routine discharge)",
  "Discharged home with no home services",
  "Discharge/Transferred to home under care of Home Health Agency",
  "Discharged/Transferred to home under care of organized home health service"
), TRUE, default = FALSE)]

dt_all[, Discharge_Unknown := fcase(Hospital_Discharge_Disposition %in% c(
  "Not Known/Not Recorded BIU 2"), TRUE, default = FALSE)]

# Assign discharge labels to throw everything else into other
dt_all[, Discharge_Labels := fcase(Discharge_Rehab,"Discharge_Rehab",
                                   #Discharge_Another_Hospital,"Discharge_Another_Hospital",
                                   Discharge_SNF,"Discharge_SNF",
                                   Discharge_ICF,"Discharge_ICF",
                                   Discharge_Home,"Discharge_Home",
                                   Discharge_Unknown,"Discharge_Unknown",
                                   Death,"Death",
                                   default = "Discharge_Other")]
dt_all[, Discharge_Other := fcase(Discharge_Labels=="Discharge_Other",TRUE, default = FALSE)]

dt_all[Guidelines=="Pre",Other_Complication_nMissing:=Other_Hospital_Complication|(rowSums(.SD, na.rm = TRUE) > 0), .SDcols = dt1_only_hc]
dt_all[Guidelines=="Post",Other_Complication_nMissing:=Other_Hospital_Complication|(rowSums(.SD, na.rm = TRUE) > 0), .SDcols = dt2_only_hc]

dt_all[,PreDataset := Guidelines=="Pre"]
dt_all[,PostDataset := Guidelines=="Post"]

# calculate mFI_5
FI_5_col = c("Functionally_Dependent_Health_Status","Diabetes_Mellitus",
             "Respiratory_COPD","Congestive_Heart_Failure_CHF","Hypertension_Requiring_Medication")
dt_all[, mFI_5 := rowSums(.SD, na.rm = TRUE) , .SDcols = FI_5_col] #not used

# any compliation and # complications
dt_all[, Any_Complication := rowSums(.SD, na.rm = TRUE) > 0, .SDcols = complications]
dt_all[, Num_Complication := rowSums(.SD, na.rm = TRUE), .SDcols = complications]
dt_all[Guidelines=="Pre",Any_Complication:=Any_Complication|Any.Complication]

# Age Groups
dt_all[,SuperGeri := Age>80]
dt_all[,Geri := !SuperGeri]
dt_all[,AgeGroup := fcase(Age>80,"81+",
                          Age>=65&Age<=80,"65-80")]

dt <- copy(dt_all)

# ==== Fracture Decoding ====
#Initialize
dt_all[,excluded:=FALSE]
dt_all[,exclusion_reason:="Not excluded."]

#Remove Spine Injury from Old Dataset 809
dim(dt)
dt_all[grepl("(^|,)809(\\.|$)", LE_Dcode, perl = TRUE),excluded:=TRUE]
dt_all[grepl("(^|,)809(\\.|$)", LE_Dcode, perl = TRUE),exclusion_reason:="Contains ICD-9 809 spine injury."]
dt <- dt[!grepl("(^|,)809(\\.|$)", LE_Dcode, perl = TRUE) ]
dim(dt)

#Remove Spine Injury from New Dataset S32
dim(dt)
dt_all[grepl("(^|,)\\s*S32\\.(?:0|1|2)", LE_Dcode, perl = TRUE),excluded:=TRUE]
dt_all[grepl("(^|,)\\s*S32\\.(?:0|1|2)", LE_Dcode, perl = TRUE),exclusion_reason:="Contains ICD-10 S32 spine injury."]
dt <- dt[!grepl("(^|,)\\s*S32\\.(?:0|1|2)", LE_Dcode, perl = TRUE) ]
dim(dt)

# Split LE_Dcode
le_split <- dt[, tstrsplit(LE_Dcode, ",", fixed=TRUE, type.convert=TRUE)]
setnames(le_split, paste0("LE_Dcode", seq_len(ncol(le_split))))

# Split UE_Dcode
ue_split <- dt[, tstrsplit(UE_Dcode, ",", fixed=TRUE, type.convert=TRUE)]
setnames(ue_split, paste0("UE_Dcode", seq_len(ncol(ue_split))))

# Combine all
dt_out <- cbind(dt,le_split,ue_split)

#Do the same for dt_all
le_split <- dt_all[, tstrsplit(LE_Dcode, ",", fixed=TRUE, type.convert=TRUE)]
setnames(le_split, paste0("LE_Dcode", seq_len(ncol(le_split))))
ue_split <- dt_all[, tstrsplit(UE_Dcode, ",", fixed=TRUE, type.convert=TRUE)]
setnames(ue_split, paste0("UE_Dcode", seq_len(ncol(ue_split))))
dt_all <- cbind(copy(dt_all),le_split,ue_split)

# 1) Counts
setDT(dt_out)
dt_out[, n_LE := rowSums(!is.na(.SD)), .SDcols = patterns("^LE_Dcode[0-9]+$")]
dt_out[, n_UE := rowSums(!is.na(.SD)), .SDcols = patterns("^UE_Dcode[0-9]+$")]
setDT(dt_all)
dt_all[, n_LE := rowSums(!is.na(.SD)), .SDcols = patterns("^LE_Dcode[0-9]+$")]
dt_all[, n_UE := rowSums(!is.na(.SD)), .SDcols = patterns("^UE_Dcode[0-9]+$")]

setDT(dt_out)

code_cols <- grep("^(UE|LE)_Dcode", names(dt_out), value = TRUE)

dim(dt_out)
dt_all[n_LE==1 & grepl("(^|,)827(\\.|$)", LE_Dcode, perl = TRUE),excluded:=TRUE]
dt_all[n_LE==1 & grepl("(^|,)827(\\.|$)", LE_Dcode, perl = TRUE),exclusion_reason:="Contains exclusively ICD-9 827.X."]
dt_out <- dt_out[!(n_LE==1 & grepl("(^|,)827(\\.|$)", LE_Dcode, perl = TRUE) )]
dim(dt_out)
dt_all[n_LE==1 & grepl("(^|,)828(\\.|$)", LE_Dcode, perl = TRUE),excluded:=TRUE]
dt_all[n_LE==1 & grepl("(^|,)828(\\.|$)", LE_Dcode, perl = TRUE),exclusion_reason:="Contains exclusively ICD-9 828.X."]
dt_out <- dt_out[!(n_LE==1 & grepl("(^|,)828(\\.|$)", LE_Dcode, perl = TRUE) )]
dim(dt_out)

dt_out[, New_InjType :=
         fcase(n_LE == 0 & n_UE == 1,"1U",
               n_LE == 0 & n_UE >= 1,"MU",
               n_LE == 1 & n_UE == 0,"1L",
               n_LE >  1 & n_UE == 0,"ML",
               n_LE == 1 & n_UE == 1,"1L1U",
               n_LE >  1 & n_UE == 1,"ML1U",
               n_LE == 1 & n_UE >  1,"1LMU",
               n_LE >  1 & n_UE >  1,"MLMU",
               default = NA_character_)]

# Preview
print(dt_out[, .(LE_Dcode1, LE_Dcode2, UE_Dcode1, New_InjType,n_LE,n_UE)])
dt_out[, .N, by = New_InjType]
table(dt_out$New_InjType)

# Find UE/LE code columns
ue_cols <- grep("^UE_Dcode", names(dt_out), value = TRUE)
le_cols <- grep("^LE_Dcode", names(dt_out), value = TRUE)
all_code_cols <- c(ue_cols, le_cols)

# Row id for joining back
dt_out[, row_id := .I]

# Melt all code columns to long, drop NA
long <- melt(dt_out, id.vars = "row_id", measure.vars = code_cols,
  variable.name = "which", value.name = "code", variable.factor = FALSE
)[!is.na(code) & nzchar(code)]

# Vectorized map
long[, loc := map_injury_location(code)]

# Keep unique (row_id, loc) pairs and drop NAs
long_u <- unique(long[!is.na(loc), .(row_id, loc)])

# Collapse to a single comma string per row -> readable column
injury_str <- long_u[, .(injury_location = paste(sort(unique(loc)), collapse=",")), by = row_id]

# keep only locations we know how to map
long_u <- long_u[loc %chin% names(name_map)]
long_u[, ind_col := unname(name_map[loc])]

# Wide 0/1 matrix
wide_inds <- dcast(
  unique(long_u[, .(row_id, ind_col)]),
  row_id ~ ind_col,
  fun.aggregate = length,
  value.var = "ind_col"
)

# Merge indicators and string back once
dt_out <- merge(dt_out, injury_str, by = "row_id", all.x = TRUE)
dt_out <- merge(dt_out, wide_inds,  by = "row_id", all.x = TRUE)

# Replace NA with 0 for indicator columns
ind_cols <- setdiff(names(wide_inds), "row_id")
for (cl in ind_cols) set(dt_out, which(is.na(dt_out[[cl]])), cl, 0L)

dt_out[,LE_femur:= LE_FNeckHead==TRUE|LE_FTroApo==TRUE|LE_FIntertro==TRUE|
         LE_FSubtro==TRUE|LE_FShaft==TRUE|LE_FLowerEnd==TRUE|LE_FOther==TRUE]

# ==== Purging ====
dt <-copy(dt_out)

# Keep only those columns that exist (avoids errors if some are missing)
present_keep <- keep_cols_1[keep_cols_1 %chin% names(dt)]
missing_keep <- setdiff(keep_cols_1, present_keep)
if (length(missing_keep)) message("Missing columns (skipped): ", paste(missing_keep, collapse = ", "))
dt <- dt[, ..present_keep] #keep ONLY these columns, in order

present_keep <- keep_cols_1[keep_cols_1 %chin% names(dt_all)]
missing_keep <- setdiff(keep_cols_1, present_keep)
if (length(missing_keep)) message("Missing columns (skipped): ", paste(missing_keep, collapse = ", "))
dt_all <- dt_all[, ..present_keep] #keep ONLY these columns, in order

dim(dt)
dt[Sex=="Nonbinary",.N] 
dt[is.na(Sex),.N] 
dt[Hospital_Discharge_Disposition=="Not Known/Not Recorded BIU 2",.N]
dt[is.na(Long_ICU_LOS_3d),.N] 
dt[is.na(Long_LOS_to_Final_Discharge_5d),.N] 

dt<-dt[Sex!="Nonbinary"] 
dt<-dt[!is.na(Sex)] 
dt<-dt[Hospital_Discharge_Disposition!="Not Known/Not Recorded BIU 2"]
dt<-dt[!is.na(Long_ICU_LOS_3d)] 
dt<-dt[!is.na(Long_LOS_to_Final_Discharge_5d)] 
dim(dt)

dt_all[Sex=="Nonbinary"&excluded==FALSE,exclusion_reason:="Sex - Nonbinary."]
dt_all[Sex=="Nonbinary"&excluded==TRUE, exclusion_reason:=paste(exclusion_reason, "Sex - Nonbinary.", sep=", ")]
dt_all[Sex=="Nonbinary",excluded:=TRUE]
dt_all[is.na(Sex)&excluded==FALSE,exclusion_reason:="Sex - Unknown."]
dt_all[is.na(Sex)&excluded==TRUE, exclusion_reason:=paste(exclusion_reason, "Sex - Unknown.", sep=", ")]
dt_all[is.na(Sex),excluded:=TRUE]
dt_all[Hospital_Discharge_Disposition=="Not Known/Not Recorded BIU 2"&excluded==FALSE,exclusion_reason:="Discharge - Unknown."]
dt_all[Hospital_Discharge_Disposition=="Not Known/Not Recorded BIU 2"&excluded==TRUE, exclusion_reason:=paste(exclusion_reason, "Discharge - Unknown.", sep=", ")]
dt_all[Hospital_Discharge_Disposition=="Not Known/Not Recorded BIU 2",excluded:=TRUE]
dt_all[is.na(Long_ICU_LOS_3d)&excluded==FALSE,exclusion_reason:="ICU LOS - Unknown."]
dt_all[is.na(Long_ICU_LOS_3d)&excluded==TRUE, exclusion_reason:=paste(exclusion_reason, "ICU LOS - Unknown.", sep=", ")]
dt_all[is.na(Long_ICU_LOS_3d),excluded:=TRUE]
dt_all[is.na(Long_LOS_to_Final_Discharge_5d)&excluded==FALSE,exclusion_reason:="Hosp LOS - Unknown."]
dt_all[is.na(Long_LOS_to_Final_Discharge_5d)&excluded==TRUE, exclusion_reason:=paste(exclusion_reason, "Hosp LOS - Unknown.", sep=", ")]
dt_all[is.na(Long_LOS_to_Final_Discharge_5d),excluded:=TRUE]

fname_csv <- sprintf("1_Pre_Post_stitched_decoded_expandedFemur_purged_%s.csv", format(Sys.Date(), "%y%m%d"))
fname_rds <- sprintf("1_Pre_Post_stitched_decoded_expandedFemur_purged_%s.rds", format(Sys.Date(), "%y%m%d"))
fwrite(dt, file.path(outdir, fname_csv))
saveRDS(dt, file.path(outdir, fname_rds))

fname_csv <- sprintf("1_Pre_Post_stitched_allpatients_exclusion_%s.csv", format(Sys.Date(), "%y%m%d"))
fname_rds <- sprintf("1_Pre_Post_stitched_allpatients_exclusion_%s.rds", format(Sys.Date(), "%y%m%d"))
fwrite(dt_all, file.path(outdir, fname_csv))
saveRDS(dt_all, file.path(outdir, fname_rds))
