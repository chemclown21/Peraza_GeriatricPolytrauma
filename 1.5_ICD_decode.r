# 1.5_ICD_decode.r for Peraza. Evolving Outcomes in Geriatric Patients with Orthopedic Polyfractures: the NTDB 2008-2023  
# Written by Vitto Resnick

# Collect all ICD codes used and decode into Dx w/ frequency.


# ==== Loading Data ==== 
files_directory = '/Users/vresnick/Documents/GitHub/Peraza2026_GeriatricPolyTrauma'
setwd(files_directory)
outdir = "outputs" #Set directory for outputing files.

#Load in pre-revision dataset
dt1 <- fread("original_datasets/most_current_DBs/vitto_08_14.csv")
setnames(dt1, make.names(colnames(dt1))) #Converts spaces to periods

#Load in post-revision dataset
dt2 <- fread("original_datasets/most_current_DBs/vitto_17_23.csv")
setnames(dt2, make.names(colnames(dt2))) #Converts spaces to periods


# ==== Extract ordered unique LE_DCode values ==== 

# Assume dt is data.table and LE_DCode is character
dt1[, LE_Dcode := as.character(LE_Dcode)]
dt1[, UE_Dcode := as.character(UE_Dcode)]
dt2[, LE_Dcode := as.character(LE_Dcode)]
dt2[, UE_Dcode := as.character(UE_Dcode)]

# Split codes
LE_codes1 <- unlist(strsplit(dt1$LE_Dcode[!is.na(dt1$LE_Dcode)], ","),use.names = FALSE)
UE_codes1 <- unlist(strsplit(dt1$UE_Dcode[!is.na(dt1$UE_Dcode)], ","),use.names = FALSE)
LE_codes2 <- unlist(strsplit(dt2$LE_Dcode[!is.na(dt2$LE_Dcode)], ","),use.names = FALSE)
UE_codes2 <- unlist(strsplit(dt2$UE_Dcode[!is.na(dt2$UE_Dcode)], ","),use.names = FALSE)

# Trim whitespace
all_codes1 <- c(LE_codes1,UE_codes1)
all_codes1 <- trimws(all_codes1)
all_codes1 <- all_codes1[nzchar(all_codes1)]
all_codes2 <- c(LE_codes2,UE_codes2)
all_codes2 <- trimws(all_codes2)
all_codes2 <- all_codes2[nzchar(all_codes2)]

# Ordered unique vector (preserves first appearance)
unique_codes1 <- all_codes1[!duplicated(all_codes1)]
unique_codes1 <- unique(unique_codes1)[order(unique(unique_codes1))]
unique_codes2 <- all_codes2[!duplicated(all_codes2)]
unique_codes2 <- unique(unique_codes2)[order(unique(unique_codes2))]

unique_codes <- c(unique_codes1,unique_codes2)

icd9_map  <- setNames(icd9cm_hierarchy$long_desc,normalize_icd(icd9cm_hierarchy$code))
icd10_map <- setNames(     icd10cm2016$long_desc,normalize_icd(     icd10cm2016$code))

df <- data.frame(ICD = unique_codes, stringsAsFactors = FALSE)
results <- lapply(df$ICD, icd_to_description)

df_out2 <- df %>% mutate(ICD_version = sapply(results,`[[`,1),
                         Diagnosis   = sapply(results,`[[`,2))

patient_id <- "inc_key"   
long1 <- extract_long_icd(dt1, patient_id)
long2 <- extract_long_icd(dt2, patient_id)

long_all <- rbindlist(list(long1, long2), use.names = TRUE)

long_all[, ICD := clean_code(ICD)]
long_all <- long_all[!is.na(ICD)]

icd_patient_counts <- long_all[, .(n_patients = uniqueN(id)),by = ICD]

df_out3 <- df_out2 %>%
  left_join(icd_patient_counts, by = "ICD") %>%
  mutate(n_patients = replace_na(n_patients, 0))

fname_csv <- sprintf("tables/eTable1_%s.csv", format(Sys.Date(), "%y%m%d"))
fwrite(df_out3, file.path(outdir, fname_csv))

