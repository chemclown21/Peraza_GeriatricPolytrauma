# 2_tables.r for Peraza. Evolving Outcomes in Geriatric Patients with Orthopedic Polyfractures: the NTDB 2008-2023  
# Written by Vitto Resnick

# Build Table 1 and Table 2

# ==== Load data ====
files_directory = '/Users/vresnick/Documents/GitHub/Peraza2026_GeriatricPolyTrauma'
setwd(files_directory)
outdir = "outputs/tables"
dt <- readRDS("outputs/datasets/1_Pre_Post_stitched_decoded_expandedFemur_purged_260120.rds")


# ==== Get initial frequencies ====

## Check denominators
dt[Geri==FALSE|SuperGeri==FALSE,.N]

# ensure character vector (object is a list)
vars <- unlist(variables_tables, use.names = FALSE)
N_by_group <- dt[, .N, by = Guidelines]# total N per group

summary_dt <- rbindlist(lapply(vars, function(v) {
  if (!v %in% names(dt)) {# handle missing columns safely
    return(data.table(
      Factor = v,Pre = NA_character_,Post = NA_character_))}
  # count patients with factor by group
  counts <- dt[, .(
    n = sum(get(v) == 1, na.rm = TRUE),
    total = .N), by = Guidelines]
  
  pre_row  <- counts[Guidelines == "Pre"]
  post_row <- counts[Guidelines == "Post"]
  
  data.table(Factor = v,
             Pre  = if (nrow(pre_row)  == 1) fmt_freq(pre_row$n,  pre_row$total)  else NA_character_,
             Post = if (nrow(post_row) == 1) fmt_freq(post_row$n, post_row$total) else NA_character_
  )}))


fname_csv <- sprintf("2_table12_frequencies_%s.csv", format(Sys.Date(), "%y%m%d"))
fname_rds <- sprintf("2_table12_frequencies_%s.rds", format(Sys.Date(), "%y%m%d"))

fwrite(summary_dt, file.path(outdir, fname_csv))
saveRDS(summary_dt, file.path(outdir, fname_rds))


# ==== Fill in frequency values: Table 1 ====

## Insurance

# Manually handle calculate (adjusted denominator) due to unknowns
gov1 = dt[Guidelines=="Pre"& Primary_Payment_Method=="Medicare, Medicaid, or Government" ,.N]
private1 = dt[Guidelines=="Pre"& Primary_Payment_Method=="Private/Commercial Insurance" ,.N]
self1 = dt[Guidelines=="Pre"& Primary_Payment_Method=="No insurance/self-pay" ,.N]
sum = dt[Guidelines=="Pre"& Primary_Payment_Method=="Medicare, Medicaid, or Government" ,.N]+dt[Guidelines=="Pre"& Primary_Payment_Method=="Private/Commercial Insurance" ,.N]+dt[Guidelines=="Pre"& Primary_Payment_Method=="No insurance/self-pay" ,.N]
fmt_freq(gov1,sum)
fmt_freq(private1,sum)
fmt_freq(self1,sum)

gov2 = dt[Guidelines=="Post"& Primary_Payment_Method=="Medicare, Medicaid, or Government" ,.N]
private2 = dt[Guidelines=="Post"& Primary_Payment_Method=="Private/Commercial Insurance" ,.N]
self2 = dt[Guidelines=="Post"& Primary_Payment_Method=="No insurance/self-pay" ,.N]
sum = dt[Guidelines=="Post"& Primary_Payment_Method=="Medicare, Medicaid, or Government" ,.N]+dt[Guidelines=="Post"& Primary_Payment_Method=="Private/Commercial Insurance" ,.N]+dt[Guidelines=="Post"& Primary_Payment_Method=="No insurance/self-pay" ,.N]
fmt_freq(gov2,sum)
fmt_freq(private2,sum)
fmt_freq(self2,sum)

## Mechanism (MOI)

dt_mech <- copy(dt[Mechanism!="Unspecified"])
vars <- unlist(variables_mech, use.names = FALSE)

# total N per group
N_by_group <- dt_mech[, .N, by = Guidelines]

mech_summary <- rbindlist(lapply(vars, function(v) {
  # handle missing columns safely
  if (!v %in% names(dt_mech)) {
    return(data.table(Factor = v,Pre = NA_character_,Post = NA_character_))}
  # count patients with factor by group
  counts <- dt_mech[, .(n = sum(get(v) == 1, na.rm = TRUE),total = .N), by = Guidelines]
  pre_row  <- counts[Guidelines == "Pre"]
  post_row <- counts[Guidelines == "Post"]
  data.table(Factor = v,
    Pre  = if (nrow(pre_row)  == 1) fmt_freq(pre_row$n,  pre_row$total)  else NA_character_,
    Post = if (nrow(post_row) == 1) fmt_freq(post_row$n, post_row$total) else NA_character_
  )
}))
mech_summary


## Injury Type
sum = dt[Guidelines=="Pre",.N]
l1 = dt[Guidelines=="Pre"&New_InjType=="1L",.N]
l1u1 = dt[Guidelines=="Pre"&New_InjType=="1L1U",.N]
l1um = dt[Guidelines=="Pre"&New_InjType=="1LMU",.N]
lm = dt[Guidelines=="Pre"&New_InjType=="ML",.N]
lm1u = dt[Guidelines=="Pre"&New_InjType=="ML1U",.N]
lmum = dt[Guidelines=="Pre"&New_InjType=="MLMU",.N]
fmt_freq(l1,sum)
fmt_freq(l1u1,sum)
fmt_freq(l1um,sum)
fmt_freq(lm,sum)
fmt_freq(lm1u,sum)
fmt_freq(lmum,sum)

sum = dt[Guidelines=="Post",.N]
l1 = dt[Guidelines=="Post"&New_InjType=="1L",.N]
l1u1 = dt[Guidelines=="Post"&New_InjType=="1L1U",.N]
l1um = dt[Guidelines=="Post"&New_InjType=="1LMU",.N]
lm = dt[Guidelines=="Post"&New_InjType=="ML",.N]
lm1u = dt[Guidelines=="Post"&New_InjType=="ML1U",.N]
lmum = dt[Guidelines=="Post"&New_InjType=="MLMU",.N]
fmt_freq(l1,sum)
fmt_freq(l1u1,sum)
fmt_freq(l1um,sum)
fmt_freq(lm,sum)
fmt_freq(lm1u,sum)
fmt_freq(lmum,sum)


## ISS

# Check denominators
dt[ISS_unknown==TRUE,.N]

# Manually handle calculate (adjusted denominator) due to unknowns
sum = dt[Guidelines=="Pre"&ISS_unknown==FALSE,.N]
i1 = dt[Guidelines=="Pre"&ISS_sub15==TRUE,.N]
i2 = dt[Guidelines=="Pre"&ISS_16_25==TRUE,.N]
i3 = dt[Guidelines=="Pre"&ISS_over25==TRUE,.N]
fmt_freq(i1,sum)
fmt_freq(i2,sum)
fmt_freq(i3,sum)

sum = dt[Guidelines=="Post"&ISS_unknown==FALSE,.N]
i1 = dt[Guidelines=="Post"&ISS_sub15==TRUE,.N]
i2 = dt[Guidelines=="Post"&ISS_16_25==TRUE,.N]
i3 = dt[Guidelines=="Post"&ISS_over25==TRUE,.N]
fmt_freq(i1,sum)
fmt_freq(i2,sum)
fmt_freq(i3,sum)


## GCS

# Check denominators
dt[GCS_unknown==TRUE,.N]

# Manually handle calculate (adjusted denominator) due to unknowns
sum = dt[Guidelines=="Pre"&GCS_unknown==FALSE,.N]
g1 = dt[Guidelines=="Pre"&GCS_sub8==TRUE,.N]
g2 = dt[Guidelines=="Pre"&GCS_9_12==TRUE,.N]
g3 = dt[Guidelines=="Pre"&GCS_13_15==TRUE,.N]
fmt_freq(g1,sum)
fmt_freq(g2,sum)
fmt_freq(g3,sum)

sum = dt[Guidelines=="Post"&GCS_unknown==FALSE,.N]
g1 = dt[Guidelines=="Post"&GCS_sub8==TRUE,.N]
g2 = dt[Guidelines=="Post"&GCS_9_12==TRUE,.N]
g3 = dt[Guidelines=="Post"&GCS_13_15==TRUE,.N]
fmt_freq(g1,sum)
fmt_freq(g2,sum)
fmt_freq(g3,sum)


## ACS
# Manually handle calculate (adjusted denominator) due to unknowns
sum = dt[Guidelines=="Pre"&ACS_unknown==FALSE,.N]
a1 = dt[Guidelines=="Pre"&ACS_1==TRUE,.N]
a2 = dt[Guidelines=="Pre"&ACS_2==TRUE,.N]
a3 = dt[Guidelines=="Pre"&ACS_34==TRUE,.N]
fmt_freq(a1,sum)
fmt_freq(a2,sum)
fmt_freq(a3,sum)

sum = dt[Guidelines=="Post"&ACS_unknown==FALSE,.N]
sum
a1 = dt[Guidelines=="Post"&ACS_1==TRUE,.N]
a2 = dt[Guidelines=="Post"&ACS_2==TRUE,.N]
a3 = dt[Guidelines=="Post"&ACS_34==TRUE,.N]
fmt_freq(a1,sum)
fmt_freq(a2,sum)
fmt_freq(a3,sum)



# ==== Get P Values: Table 1 ====

# Age (grouped, Chi)
age <- matrix(
  # Pre Post
  c(dt[Guidelines=="Pre"&Geri==TRUE,.N], dt[Guidelines=="Post"&Geri==TRUE,.N],   
    dt[Guidelines=="Pre"&Geri==FALSE,.N], dt[Guidelines=="Post"&Geri==FALSE,.N]),  
  nrow = 2,byrow = TRUE)
rownames(age) <- c("65-80", ">80")
colnames(age) <- c("Pre", "Post")
age
chisq.test(age)

# Sex (grouped, Chi)
sex <- matrix(
  c(dt[Guidelines=="Pre"&Female==TRUE,.N], dt[Guidelines=="Post"&Female==TRUE,.N],   
    dt[Guidelines=="Pre"&Female==FALSE,.N], dt[Guidelines=="Post"&Female==FALSE,.N]),  
  nrow = 2,byrow = TRUE)
rownames(sex) <- c("Female", "Male")
colnames(sex) <- c("Pre", "Post")
sex
chisq.test(sex)

# Race (grouped, Chi)
race <- matrix(
  c(dt[Guidelines=="Pre"&Black==TRUE,.N], dt[Guidelines=="Post"&Black==TRUE,.N],   
    dt[Guidelines=="Pre"&White==TRUE,.N], dt[Guidelines=="Post"&White==TRUE,.N],
    dt[Guidelines=="Pre"&Other_Race==TRUE,.N], dt[Guidelines=="Post"&Other_Race==TRUE,.N]), 
  nrow = 3,byrow = TRUE)
rownames(race) <- c("Black", "White","Other")
colnames(race) <- c("Pre", "Post")
race
chisq.test(race)

# Ethnicity (grouped, Chi)
hisp <- matrix(
  c(dt[Guidelines=="Pre"&Hispanic_Ethnicity==TRUE,.N], dt[Guidelines=="Post"&Hispanic_Ethnicity==TRUE,.N],   
    dt[Guidelines=="Pre"&Hispanic_Ethnicity==FALSE,.N], dt[Guidelines=="Post"&Hispanic_Ethnicity==FALSE,.N]), 
  nrow = 2,byrow = TRUE)
rownames(hisp) <- c("Hisp", "Non-hisp")
colnames(hisp) <- c("Pre", "Post")
hisp
chisq.test(hisp)

# Payment Method (grouped, Chi)
pay <- matrix(
  c(dt[Guidelines=="Pre"&Gov_Care_Caid==TRUE,.N], dt[Guidelines=="Post"&Gov_Care_Caid==TRUE,.N],   
    dt[Guidelines=="Pre"&Private_Ins==TRUE,.N], dt[Guidelines=="Post"&Private_Ins==TRUE,.N],   
    dt[Guidelines=="Pre"&No_Insurance==TRUE,.N], dt[Guidelines=="Post"&No_Insurance==TRUE,.N]), 
  nrow = 3,byrow = TRUE)
rownames(pay) <- c("Gov_Care_Caid", "Private_Ins","No_Insurance")
colnames(pay) <- c("Pre", "Post")
pay
chisq.test(pay)

# Comorbidities (row-wise)
comorbs <- rbindlist(lapply(comorbs_cols, var_wise_Chi))
comorbs

# MOI (grouped, Chi)
mechs <- matrix(
  c(dt[Guidelines=="Pre"&Fall==TRUE,.N], dt[Guidelines=="Post"&Fall==TRUE,.N],   
    dt[Guidelines=="Pre"&MVT==TRUE,.N], dt[Guidelines=="Post"&MVT==TRUE,.N],   
    dt[Guidelines=="Pre"&Mech_Other==TRUE,.N], dt[Guidelines=="Post"&Mech_Other==TRUE,.N]), 
  nrow = 3,byrow = TRUE)
rownames(mechs) <- c("Fall", "MVT","Mech_Other")
colnames(mechs) <- c("Pre", "Post")
mechs
chisq.test(mechs)

# Injury Groups (grouped, Chi)
inj <- matrix(
  c(dt[Guidelines=="Pre"&New_InjType=="1L",.N], dt[Guidelines=="Post"&New_InjType=="1L",.N],   
    dt[Guidelines=="Pre"&New_InjType=="1L1U",.N], dt[Guidelines=="Post"&New_InjType=="1L1U",.N],
    dt[Guidelines=="Pre"&New_InjType=="1LMU",.N], dt[Guidelines=="Post"&New_InjType=="1LMU",.N],   
    dt[Guidelines=="Pre"&New_InjType=="ML",.N], dt[Guidelines=="Post"&New_InjType=="ML",.N],
    dt[Guidelines=="Pre"&New_InjType=="ML1U",.N], dt[Guidelines=="Post"&New_InjType=="ML1U",.N],
    dt[Guidelines=="Pre"&New_InjType=="MLMU",.N], dt[Guidelines=="Post"&New_InjType=="MLMU",.N]), 
  nrow = 6,byrow = TRUE)
rownames(inj) <- c("1L","1L1U","1LMU","ML","ML1U","MLMU")
colnames(inj) <- c("Pre", "Post")
inj
chisq.test(inj)

# ISS (grouped, Chi)
iss <- matrix(
  c(dt[Guidelines=="Pre"&ISS_sub15==TRUE,.N], dt[Guidelines=="Post"&ISS_sub15==TRUE,.N],   
    dt[Guidelines=="Pre"&ISS_16_25==TRUE,.N], dt[Guidelines=="Post"&ISS_16_25==TRUE,.N],
    dt[Guidelines=="Pre"&ISS_over25==TRUE,.N], dt[Guidelines=="Post"&ISS_over25==TRUE,.N]), 
  nrow = 3,byrow = TRUE)
rownames(iss) <- c("ISS_sub15","ISS_16_25","ISS_over25")
colnames(iss) <- c("Pre", "Post")
iss
chisq.test(iss)

# GCS (grouped, Chi)
gcs <- matrix(
  c(dt[Guidelines=="Pre"&GCS_sub8==TRUE,.N], dt[Guidelines=="Post"&GCS_sub8==TRUE,.N],   
    dt[Guidelines=="Pre"&GCS_9_12==TRUE,.N], dt[Guidelines=="Post"&GCS_9_12==TRUE,.N],
    dt[Guidelines=="Pre"&GCS_13_15==TRUE,.N], dt[Guidelines=="Post"&GCS_13_15==TRUE,.N]), 
  nrow = 3,byrow = TRUE)
rownames(gcs) <- c("GCS_sub8","GCS_9_12","GCS_13_15")
colnames(gcs) <- c("Pre", "Post")
gcs
chisq.test(gcs)

# ACS (grouped, Chi)
acs <- matrix(
  c(dt[Guidelines=="Pre"&ACS_1==TRUE,.N], dt[Guidelines=="Post"&ACS_1==TRUE,.N],   
    dt[Guidelines=="Pre"&ACS_2==TRUE,.N], dt[Guidelines=="Post"&ACS_2==TRUE,.N],
    dt[Guidelines=="Pre"&ACS_34==TRUE,.N], dt[Guidelines=="Post"&ACS_34==TRUE,.N]), 
  nrow = 3,byrow = TRUE)
rownames(acs) <- c("ACS_1","ACS_2","ACS_34")
colnames(acs) <- c("Pre", "Post")
acs
chisq.test(acs)

# ==== Get P Values: Table 2 ====

# Discharge disposition (grouped, Chi)
dis <- matrix(
  c(dt[Guidelines=="Pre"&Death==TRUE,.N], dt[Guidelines=="Post"&Death==TRUE,.N],   
    dt[Guidelines=="Pre"&Discharge_Home==TRUE,.N], dt[Guidelines=="Post"&Discharge_Home==TRUE,.N],
    dt[Guidelines=="Pre"&Discharge_SNF==TRUE,.N], dt[Guidelines=="Post"&Discharge_SNF==TRUE,.N],   
    dt[Guidelines=="Pre"&Discharge_Rehab==TRUE,.N], dt[Guidelines=="Post"&Discharge_Rehab==TRUE,.N],
    dt[Guidelines=="Pre"&Discharge_ICF==TRUE,.N], dt[Guidelines=="Post"&Discharge_ICF==TRUE,.N],
    dt[Guidelines=="Pre"&Discharge_Other==TRUE,.N], dt[Guidelines=="Post"&Discharge_Other==TRUE,.N]), 
  nrow = 6,byrow = TRUE)
rownames(dis) <- c("Death","Discharge_Home","Discharge_SNF","Discharge_Rehab",
                   "Discharge_ICF","Discharge_Other")
colnames(dis) <- c("Pre", "Post")
dis
chisq.test(dis)

# Part two of Table 2 (row-wise)
table2b <- rbindlist(lapply(table2b_cols, var_wise_Chi))
table2b
