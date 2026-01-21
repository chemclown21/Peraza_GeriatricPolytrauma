# 4_regression.r for Peraza. Evolving Outcomes in Geriatric Patients with Orthopedic Polyfractures: the NTDB 2008-2023  
# Written by Vitto Resnick

# Logistic Regression Analyses

# ==== Load data ====
files_directory = '/Users/vresnick/Documents/GitHub/Peraza2026_GeriatricPolyTrauma'
setwd(files_directory)
outdir = "outputs"
dt <- readRDS("outputs/datasets/1_Pre_Post_stitched_decoded_expandedFemur_purged_260120.rds")

# ==== Regression test ====

#Run this as a test
run_ps_overlap_logit_psweight(data=dt[New_InjType == "MLMU"],
                              outcome = "Death",
                              covariates=covariate_names_full,
                              strat_var="New_InjType",
                              stratum="MLMU")

# ==== Prep data ====

# Treatment / exposure: Guidelines (ref = "Pre")
dt[, Guidelines := factor(Guidelines)]
dt[, Guidelines := relevel(Guidelines, ref = "Pre")]


# ==== InjType-Big4 : Prep data ====

# New_InjType factor (for stratification)
dt[, New_InjType := factor(New_InjType,levels = injury_levels)]
# Make sure all covariates are factors (categorical)
dt[, (covariate_names_full) := lapply(.SD, factor), .SDcols = covariate_names_full]

# ==== InjType-Big4 : Run models ====

# Run models: stratified by New_InjType + whole dataset
results_list <- list()
sv = "New_InjType"
for (y in outcome_vars_reg){
  
  message(sprintf("Running: All Patients | Outcome = %s",y))
  res_all <- run_ps_overlap_logit_psweight(dt,y,covariate_names_full,strat_var=sv)
  results_list[[length(results_list) + 1L]] <- res_all
  
  for (inj in injury_levels){
    dt_sub <- dt[New_InjType == inj]
    message(sprintf("Running: StratVar = %s | Stratum = %s | Outcome = %s",sv, inj, y))
    results_list[[length(results_list) + 1L]] <- 
      run_ps_overlap_logit_psweight(dt_sub,y,covariate_names_full,strat_var = sv,stratum = inj)
  }
}

out <- rbindlist(results_list, use.names = TRUE, fill = TRUE)
plot_dt <- as.data.table(out)

plot_dt = apply_significance(plot_dt)

plot_dt[, Outcome := factor(Outcome,levels = outcome_vars_reg)]
plot_dt[, Stratum := factor(Stratum,levels = rev(c("All", "1L", "1L1U", "1LMU", "ML", "ML1U", "MLMU")))]

# ==== InjType-Big4 : Save data ====

fname_csv <- sprintf("tables/4_Pre_Post_ORs_%s.csv", format(Sys.Date(), "%y%m%d"))
fname_rds <- sprintf("tables/4_Pre_Post_ORs_%s.rds", format(Sys.Date(), "%y%m%d"))
fwrite(plot_dt, file.path(outdir, fname_csv))
saveRDS(plot_dt, file.path(outdir, fname_rds))

# ==== InjType-Big4 : (Optional) Load old data ====

plot_dt <- readRDS("outputs/tables/4_Pre_Post_ORs_260107.rds")

# ==== InjType-Big4 : Plot data ====

x_limits1 <- plot_dt[!is.na(LCL), .(xmin = min(LCL, na.rm = TRUE) * 0.9), by = Outcome]
x_limits1[, xmin := pmin(xmin, 0.9)] ## safety: always show OR = 1
x_limits1[, Stratum := levels(plot_dt$Stratum)[1]] ## add a dummy y so geom_blank does not inherit global y

p_or <- ggplot(
  plot_dt,aes(y = Stratum,x = OR,color = sig_class)) +
  geom_blank(data = x_limits1,aes(x = xmin, y = Stratum),inherit.aes = FALSE) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCL, xmax = UCL),height = 0.2,linewidth = 0.8) +
  geom_vline(xintercept = 1,linetype = "dotted",linewidth = 0.9,color = "black") +
  scale_x_log10(
    name = "Odds ratio (95% CI)",breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5, 1.6,1.7,1.8,1.9, 2, 3, 4)) +
  scale_color_manual(values = c("NS" = "grey",
                                "FDR-corrected P < 0.05" = "#E69F00","FDR-corrected P < 0.001" = "#00BFFF"),
                     name = NULL) +
  facet_wrap(~Outcome, ncol = 2, scales = "free_x") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    strip.text  = element_text(size = 14, color = "black"),
    legend.position = "bottom"
  )

print(p_or)

# ==== InjType-Big4 : Save plots ====

fname <- sprintf("outputs/figs/InjType/4_PrePostInjType_%s", format(Sys.Date(), "%y%m%d"))
ggsave(paste(fname,".png",sep=""), p_or, width = 6, height = 5)
ggsave(paste(fname,".pdf",sep=""), p_or, width = 6, height = 5)





# ==== All-Complications : Run models ====

results_list <- list()
for (y in complications_reg) {
  message(sprintf("Running: All Patients | Outcome = %s",y))
  res_all <- run_ps_overlap_logit_psweight(dt,y,covariate_names_full,strat_var = "All",stratum = "All")
  results_list[[length(results_list) + 1L]] <- res_all
}

out2 <- rbindlist(results_list, use.names = TRUE, fill = TRUE)

plot_dt2 <- as.data.table(out2)
plot_dt2 <- apply_significance(plot_dt2)

plot_dt2[, Outcome := factor(Outcome,levels = complications_reg)]
plot_dt2[, Stratum := factor(Stratum,levels = rev(c("All")))]

# ==== All-Complications : Save data ====

plot_dt2[Outcome=="Acute_Kidney_Injury",Outcome:="Acute kidney injury"]
plot_dt2[Outcome=="Acute_Lung_Injury_ARDS",Outcome:="Acute lung injury/ARDS"]
plot_dt2[Outcome=="Cardiac_Arrest_with_Resuscitative_Efforts",Outcome:="Cardiac arrest with resuscitative efforts"]
plot_dt2[Outcome=="CLABSI",Outcome:="Central line-associated bloodstream infection"]
plot_dt2[Outcome=="Deep_SSI",Outcome:="Deep SSI"]
plot_dt2[Outcome=="Deep_Vein_Thrombosis_DVT_Thrombophlebitis",Outcome:="Deep vein thrombosis/thrombophlebitis"]
plot_dt2[Outcome=="Drug_or_Alcohol_Withdrawal_Syndrome",Outcome:="Drug or alcohol withdrawal syndrome"]
plot_dt2[Outcome=="Extremity_Compartment_Syndrome",Outcome:="Extremity compartment syndrome"]
plot_dt2[Outcome=="MI_hc",Outcome:="Myocardial infarction"]
plot_dt2[Outcome=="Organ_Space_SSI",Outcome:="Organ/space SSI"]
plot_dt2[Outcome=="Osteomyelitis_",Outcome:="Osteomyelitis"]
plot_dt2[Outcome=="Pressure_Ulcer",Outcome:="Pressure ulcer"]
plot_dt2[Outcome=="Pulmonary_Embolism",Outcome:="Pulmonary embolism"]
plot_dt2[Outcome=="Severe_Sepsis",Outcome:="Severe sepsis"]
plot_dt2[Outcome=="Stroke_CVA",Outcome:="Stroke/CVA"]
plot_dt2[Outcome=="Superficial_Incisional_SSI",Outcome:="Superficial incisional SSI"]
plot_dt2[Outcome=="Unplanned_ICU",Outcome:="Unplanned admission to the ICU"]
plot_dt2[Outcome=="Unplanned_Intubation",Outcome:="Unplanned intubation"]
plot_dt2[Outcome=="Unplanned_OR",Outcome:="Unplanned visit to the OR"]
plot_dt2[Outcome=="UTI",Outcome:="Urinary tract infection"]
plot_dt2[Outcome=="Other_Complication_nMissing",Outcome:="Other complication"]

fname_csv <- sprintf("tables/4_Pre_Post_ORs_Complications_%s.csv", format(Sys.Date(), "%y%m%d"))
fname_rds <- sprintf("tables/4_Pre_Post_ORs_Complications_%s.rds", format(Sys.Date(), "%y%m%d"))
fwrite(plot_dt2, file.path(outdir, fname_csv))
saveRDS(plot_dt2, file.path(outdir, fname_rds))

# ==== All-Complications : (Optional) Load old data ====

plot_dt2 <- readRDS("outputs/tables/4_Pre_Post_ORs_Complications_260108.rds")

# ==== All-Complications : Plot data ====

x_limits2 <- plot_dt2[!is.na(LCL),.(xmin = min(LCL, na.rm = TRUE) * 0.9),by = Outcome]
x_limits2[, xmin := pmin(xmin, 0.9)]## safety: always show OR = 1
x_limits2[,Outcome := levels(plot_dt2$Outcome)[1]]## add a dummy y so geom_blank does not inherit global y

p_or <- ggplot(
  plot_dt2, aes(y = fct_reorder(Outcome, OR),x = OR,color = sig_class)) +
  geom_blank(data = x_limits2,aes(x = xmin, y = Outcome),inherit.aes = FALSE) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = LCL, xmax = UCL),height = 0.2,linewidth = 0.8) +
  geom_vline(xintercept = 1,linetype = "dotted",linewidth = 0.9,color = "black") +
  scale_x_log10(name = "Odds ratio (95% CI)",
    breaks = c(0.01,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2, 3, 4,5,6,7,8,9,10)) +
  scale_color_manual(
    values = c("NS" = "grey",
      "FDR-corrected P < 0.05" = "#E69F00","FDR-corrected P < 0.001" = "#00BFFF"
    ),name = NULL) +
  facet_wrap(~Stratum, ncol = 1, scales = "free_x") +
  labs(y = "Outcomes (Complications)") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    strip.text = element_blank(),
    legend.position = "bottom"
  ) 


print(p_or)

# ==== All-Complications : Save plots ====

fname <- sprintf("outputs/figs/Complics/4_PrePostComplic_%s", format(Sys.Date(), "%y%m%d"))
ggsave(paste(fname,".png",sep=""), p_or, width = 10, height = 6)
ggsave(paste(fname,".pdf",sep=""), p_or, width = 10, height = 6)



# ==== Demographics-Big4 : Run models ==== 
results_demo <- list()
for (y in outcome_vars_reg) { #iterate thru demographic variables
  for (sv in demo_vars_reg) { #iterate thru each variable's levels
    message(sprintf("Running: StratVar = %s | All Patients | Outcome = %s",sv,y))
    results_demo[[length(results_demo) + 1L]] <- 
      run_ps_overlap_logit_psweight(dt,y,covariate_names_full,strat_var = sv,stratum = "All")
    
    levs <- levels(dt[[sv]])
    for (lv in levs) { #iterate thru 4 outcomes
      dt_sub <- dt[get(sv) == lv]
      message(sprintf("Running: StratVar = %s | Stratum = %s | Outcome = %s",sv, lv, y))
      results_demo[[length(results_demo) + 1L]] <- 
        run_ps_overlap_logit_psweight(dt_sub,y,covariate_names_full,strat_var = sv,stratum = lv)
    }
  }
}
plot_demo_dt <- rbindlist(results_demo, use.names = TRUE, fill = TRUE)

plot_demo_dt = apply_significance(plot_demo_dt)

plot_demo_dt[, Outcome := factor(Outcome, levels = outcome_vars_reg)]
plot_demo_dt[, StratVar := factor(StratVar, levels = demo_vars_reg)]

# ==== Demographics-Big4 : Save data ====

plot_demo_dt[StratVar=="Hispanic_Ethnicity"&Stratum=="TRUE",Stratum:="Hispanic"]
plot_demo_dt[StratVar=="Hispanic_Ethnicity"&Stratum=="FALSE",Stratum:="Non-Hispanic"]
plot_demo_dt[StratVar=="Primary_Payment_Method"&Stratum=="Medicare, Medicaid, or Government",Stratum:="Government Insurance"]

fname_csv <- sprintf("tables/4_Pre_Post_ORs_disparities_ato_%s.csv", format(Sys.Date(), "%y%m%d"))
fname_rds <- sprintf("tables/4_Pre_Post_ORs_disparities_ato_%s.rds", format(Sys.Date(), "%y%m%d"))

fwrite(plot_demo_dt, file.path(outdir, fname_csv))
saveRDS(plot_demo_dt, file.path(outdir, fname_rds))

# ==== Demographics-Big4 : (Optional) Load old data ====
#*** or comment this out!
plot_demo_dt <- readRDS("outputs/tables/4_Pre_Post_ORs_disparities_ato_260108.rds")

# ==== Demographics-Big4 : Plot data ====
x_breaks <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1,1.2,1.3,1.4,1.5, 1.6,1.7,1.8,1.9,2, 3, 4)

x_limits_outcome <- plot_demo_dt[
  !is.na(LCL) & !is.na(UCL),
  .(xmin = min(LCL) * 0.8,
    xmax = max(UCL) * 1.2),by = Outcome]

## safety: always include OR = 1
x_limits_outcome[, xmin := pmin(xmin, 0.9)]
x_limits_outcome[, xmax := pmax(xmax, 1.1)]

## give a valid discrete y level
x_limits_outcome[, Stratum := "All"]

make_demo_panel <- function(plot_dt, strat_var, show_x = FALSE, show_strip = TRUE) {
  
  dd <- plot_dt[StratVar == strat_var]
  
  ## y order
  levs <- levels(factor(setdiff(unique(dd$Stratum),"All")))
  print(levs)
  dd[, Stratum := factor(Stratum, levels = rev(c("All", levs)))]
  
  ## split by Outcome so we can apply per-Outcome limits
  plots <- lapply(split(dd, dd$Outcome), function(dsub) {
    
    lims <- x_limits_outcome[Outcome == unique(dsub$Outcome)]
    is_first <- unique(dsub$Outcome) == outcome_vars_reg[1]
    
    ggplot(dsub, aes(y = Stratum, x = OR, color = sig_class)) +
      geom_point(size = 3) +
      geom_errorbar(aes(xmin = LCL, xmax = UCL),
                    height = 0.2, linewidth = 0.8) +
      geom_vline(xintercept = 1, linetype = "dotted", linewidth = 0.9) +
      scale_x_log10(
        limits = c(lims$xmin, lims$xmax),
        breaks = x_breaks,
        name = NULL
      ) +
      scale_color_manual(
        values = c(
          "NS" = "grey",
          "FDR-corrected P < 0.05" = "#E69F00",
          "FDR-corrected P < 0.001" = "#00BFFF"
        ),name = NULL) +
      labs(y = NULL, title = unique(dsub$Outcome)) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.8),
        axis.text.x        = element_text(size = 11),
        axis.text.y  = if (is_first) element_text(size = 11) else element_blank(),
        axis.title.y = if (is_first) element_text(size = 12) else element_blank(),
        axis.ticks.y = element_line(),
        axis.ticks         = element_line(),
        #plot.title         = if (show_strip)
        #  element_text(size = 12, face = "bold")
        #else element_blank(),
        plot.title = if (show_strip)
          element_text(
            size = 12,
            face = "bold",
            hjust = 0.5        # center text
          )
        else element_blank(),
        legend.position    = "none"
      ) +
      (if (!show_x) theme(axis.text.x = element_blank()) else labs(x = "Odds ratio (95% CI)"))
  })
  
  ## combine outcomes into one row (columns aligned)
  wrap_plots(plots, nrow = 1)
}

p_list <- vector("list", length(demo_vars_reg))
names(p_list) <- demo_vars_reg

for (i in seq_along(demo_vars_reg)) {
  sv <- demo_vars_reg[i]
  p_list[[sv]] <- make_demo_panel(
    plot_dt   = plot_demo_dt,
    strat_var = sv,
    show_x    = (i == length(demo_vars_reg)),  # x-axis title only on bottom
    show_strip = (i == 1)                  # facet titles only on top row
  )
}

p_demographics <- wrap_plots(p_list, ncol = 1, guides = "collect") &
  theme(legend.position = "none")

print(p_demographics)

# ==== Demographics-Big4 : Save plots ====

fname <- sprintf("outputs/figs/disparities/4_PrePostDisparities_%s", format(Sys.Date(), "%y%m%d"))
ggsave(paste(fname,".png",sep=""), p_demographics, width = 12, height = 7)
ggsave(paste(fname,".pdf",sep=""), p_demographics, width = 12, height = 7)


# ==== Combine &Save ALL data ====

all_results = rbindlist(list(plot_dt, plot_dt2,plot_demo_dt), use.names = TRUE, fill = TRUE)

fname_csv <- sprintf("tables/4_all_overlap_results_%s.csv", format(Sys.Date(), "%y%m%d"))
fname_rds <- sprintf("tables/4_all_overlap_results_%s.rds", format(Sys.Date(), "%y%m%d"))

fwrite(all_results, file.path(outdir, fname_csv))
saveRDS(all_results, file.path(outdir, fname_rds))
