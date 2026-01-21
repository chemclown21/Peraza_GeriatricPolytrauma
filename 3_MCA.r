# 3_MCA.r for Peraza. Evolving Outcomes in Geriatric Patients with Orthopedic Polyfractures: the NTDB 2008-2023  
# Written by Vitto Resnick

# Multiple correspondence analysis

# ==== Load data ====
files_directory = '/Users/vresnick/Documents/GitHub/Peraza2026_GeriatricPolyTrauma'
setwd(files_directory)
outdir = "outputs"
dt <- readRDS("outputs/datasets/1_Pre_Post_stitched_decoded_expandedFemur_purged_260120.rds")

# ==== MCA ====
mca_theme <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
  axis.line = element_blank(),   # panel.border handles outline
  axis.ticks = element_blank(),#element_line(color = "black"),
  axis.text = element_text(color = "black"),
  axis.title = element_blank()#element_text(color = "black")
)
colors2 <- c(
  "1L"   = "#56B4E9","1L1U" = "#CC79A7",
  "1LMU" = "#009E73","ML"   = "#E69F00",
  "ML1U" = "mediumorchid1","MLMU" = "violetred2"
)

# Columns for MCA
covariate_names <- c("Guidelines",covariate_names_full)
present_keep <- covariate_names[covariate_names %chin% names(dt)]
missing_keep <- setdiff(covariate_names, present_keep)
if (length(missing_keep)) message("Missing columns (skipped): ", paste(missing_keep, collapse = ", "))
dt4MCA <- dt[, ..present_keep]

# Sample 
sub <- copy(dt4MCA)[sample(.N)]

# Keep only categorical columns
is_cat <- vapply(sub, function(x) is.factor(x) || is.character(x) || is.logical(x), logical(1))
sub <- sub[, which(is_cat), with = FALSE]

# Coerce to factor
for (nm in names(sub)) {
  if (is.character(sub[[nm]]) || is.logical(sub[[nm]])) sub[[nm]] <- factor(sub[[nm]])
}

# Drop sparse factor levels (<1% of observations)
min_prop <- 0.01
skip_vars <- "New_InjType"
for (nm in setdiff(names(sub), skip_vars)) {
  tab <- table(sub[[nm]])
  keep_lvls <- names(tab)[tab / sum(tab) >= min_prop]
  # convert rare levels to NA, then drop
  sub[[nm]][!sub[[nm]] %in% keep_lvls] <- NA
  sub[[nm]] <- droplevels(sub[[nm]])
}

# Drop degenerate factors (≤1 level or all NA)
keep <- vapply(sub, function(x) {
  lx <- levels(x)
  length(lx[!is.na(lx)]) > 1 && sum(!is.na(x)) > 0
}, logical(1))
sub <- sub[, names(keep)[keep], with = FALSE]

color_vars <- c("Guidelines", "New_InjType")
for (cv in color_vars) {
  if (!cv %in% names(sub) && cv %in% names(dt4MCA)) {
    sub[, (cv) := dt4MCA[[cv]] ]
    if (!is.factor(sub[[cv]])) sub[[cv]] <- factor(sub[[cv]])
  } else if (cv %in% names(sub) && !is.factor(sub[[cv]])) {
    sub[[cv]] <- factor(sub[[cv]])
  }
}

# indices of supplementary qualitative variables
quali_sup_idx <- match(intersect(color_vars, names(sub)), names(sub))

# convert to plain data.frame for FactoMineR
sub_df <- as.data.frame(sub)

# run MCA
mca_obj <- MCA(sub_df, graph = FALSE,
               quali.sup = quali_sup_idx)  # let color vars be supplementary

# plot
p <- fviz_mca_ind(mca_obj,
    geom = "point",habillage = "Guidelines",
    addEllipses = FALSE,repel = TRUE,alpha.ind = 0.4,pointsize = 1.2
)
ggsave("outputs/figs/MCAs/3_MCA_guidelines.png", p, width = 10, height = 6,dpi = 600)
ggsave("outputs/figs/MCAs/3_MCA_guidelines.pdf", p + mca_theme, width = 10, height = 6,dpi = 600)

p <- fviz_mca_ind(mca_obj,geom = "point",habillage = "New_InjType",
    addEllipses = FALSE,repel = TRUE,alpha.ind = 0.4,palette = colors2,pointsize = 1.2,   # smaller dots
) + mca_theme
ggsave("outputs/figs/MCAs/3_MCA_InjType.png", p, width = 10, height = 6,dpi = 600)
ggsave("outputs/figs/MCAs/3_MCA_InjType.pdf", p, width = 10, height = 6,dpi = 600)

# get separate components for figure making (guidelines)
p_base <- fviz_mca_ind(mca_obj,geom = "point",habillage = "Guidelines",
  addEllipses = FALSE,repel = TRUE,alpha.ind = 0.4,pointsize = 1.2) + mca_theme
p_points_only <- p_base +
  theme(axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )
ggsave("outputs/figs/MCAs/3_MCA_guidelines_points.png",
  p_points_only,width = 10,height = 6,dpi = 600,bg = "transparent")

p_axes_only <- p_base
p_axes_only$layers <- list()  # remove all geoms (points)
ggsave("outputs/figs/MCAs/3_MCA_guidelines_axes.pdf",p_axes_only,width = 10,height = 6)

# get separate components for figure making (inj type)
p_base <- fviz_mca_ind(mca_obj, geom = "point",habillage = "New_InjType",
  addEllipses = FALSE,repel = TRUE,
  alpha.ind = 0.4,palette = colors2,pointsize = 1.2) + mca_theme
p_points_only <- p_base +
  theme(axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )
ggsave("outputs/figs/MCAs/3_MCA_injtype_points.png",
  p_points_only,width = 10,height = 6,dpi = 600,bg = "transparent")

p_axes_only <- p_base
p_axes_only$layers <- list()  # remove all geoms (points)
ggsave("outputs/figs/MCAs/3_MCA_injtype_axes.pdf",p_axes_only,width = 10,height = 6)


# Eta^2 table 
eta <- as.data.frame(mca_obj$var$eta2)
eta$variable <- rownames(eta)
head(eta[order(-eta[,"Dim 1"]), c("variable","Dim 1")], 15)
head(eta[order(-eta[,"Dim 2"]), c("variable","Dim 2")], 15)