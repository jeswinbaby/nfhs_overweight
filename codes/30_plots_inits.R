rm(list = setdiff(ls(), glo_vars))
nfhs_wm <- readRDS(paste0(data_path, "nfhs_wm_v2.rds"))
nfhs_wm <- data.frame(nfhs_wm)
cat_data <- c("ovrwt"
              , "parity"
              , "state"
              , "round"
              , "psu"
              , "age_category"
              , "education"
              , "caste"
              , "wealth_index"
              , "wealth_index_sm"
              , "wealth_index_stateround"
              , "wealth_index_comb"
              , "residence_type"
)
nfhs_wm[,cat_data] <- lapply(nfhs_wm[,cat_data], as.factor)
ind_vars <- c("education", "age_category", "caste", "wealth_index_sm","parity")
eco_vars <- c("gdp", "lfpr", "literacy", "hdi", "physical_sedentary")
ntr_vars <- c("oils", "sugar", "cereal")
rnd_vars <- c("state", "psu", "individual")
mod_vars <- c("DIC")
library("lattice")
specs = c(width = 1200, height = 800, pointsize = 12)

## Convention
# dataset per figure (create a .R file for each figure's dataset)
#- Store .R file in overweight-analysis/code_support/...(30's series)
#- Store dataset in SJRI_Emory_Collaboration/Project1_ObesityTrendsNFHS/Analysis/Plots
#2. Generate each figure with the following specification
#- Point size >=12pt
#- Figure width = 1200 px (800 px if square)
#- Figure height = 800 px
#- DPI : 300
# 3. Figure checklist:
#- X axis (Sentence case with appropriate units)
#- Y axis (Sentence case with appropriate units)
#- Legend (Sentence case)

library(dplyr)
library(ggplot2)
library(ggrepel)

MySpecial <- list(
  # move the x axis labels up top
  scale_x_discrete(position = "top"),
  theme_bw(),
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none"),
  # Remove the panel border
  theme(panel.border     = element_blank()),
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()),
  theme(axis.text.y      = element_blank()),
  theme(panel.grid.major.y = element_blank()),
  theme(panel.grid.minor.y = element_blank()),
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()),
  theme(panel.grid.major.x = element_blank()),
  theme(axis.text.x.top      = element_text(size=12)),
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()),
  # Format title & subtitle
  theme(plot.title       = element_text(size=12, face = "bold", hjust = 0.5)),
  theme(plot.subtitle    = element_text(hjust = 0.5))
)
