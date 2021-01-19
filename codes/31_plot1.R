################################################################################
#Statewise trends in obesity
################################################################################
source("code_support\\30_plots_inits.R")

#Data Preparation
library(readxl)
nfhs_wm$ovrwt <- as.numeric(as.character(nfhs_wm$ovrwt))
tdat = list()
tdat[["National"]] = group_by(nfhs_wm, round) %>%
  summarise(National = round(wtd.mean(ovrwt, weights = sample_weight),2))
tdat[["National"]]$state <- "National"
tdat[["Statewise"]] = group_by(nfhs_wm, state, round) %>%
  summarise(National = round(wtd.mean(ovrwt, weights = sample_weight),2))
tdat <- rbindlist(tdat, use.names = T)

udat = list()
udat[["National"]] = group_by(nfhs_wm[nfhs_wm$residence_type == 1,], round) %>%
  summarise(Urban = round(wtd.mean(ovrwt, weights = sample_weight),2))
udat[["National"]]$state <- "National"
udat[["Statewise"]] = group_by(nfhs_wm[nfhs_wm$residence_type == 1,], state, round) %>%
  summarise(Urban = round(wtd.mean(ovrwt, weights = sample_weight),2))
udat <- rbindlist(udat, use.names = T)

rdat = list()
rdat[["National"]] = group_by(nfhs_wm[nfhs_wm$residence_type == 2,], round) %>%
  summarise(Rural = round(wtd.mean(ovrwt, weights = sample_weight),2))
rdat[["National"]]$state <- "National"
rdat[["Statewise"]] = group_by(nfhs_wm[nfhs_wm$residence_type == 2,], state, round) %>%
  summarise(Rural = round(wtd.mean(ovrwt, weights = sample_weight),2))
rdat <- rbindlist(rdat, use.names = T)

tdat <- merge(tdat,udat, by = c("state", "round"))
tdat <- merge(tdat,rdat, by = c("state", "round"))

tdat$round <- as.factor(tdat$round)
nfhs2_st_map <- read_excel(paste0(data_path, "support_data\\state_map.xlsx")
                           , sheet = "nfhs_2")
nfhs2_st_map$nfhs_2 <- as.character(nfhs2_st_map$nfhs_2)
tdat <- merge(tdat, nfhs2_st_map, by.x = "state", by.y = "nfhs_2", all.x = T)
tdat$state.y[tdat$state == "National"] <- "National"
tdat$state.y <- as.factor(tdat$state.y)
tdat$state.y <- factor("National"
                       , levels = c(sort(unique(tdat$state.y[tdat$state.y != "National"]), decreasing = F)))
tdat <- tdat[order(tdat$state.y, tdat$round, decreasing = FALSE),]
library(forcats)
tdat$state.y <- fct_relevel(tdat$state.y,"National")
saveRDS(tdat, paste0(data_path,"plot_data\\plot1_data.rds"))

#Plotting
tdat <- readRDS(paste0(data_path,"plot_data\\plot1_data.rds"))
tiff(paste0(plot_path,"fig1.tif"),width = 234,height=291
     ,units = "mm",res = 300,pointsize = 1, compression = "lzw")
xyplot(National + Urban + Rural ~ round | state_name,
       data = tdat,
       type = "l",
       lty = c(1,2,3),
       lwd = 2, as.table = T,
       col.line = "gray23"
       , layout = c(3,9)
       , xlab = "Round"
       , ylab = "Prevalence"
       , key = list(space = "right",
                    lines = list(col = "gray33", lty = c(1,2,3), lwd = 2),
                    text = list(c("Overall", "Urban", "Rural"))
       ), strip = strip.custom(bg = "white", par.strip.text = list(font = 2)))
dev.off()
