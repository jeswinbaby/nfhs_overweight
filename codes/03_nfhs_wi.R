rm(list = setdiff(ls(), glo_vars))
nfhs4_hh_ide <- data.frame(readRDS(paste0(nfhs4_path
                                          ,"working/rds/nfhs4_h_identification.rds")))
nfhs3_hh_ide <- data.frame(readRDS(paste0(nfhs3_path
                                          ,"working/rds/nfhs3_h_identification.rds")))
nfhs2_hh_ide <- data.frame(readRDS(paste0(nfhs2_path
                                          ,"working/rds/nfhs2_h_identification.rds")))
#nfhs4_hh_ide$hhid <- str_pad(nfhs4_hh_ide$hhid, width = 8)
wi_map <- read_excel(paste0(data_path
                            ,"support_data/pca_var_map.xlsx"), sheet = "overall")
nfhs2_hh <- nfhs2_hh_ide[,c("hhid", "hv005", "hv025", "hv024", wi_map$nfhs_2)]
nfhs3_hh <- nfhs3_hh_ide[,c("hhid", "hv005", "hv025", "hv024", wi_map$nfhs_3)]
nfhs4_hh <- nfhs4_hh_ide[,c("hhid", "hv005", "hv025", "hv024", wi_map$nfhs_4)]
colnames(nfhs2_hh) = c("hhid", "hv005", "hv025", "hv024", wi_map$variable)
colnames(nfhs3_hh) = c("hhid", "hv005", "hv025", "hv024", wi_map$variable)
colnames(nfhs4_hh) = c("hhid", "hv005", "hv025", "hv024", wi_map$variable)
tlt_map <- read_excel(paste0(data_path
                             ,"support_data\\pca_var_map.xlsx"), sheet = "toilet")
nfhs2_hh <- merge(nfhs2_hh, tlt_map[,c("nfhs2", "tag")], by.x = "toilet", by.y = "nfhs2",all.x = T)
nfhs3_hh <- merge(nfhs3_hh, tlt_map[,c("nfhs3", "tag")], by.x = "toilet", by.y = "nfhs3",all.x = T)
nfhs4_hh <- merge(nfhs4_hh, tlt_map[,c("nfhs4", "tag")], by.x = "toilet", by.y = "nfhs4",all.x = T)
nfhs2_hh$toilet <- nfhs2_hh$tag
nfhs3_hh$toilet <- nfhs3_hh$tag
nfhs4_hh$toilet <- nfhs4_hh$tag
nfhs2_hh <- nfhs2_hh[, -ncol(nfhs2_hh)]
nfhs3_hh <- nfhs3_hh[, -ncol(nfhs3_hh)]
nfhs4_hh <- nfhs4_hh[, -ncol(nfhs4_hh)]

wtr_map <- read_excel(paste0(data_path
                             ,"support_data/pca_var_map.xlsx"), sheet = "water")
nfhs2_hh <- merge(nfhs2_hh, wtr_map[,c("nfhs2", "tag")], by.x = "water", by.y = "nfhs2",all.x = T)
nfhs3_hh <- merge(nfhs3_hh, wtr_map[,c("nfhs3", "tag")], by.x = "water", by.y = "nfhs3",all.x = T)
nfhs4_hh <- merge(nfhs4_hh, wtr_map[,c("nfhs4", "tag")], by.x = "water", by.y = "nfhs4",all.x = T)
nfhs2_hh$water <- nfhs2_hh$tag
nfhs3_hh$water <- nfhs3_hh$tag
nfhs4_hh$water <- nfhs4_hh$tag
nfhs2_hh <- nfhs2_hh[, -ncol(nfhs2_hh)]
nfhs3_hh <- nfhs3_hh[, -ncol(nfhs3_hh)]
nfhs4_hh <- nfhs4_hh[, -ncol(nfhs4_hh)]

ckg_map <- read_excel(paste0(data_path
                             ,"support_data/pca_var_map.xlsx"), sheet = "cooking")
nfhs2_hh <- merge(nfhs2_hh, ckg_map[,c("nfhs2", "tag")], by.x = "cooking", by.y = "nfhs2",all.x = T)
nfhs3_hh <- merge(nfhs3_hh, ckg_map[,c("nfhs3", "tag")], by.x = "cooking", by.y = "nfhs3",all.x = T)
nfhs4_hh <- merge(nfhs4_hh, ckg_map[,c("nfhs4", "tag")], by.x = "cooking", by.y = "nfhs4",all.x = T)
nfhs2_hh$cooking <- nfhs2_hh$tag
nfhs3_hh$cooking <- nfhs3_hh$tag
nfhs4_hh$cooking <- nfhs4_hh$tag
nfhs2_hh <- nfhs2_hh[, -ncol(nfhs2_hh)]
nfhs3_hh <- nfhs3_hh[, -ncol(nfhs3_hh)]
nfhs4_hh <- nfhs4_hh[, -ncol(nfhs4_hh)]

nfhs2_hh$round = 2
nfhs3_hh$round = 3
nfhs4_hh$round = 4
nfhs_hh <- rbind(nfhs2_hh, nfhs3_hh)
nfhs_hh <- rbind(nfhs_hh, nfhs4_hh)
nfhs_hh[is.na(nfhs_hh)] = 0
nfhs_hh$hv005 <- nfhs_hh$hv005 / 1000000

#dummy variable
vars <- setdiff(wi_map$variable,c("toilet", "cooking","water"))
library(varhandle)
#cooking
strt = length(nfhs_hh) + 1
end = length(nfhs_hh) + length(unique(nfhs_hh$cooking)) - 1
dd = as.data.frame(to.dummy(nfhs_hh$cooking, "cooking")[,-1])
colnames(dd) = gsub(".", '_',colnames(dd), fixed = T)
vars <- c(vars, colnames(dd))
nfhs_hh[, strt:end] = dd
#water
strt = length(nfhs_hh) + 1
end = length(nfhs_hh) + length(unique(nfhs_hh$water)) - 1
dd = as.data.frame(to.dummy(nfhs_hh$water, "water")[,-1])
colnames(dd) = gsub(".", '_',colnames(dd), fixed = T)
vars <- c(vars, colnames(dd))
nfhs_hh[, strt:end] = dd
#toilet
strt = length(nfhs_hh) + 1
end = length(nfhs_hh) + length(unique(nfhs_hh$toilet)) - 1
dd = as.data.frame(to.dummy(nfhs_hh$toilet, "toilet")[,-1])
colnames(dd) = gsub(".", '_',colnames(dd), fixed = T)
vars <- c(vars, colnames(dd))
nfhs_hh[, strt:end] = dd

#PCA- National
gg <- princomp(nfhs_hh[,vars])
nfhs_hh$score <- predict(gg,nfhs_hh)[,1]

#PCA- Urban
nfhs_hh_urb = nfhs_hh[nfhs_hh$hv025 == 1,]
gg <- princomp(nfhs_hh_urb[,vars])
nfhs_hh_urb$score_urb <- predict(gg,nfhs_hh_urb)[,1]

#PCA- Rural
nfhs_hh_rur = nfhs_hh[nfhs_hh$hv025 == 2,]
gg <- princomp(nfhs_hh_rur[,vars])
nfhs_hh_rur$score_rur <- predict(gg,nfhs_hh_rur)[,1]

#level and scaling parameters
fit1 <- lm(score ~ score_urb, data = nfhs_hh_urb)
fit2 <- lm(score ~ score_rur, data = nfhs_hh_rur)

#Rescaling the score
nfhs_hh_urb$wealth_score =   fit1$coefficients[1] + (fit1$coefficients[2]*nfhs_hh_urb$score_urb)
nfhs_hh_rur$wealth_score =   fit2$coefficients[1] + (fit2$coefficients[2]*nfhs_hh_rur$score_rur)

#combined data
nfhs_wi <- rbind(nfhs_hh_urb[,c("hhid", "wealth_score", "round","hv024")]
                 , nfhs_hh_rur[,c("hhid", "wealth_score", "round","hv024")])

################################################################################
# Wealth Index creation
################################################################################
# 1. wealth_index_sm
# Round specific but not state specific will account for round level changes but
# not for absolute state-level differences

nfhs_wi <- nfhs_wi %>%
  group_by(round) %>%
  dplyr::mutate(wealth_index_sm = cut(wealth_score,
                                              breaks = quantile(wealth_score
                                                                , probs = seq(0,1, by = 0.2)
                                                                , na.rm = TRUE),
                                              labels = 1:5,
                                              include.lowest = TRUE)) %>%
  ungroup()

# 2. wealth_index_state_round
# Round and state specific will account for round and state level changes

nfhs_wi <- nfhs_wi %>%
  group_by(round, hv024) %>%
  dplyr::mutate(wealth_index_stateround = cut(wealth_score,
                                              breaks = quantile(wealth_score
                                                                , probs = seq(0,1, by = 0.2)
                                                                , na.rm = TRUE),
                                              labels = 1:5,
                                              include.lowest = TRUE)) %>%
  ungroup()

# 3. wealth_index_comb
# Round and state independent won't account for round or state level changes

nfhs_wi <- nfhs_wi %>%
  dplyr::mutate(wealth_index_comb = cut(wealth_score,
                                              breaks = quantile(wealth_score
                                                                , probs = seq(0,1, by = 0.2)
                                                                , na.rm = TRUE),
                                              labels = 1:5,
                                              include.lowest = TRUE))

saveRDS(nfhs_wi, paste0(data_path,"support_data\\nfhs_wi.RDS"))
