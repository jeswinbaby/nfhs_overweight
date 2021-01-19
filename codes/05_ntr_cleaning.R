rm(list = setdiff(ls(), glo_vars))
#Consumption data
nss55_ide <- readRDS(paste0(nss55_path, "working/rds_files/r5501_identification_l01_v1.RDS"))
nss55_val <- readRDS(paste0(nss55_path, "working/rds_files/r5501_total_consumption_value_30_rs_unadj_v1.RDS"))
nss55_ntr <- readRDS(paste0(nss55_path, "working/rds_files/r5501_30_nutrient_v1.RDS"))
nss55_mem <- readRDS(paste0(nss55_path, "working/rds_files/r5501_members_v1.RDS"))
nss55_qty <- readRDS(paste0(nss55_path,"working/rds_files/r5501_total_consumption_qty_30_100g_unadj_v1.RDS"))
food_grp_map <- read_excel(paste0(nss55_path,"data_guide/layout/nss55_1_0_layout.xlsx"),
                           sheet = "food_group_final")

#Creating consumption data
nss55_val$tot_val <- rowSums(nss55_val[,-1], na.rm = T)
nss55_hh <- merge(nss55_ide[,c("hhuid","d_state", "sector", "final_weight")]
                  , nss55_val[,c("hhuid","tot_val")]
                  , by = "hhuid")

#selecting relevant nutrients
macro_ntr <- c("Carbohydrate.CHOAVLDF", "Protein.PROTCNT", "TotalFat.FATCE", "Energy.ENERKC")
nss55_ntr <- nss55_ntr[,c("hhuid", macro_ntr)]

quant1 <- quantile(nss55_ntr$Energy.ENERKC, 0.01)
quant99<- quantile(nss55_ntr$Energy.ENERKC, 0.99)
nss55_ntr <- nss55_ntr[nss55_ntr$Energy.ENERKC >= quant1,]
nss55_ntr <- nss55_ntr[nss55_ntr$Energy.ENERKC <= quant99,]

#computing nutrients as a percentage of energy
nss55_ntr$carbohydrate_percent <- nss55_ntr$Carbohydrate.CHOAVLDF * 4 / nss55_ntr$Energy.ENERKC * 100
nss55_ntr$protein_percent <- nss55_ntr$Protein.PROTCNT * 4 / nss55_ntr$Energy.ENERKC * 100
nss55_ntr$fat_percent <- nss55_ntr$TotalFat.FATCE * 9 / nss55_ntr$Energy.ENERKC * 100

#Computing whether fat consumption exceeds limit
nss55_ntr$fat_lmt <- ifelse(nss55_ntr$fat_percent >= 20, 1, 0)

#Creating percapita nutrient consumption data
nss55_ntr <- merge(nss55_ntr, nss55_mem[,c("hhuid", "consumer_units")], by = "hhuid")
nss55_ntr[,macro_ntr] <- nss55_ntr[,macro_ntr]/nss55_ntr$consumer_units
nss55_hh <- merge(nss55_hh, nss55_ntr[,c("hhuid", macro_ntr, "carbohydrate_percent", "protein_percent", "fat_percent", "fat_lmt")], by = "hhuid", all = T)

#calculating food group intake
req_food_grps <- c("Sugar..jaggery", "Rice", "Wheat", "Vegetable.oils","Butter.ghee", "Transfat")
nss55_qty[is.na(nss55_qty)] <- 0
food_grp_map[is.na(food_grp_map)] <- 0
nss55_qty_m <- as.matrix(nss55_qty[,c(2:163)])
food_grp_map <- as.matrix(food_grp_map[,c(3:28)])
nss55_food_grp <- nss55_qty_m %*% food_grp_map %>% data.frame()
nss55_food_grp$hhuid <- nss55_qty$hhuid

nss55_food_grp <- nss55_food_grp[!is.infinite(nss55_food_grp$Sugar..jaggery),]
nss55_food_grp <- merge(nss55_food_grp, nss55_mem[,c("hhuid", "consumer_units")], by = "hhuid")
nss55_food_grp[,req_food_grps] <- nss55_food_grp[,req_food_grps]/nss55_food_grp$consumer_units
nss55_food_grp$sugar <- nss55_food_grp$Sugar..jaggery
nss55_food_grp$cereal <- nss55_food_grp$Rice + nss55_food_grp$Wheat
nss55_food_grp$oils <- nss55_food_grp$Vegetable.oils +
  nss55_food_grp$Butter.ghee +
  nss55_food_grp$Transfat

nss55_hh <- merge(nss55_hh, nss55_food_grp[,c("hhuid", "sugar", "cereal", "oils")], by = "hhuid", all = T)

#Harmonizing with nfhs 2 state codes
nss55_st_map <- read_excel(paste0(data_path, "support_data\\state_map.xlsx"), sheet = "nss55")
nss55_hh <- merge(nss55_hh, nss55_st_map, by.x = "d_state", by.y = "nss55")

#removing energy outliers
nss55_hh = nss55_hh[!is.infinite(nss55_hh$Energy.ENERKC),]

#removing outliers
vv <- c("sugar"
        , "cereal"
        , "oils")

cons = data.table()
for(i in unique(nss55_hh$nfhs2)) {
  data <- nss55_hh[nss55_hh$nfhs2== i,]
  temp = data.table(state = i)
  for (j in vv){
    low <- quantile(data[,j],0.01,na.rm = T)
    up <- quantile(data[,j],0.99,na.rm = T)
    filt <- data[between(data[,j], low, up),]
    temp[,j] <- round(wtd.mean(filt[,j]
                               , filt[,"final_weight"],na.rm = T), 2)
    temp[,paste0("rur_",j)] <- round(wtd.mean(filt[filt$sector == 1,j]
                               , filt[filt$sector == 1,"final_weight"],na.rm = T), 2)
    temp[,paste0("urb_",j)] <- round(wtd.mean(filt[filt$sector == 2,j]
                               , filt[filt$sector == 2,"final_weight"],na.rm = T), 2)
  }
  cons <- rbind(cons,temp)
}

#Merging with original data
ntr2 = cons

#Consumption Data
nss61_ide <- readRDS(paste0(nss61_path, "working/rds_files/r6101_identification_v1.RDS"))
nss61_val <- readRDS(paste0(nss61_path, "working/rds_files/r6101l05_total_consumption_val_in_rs_unadj_v1.RDS"))
nss61_ntr <- readRDS(paste0(nss61_path, "working/rds_files/r6101_nutrient_v1.RDS"))
nss61_mem <- readRDS(paste0(nss61_path, "working/rds_files/r6101_members_v1.RDS"))
nss61_qty <- readRDS(paste0(nss61_path,"working/rds_files/r6101l05_total_consumption_qty_in_100g_adj_v1.RDS"))
food_grp_map <- read_excel(paste0(nss61_path,"data_guide/layout/nss61_1_0_layout.xlsx"),
                           sheet = "food_group_final")

#Creating consumption data
nss61_val$tot_val <- rowSums(nss61_val[,-1], na.rm = T)
nss61_hh <- merge(nss61_ide[,c("hhuid","d_state", "sector", "final_weight")]
                  , nss61_val[,c("hhuid","tot_val")]
                  , by = "hhuid")

#selecting relevant nutrients
macro_ntr <- c("Carbohydrate.CHOAVLDF", "Protein.PROTCNT", "TotalFat.FATCE", "Energy.ENERKC")
nss61_ntr <- nss61_ntr[,c("hhuid", macro_ntr)]

quant1 <- quantile(nss61_ntr$Energy.ENERKC, 0.01)
quant99<- quantile(nss61_ntr$Energy.ENERKC, 0.99)
nss61_ntr <- nss61_ntr[nss61_ntr$Energy.ENERKC >= quant1,]
nss61_ntr <- nss61_ntr[nss61_ntr$Energy.ENERKC <= quant99,]

#computing nutrients as a percentage of energy
nss61_ntr$carbohydrate_percent <- nss61_ntr$Carbohydrate.CHOAVLDF * 4 / nss61_ntr$Energy.ENERKC * 100
nss61_ntr$protein_percent <- nss61_ntr$Protein.PROTCNT * 4 / nss61_ntr$Energy.ENERKC * 100
nss61_ntr$fat_percent <- nss61_ntr$TotalFat.FATCE * 9 / nss61_ntr$Energy.ENERKC * 100

#Computing whether fat consumption exceeds limit
nss61_ntr$fat_lmt <- ifelse(nss61_ntr$fat_percent >= 20, 1, 0)

#Creating percapita nutrient consumption data
nss61_ntr <- merge(nss61_ntr, nss61_mem[,c("hhuid", "consumer_units")], by = "hhuid")
nss61_ntr[,macro_ntr] <- nss61_ntr[,macro_ntr]/nss61_ntr$consumer_units
nss61_hh <- merge(nss61_hh, nss61_ntr[,c("hhuid", macro_ntr,"carbohydrate_percent", "protein_percent", "fat_percent", "fat_lmt")], by = "hhuid", all = T)

#calculating food group intake
req_food_grps <- c("Sugar..jaggery", "Rice", "Wheat", "Vegetable.oils","Butter.ghee", "Transfat")
nss61_qty[is.na(nss61_qty)] <- 0
food_grp_map[is.na(food_grp_map)] <- 0
nss61_qty_m <- as.matrix(nss61_qty[,c(2:163)])
food_grp_map <- as.matrix(food_grp_map[,c(3:28)])
nss61_food_grp <- nss61_qty_m %*% food_grp_map %>% data.frame()
nss61_food_grp$hhuid <- nss61_qty$hhuid

nss61_food_grp <- merge(nss61_food_grp, nss61_mem[,c("hhuid", "consumer_units")], by = "hhuid")
nss61_food_grp[,req_food_grps] <- nss61_food_grp[,req_food_grps]/nss61_food_grp$consumer_units
nss61_food_grp$sugar <- nss61_food_grp$Sugar..jaggery
nss61_food_grp$cereal <- nss61_food_grp$Rice + nss61_food_grp$Wheat
nss61_food_grp$oils <- nss61_food_grp$Vegetable.oils +
  nss61_food_grp$Butter.ghee +
  nss61_food_grp$Transfat
nss61_hh <- merge(nss61_hh
                  , nss61_food_grp[,c("hhuid", "sugar", "cereal", "oils")]
                  , by = "hhuid"
                  , all = T)

#Harmonizing with nfhs 2 state codes
nss61_st_map <- read_excel(paste0(data_path, "support_data\\state_map.xlsx"), sheet = "nss61")
nss61_hh <- merge(nss61_hh, nss61_st_map, by.x = "d_state", by.y = "nss61")

#removing energy outliers
nss61_hh = nss61_hh[!is.infinite(nss61_hh$Energy.ENERKC),]

#removing outliers
vv <- c("sugar", "cereal", "oils")

cons = data.table()
for(i in unique(nss61_hh$nfhs2)) {
  data <- nss61_hh[nss61_hh$nfhs2== i,]
  temp = data.table(state = i)
  for (j in vv){
    low <- quantile(data[,j],0.01,na.rm = T)
    up <- quantile(data[,j],0.99,na.rm = T)
    filt <- data[between(data[,j], low, up),]
    temp[,j] <- round(wtd.mean(filt[,j]
                               , filt[,"final_weight"],na.rm = T), 2)
    temp[,paste0("rur_",j)] <- round(wtd.mean(filt[filt$sector == 1,j]
                               , filt[filt$sector == 1,"final_weight"],na.rm = T), 2)
    temp[,paste0("urb_",j)] <- round(wtd.mean(filt[filt$sector == 2,j]
                               , filt[filt$sector == 2,"final_weight"],na.rm = T), 2)
  }
  cons <- rbind(cons,temp)
}

#Merging with original data
ntr3 <- cons

#Consumption and Nutrition Data
nss68_ide <- readRDS(paste0(nss68_path
                            , "working/rds_files/r6801t1_identification_v1.RDS"))
nss68_val <- readRDS(paste0(nss68_path
                            , "working/rds_files/r6801t1l05_total_consumption_val_in_rs_unadj_v1.RDS"))
nss68_ntr <- readRDS(paste0(nss68_path
                            , "working/rds_files/r6801t1_nutrient_v1.RDS"))
nss68_mem <- readRDS(paste0(nss68_path
                            , "working/rds_files/r6801t1_members_v1.RDS"))
nss68_qty <- readRDS(paste0(nss68_path
                            ,"working/rds_files/r6801t1l05_total_consumption_qty_in_100g_adj_v1.RDS"))
food_grp_map <- read_excel(paste0(nss68_path,"data_guide/layout/nss68_1_0_type1_layout.xlsx"),
                           sheet = "food_group_final")

#Creating consumption data
nss68_val$tot_val <- rowSums(nss68_val[,-1], na.rm = T)
nss68_hh <- merge(nss68_ide[,c("hhuid","d_state", "sector", "final_weight")]
                  , nss68_val[,c("hhuid","tot_val")]
                  , by = "hhuid")

#selecting relevant nutrients
macro_ntr <- c("Carbohydrate.CHOAVLDF", "Protein.PROTCNT"
               , "TotalFat.FATCE", "Energy.ENERKC")
nss68_ntr <- nss68_ntr[,c("hhuid", macro_ntr)]
quant1 <- quantile(nss68_ntr$Energy.ENERKC, 0.01)
quant99<- quantile(nss68_ntr$Energy.ENERKC, 0.99)
nss68_ntr <- nss68_ntr[nss68_ntr$Energy.ENERKC >= quant1,]
nss68_ntr <- nss68_ntr[nss68_ntr$Energy.ENERKC <= quant99,]
#computing nutrients as a percentage of energy
nss68_ntr$carbohydrate_percent <- nss68_ntr$Carbohydrate.CHOAVLDF * 4 /
  nss68_ntr$Energy.ENERKC * 100
nss68_ntr$protein_percent <- nss68_ntr$Protein.PROTCNT * 4 /
  nss68_ntr$Energy.ENERKC * 100
nss68_ntr$fat_percent <- nss68_ntr$TotalFat.FATCE * 9 /
  nss68_ntr$Energy.ENERKC * 100
nss68_ntr$fat_lmt <- ifelse(nss68_ntr$fat_percent >= 20, 1, 0)
#Creating percapita nutrient consumption data
nss68_ntr <- merge(nss68_ntr, nss68_mem[,c("hhuid", "consumer_units")]
                   , by = "hhuid")
nss68_ntr[,macro_ntr] <- nss68_ntr[,macro_ntr]/nss68_ntr$consumer_units
nss68_hh <- merge(nss68_hh
                  , nss68_ntr[,c("hhuid", macro_ntr, "carbohydrate_percent"
                                 , "protein_percent", "fat_percent", "fat_lmt")]
                  , by = "hhuid", all = T)

#calculating food group intake
req_food_grps <- c("Sugar..jaggery", "Rice", "Wheat"
                   , "Vegetable.oils","Butter.ghee", "Transfat")
nss68_qty[is.na(nss68_qty)] <- 0
food_grp_map[is.na(food_grp_map)] <- 0
nss68_qty_m <- as.matrix(nss68_qty[,c(2:152)])
food_grp_map <- as.matrix(food_grp_map[,c(3:28)])
nss68_food_grp <- nss68_qty_m %*% food_grp_map %>% data.frame()
nss68_food_grp$hhuid <- nss68_qty$hhuid

nss68_food_grp <- merge(nss68_food_grp
                        , nss68_mem[,c("hhuid", "consumer_units")]
                        , by = "hhuid")
nss68_food_grp[,req_food_grps] <- nss68_food_grp[,req_food_grps]/
  nss68_food_grp$consumer_units
nss68_food_grp$sugar <- nss68_food_grp$Sugar..jaggery
nss68_food_grp$cereal <- nss68_food_grp$Rice + nss68_food_grp$Wheat
nss68_food_grp$oils <- nss68_food_grp$Vegetable.oils +
  nss68_food_grp$Butter.ghee +
  nss68_food_grp$Transfat
nss68_hh <- merge(nss68_hh
                  , nss68_food_grp[,c("hhuid", "sugar", "cereal", "oils")]
                  , by = "hhuid", all = T)

#Harmonizing with nfhs 2 state codes
nss68_st_map <- read_excel(paste0(data_path, "support_data\\state_map.xlsx"), sheet = "nss68")
nss68_hh <- merge(nss68_hh, nss68_st_map, by.x = "d_state", by.y = "nss68")

#removing outliers
vv <- c("sugar"
        , "cereal"
        , "oils")

cons = data.table()
for(i in unique(nss68_hh$nfhs2)) {
  data <- nss68_hh[nss68_hh$nfhs2== i,]
  temp = data.table(state = i)
  for (j in vv){
    low <- quantile(data[,j],0.01,na.rm = T)
    up <- quantile(data[,j],0.99,na.rm = T)
    filt <- data[between(data[,j], low, up),]
    temp[,j] <- round(wtd.mean(filt[,j]
                               , filt[,"final_weight"],na.rm = T), 2)
    temp[,paste0("rur_",j)] <- round(wtd.mean(filt[filt$sector == 1,j]
                               , filt[filt$sector == 1,"final_weight"],na.rm = T), 2)
    temp[,paste0("urb_",j)] <- round(wtd.mean(filt[filt$sector == 2,j]
                               , filt[filt$sector == 2,"final_weight"],na.rm = T), 2)
  }
  cons <- rbind(cons,temp)
}
ntr4 <- cons

ntr2$round <- 2
ntr3$round <- 3
ntr4$round <- 4
ntr <- rbind(ntr2,ntr3)
ntr <- rbind(ntr,ntr4)
write.csv(ntr, paste0(data_path,"contextual_variables\\consumption.csv"), row.names = F)

