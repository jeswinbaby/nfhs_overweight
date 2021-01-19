rm(list = setdiff(ls(), glo_vars))
library(readxl)
nfhs2_wm <- readRDS(paste0(data_path, "ind_data\\nfhs2_wm.rds"))
nfhs3_wm <- readRDS(paste0(data_path, "ind_data\\nfhs3_wm.rds"))
nfhs4_wm <- readRDS(paste0(data_path, "ind_data\\nfhs4_wm.rds"))
sample_sizes <- data.table()
sample_sizes <- rbind(sample_sizes,t(c(nrow(nfhs2_wm), nrow(nfhs3_wm), nrow(nfhs4_wm))))
nfhs2_wm$round <- 2
nfhs3_wm$round <- 3
nfhs4_wm$round <- 4
nfhs_wm <- list()
cn <- c("hhid","age", "res_years","education", "state", "psu","caste", "wealth_index",
        "residence_type","parity", "bmi", "sample_weight", "round", "married","pregnant", "lactating", "slum")

nfhs4_wm <- nfhs4_wm[,c("hhid","v012", "v104", "v106", "v024", "v021","s116", "v190",
                        "v025","v137", "v445", "v005", "round", "s301","v454","v404", "sslumc")]
colnames(nfhs4_wm) <- cn
nfhs4_st_map <- read_excel(paste0(data_path
                                  , "support_data\\state_map.xlsx"), sheet = "nfhs_4")
nfhs4_wm <- merge(nfhs4_wm, nfhs4_st_map, by.x = "state", by.y = "nfhs_4")
nfhs4_wm$state <- nfhs4_wm$nfhs_2

#Removing union territories
nfhs4_wm <- nfhs4_wm[!is.na(nfhs4_wm$state)]
nfhs_wm[["nfhs4"]] <- nfhs4_wm

nfhs3_wm <- nfhs3_wm[,c("hhid","v012",  "v104","v106", "v024", "s021","s118", "v190",
                        "v025","v137", "v445", "v005","round", "s301","v454","v404", "sslumc")]

colnames(nfhs3_wm) <- cn
#Harmonizing the states with NFHS 2 states
nfhs3_st_map <- read_excel(paste0(data_path, "support_data\\state_map.xlsx"), sheet = "nfhs_3")
nfhs3_wm <- merge(nfhs3_wm, nfhs3_st_map, by.x = "state", by.y = "nfhs_3")
nfhs3_wm$state <- nfhs3_wm$nfhs_2

#Removing union territories
nfhs3_wm <- nfhs3_wm[!is.na(nfhs3_wm$state)]

nfhs3_wm$slum[nfhs3_wm$slum == 2] = 0
nfhs_wm[["nfhs3"]] <- nfhs3_wm

nfhs2_wm <- nfhs2_wm[,c("hhid","v012",  "v104","v106", "v024", "v021",
                        "v131", "v190",
                        "v025","v137", "v445", "v005", "round", "v213","v404", "sslumhh")]

colnames(nfhs2_wm) <- c("hhid","age", "res_years","education", "state", "psu","caste", "wealth_index",
                              "residence_type","parity", "bmi", "sample_weight", "round", "pregnant", "lactating", "slum")
nfhs2_wm$married <- 1
nfhs_wm[["nfhs2"]] <- nfhs2_wm
sample_sizes <- rbind(sample_sizes,t(c(nrow(nfhs2_wm), nrow(nfhs3_wm), nrow(nfhs4_wm))))

#merging data
nfhs_wm <- rbindlist(nfhs_wm, use.names = T, fill = T)
nfhs_wm <- nfhs_wm[,-c("state.y","nfhs_2")]
#Creating a dummy for overweight Y/N
nfhs_wm$ovrwt <- ifelse(nfhs_wm$bmi >= 2500, 1, 0)
nfhs_wm$ovrwt_23 <- ifelse(nfhs_wm$bmi >= 2300, 1, 0)

nfhs_wm$slum[is.na(nfhs_wm$slum)] = 0
#removing outliers
nfhs_wm <- nfhs_wm[nfhs_wm$age >= 18,] #Selecting women aged 18 or more
sample_sizes <- rbind(sample_sizes,t(c(nrow(nfhs_wm[nfhs_wm$round== 2,]), nrow(nfhs_wm[nfhs_wm$round== 3,]), nrow(nfhs_wm[nfhs_wm$round== 4,]))))
nfhs_wm <- nfhs_wm[nfhs_wm$married != 0,] #selecting married women
sample_sizes <- rbind(sample_sizes,t(c(nrow(nfhs_wm[nfhs_wm$round== 2,]), nrow(nfhs_wm[nfhs_wm$round== 3,]), nrow(nfhs_wm[nfhs_wm$round== 4,]))))
nfhs_wm <- nfhs_wm[nfhs_wm$pregnant == 0,] #selecting non pregnant women
sample_sizes <- rbind(sample_sizes,t(c(nrow(nfhs_wm[nfhs_wm$round== 2,]), nrow(nfhs_wm[nfhs_wm$round== 3,]), nrow(nfhs_wm[nfhs_wm$round== 4,]))))
nfhs_wm <- nfhs_wm[nfhs_wm$lactating == 0,] #selecting non lactating women
sample_sizes <- rbind(sample_sizes,t(c(nrow(nfhs_wm[nfhs_wm$round== 2,]), nrow(nfhs_wm[nfhs_wm$round== 3,]), nrow(nfhs_wm[nfhs_wm$round== 4,]))))
nfhs_wm <- nfhs_wm[nfhs_wm$bmi >= 1200 & nfhs_wm$bmi <= 5000,] #removing bmi outliers
sample_sizes <- rbind(sample_sizes,t(c(nrow(nfhs_wm[nfhs_wm$round== 2,]), nrow(nfhs_wm[nfhs_wm$round== 3,]), nrow(nfhs_wm[nfhs_wm$round== 4,]))))
nfhs_wm <- na.omit(nfhs_wm) #removing any missing rows
sample_sizes <- rbind(sample_sizes,t(c(nrow(nfhs_wm[nfhs_wm$round== 2,]), nrow(nfhs_wm[nfhs_wm$round== 3,]), nrow(nfhs_wm[nfhs_wm$round== 4,]))))

#Recategorizing
nfhs_wm$parity[nfhs_wm$parity >= 3] = 3
nfhs_wm$caste <- as.numeric(nfhs_wm$caste)
nfhs_wm$caste[nfhs_wm$caste >= 4] <- 4
nfhs_wm$caste[nfhs_wm$caste == 4] <- 0
nfhs_wm$caste[nfhs_wm$caste %in% c(1,2)] <- 1
nfhs_wm$caste[nfhs_wm$caste == 3] <- 2
nfhs_wm$education[nfhs_wm$education >= 3] <- 3
nfhs_wm$age_category <- cut(nfhs_wm$age,
                            breaks = c(17, 25, 35, 50),
                            labels = c("18-25", "25-35", "35-50"))
nfhs_wm$migrated <- ifelse(nfhs_wm$res_years <= 5 | nfhs_wm$res_years == 96, 1, 0)
nfhs_wm <- nfhs_wm[,c("hhid","age", "education", "state", "psu","caste", "wealth_index",
                        "residence_type","parity", "ovrwt", "ovrwt_23", "sample_weight", "round", "age_category", "migrated", "slum")]
saveRDS(nfhs_wm, paste0(data_path, "nfhs_wm.RDS"))
saveRDS(sample_sizes, paste0(data_path, "support_data\\data_selection.RDS"))

