rm(list = setdiff(ls(), glo_vars))

# Round 2
nss55_emp_path <- "F:/St John's National Academy of Health Sciences/TATA NIN - sjri_data_repository/nss/nss55_10/nss55_10_0/"
nss55_emp <- readRDS(paste0(nss55_emp_path, "working/rds_files/r55100l04_v1.RDS"))
nss55_emp_id <- readRDS(paste0(nss55_emp_path, "working/rds_files/r55100l01_v1.RDS"))
nss55_emp_id$d_state <- with(nss55_emp_id,substr(state_region,1,str_length(state_region)-1))
nss55_emp <- merge(nss55_emp, nss55_emp_id[, c("hhuid", "d_state")], by = "hhuid")

emp_map <- read_xlsx(paste0(data_path, "support_data\\occupation_category.xlsx"), sheet = "sedn_68")

#Labour Force Participation
nss55_emp$lbr_force <- ifelse(nss55_emp$usual_status %in% 91:99, 0, 1)
#Employment status
nss55_emp$employed <- ifelse(nss55_emp$usual_status %in% 81:99, 0, 1)
#Workers by sector
nss55_emp$sector_agriculture <- ifelse(nss55_emp$usual_ind <= 6000, 1, 0)
nss55_emp$sector_industry <- ifelse(nss55_emp$usual_ind > 7000 & nss55_emp$usual_ind <= 45000, 1, 0)
nss55_emp$sector_service <- ifelse(nss55_emp$usual_ind >= 46000, 1, 0)

#Physical activity
nss55_emp <- merge(nss55_emp, emp_map[,c("nco_code", "Type")]
                   , by.x = "wk_nco"
                   , by.y = "nco_code"
                   , all.x = T)
nss55_emp$physical_sedentary <- ifelse(nss55_emp$Type == "Sedantary", 1, 0)
nss55_emp$physical_moderate <- ifelse(nss55_emp$Type == "Moderate", 1, 0)
nss55_emp$physical_heavy <- ifelse(nss55_emp$Type == "Heavy", 1, 0)

#Workers by wage-type
nss55_emp$wage_self_employed <- ifelse(nss55_emp$usual_status %in% c(11, 12, 21), 1, 0)
nss55_emp$wage_salaried <- ifelse(nss55_emp$usual_status %in% c(31), 1, 0)
nss55_emp$wage_casual_labour <- ifelse(nss55_emp$usual_status %in% c(41, 51), 1, 0)

#Harmonizing with nfhs 2 state codes
nss55_st_map <- read_excel(paste0(data_path, "support_data\\state_map.xlsx"), sheet = "nss55")
nss55_emp <- merge(nss55_emp, nss55_st_map, by.x = "d_state", by.y = "nss55")

#grouping with the states
emp = group_by(nss55_emp, nfhs2) %>%
  summarise(lfpr = round(wtd.mean(lbr_force)*100,2))

#grouping with the states
wage = group_by(nss55_emp[nss55_emp$employed == 1,], nfhs2) %>%
  summarise(wage_self_employed = round(wtd.mean(wage_self_employed)*100,2)
            , wage_salaried = round(wtd.mean(wage_salaried)*100,2)
            , wage_casual_labour = round(wtd.mean(wage_casual_labour)*100,2)
            , physical_sedentary = round(wtd.mean(physical_sedentary)*100,2)
            , physical_moderate = round(wtd.mean(physical_moderate)*100,2)
            , physical_heavy = round(wtd.mean(physical_heavy)*100,2)
            , sector_agriculture = round(wtd.mean(sector_agriculture)*100,2)
            , sector_industry = round(wtd.mean(sector_industry)*100,2)
            , sector_service = round(wtd.mean(sector_service)*100,2)
  )

wage2 = group_by(nss55_emp[nss55_emp$employed == 1,], nfhs2, sector) %>%
  summarise(physical_sedentary = round(wtd.mean(physical_sedentary)*100,2))

#Merging
emp2 <- merge(emp, wage, by = "nfhs2")

# Round 3
nss61_emp_path <- "F:/St John's National Academy of Health Sciences/TATA NIN - sjri_data_repository/nss/nss61_10/nss61_10_0/"
nss61_emp <- readRDS(paste0(nss61_emp_path, "working/rds_files/r61100l04_v2.RDS"))
nss61_emp_id <- readRDS(paste0(nss61_emp_path, "working/rds_files/r61100l01_v2.RDS"))
nss61_emp_id$d_state <- with(nss61_emp_id,substr(state_region,1,str_length(state_region)-1))
nss61_emp_id$d_weight <- with(nss61_emp_id, ifelse(nss==nsc,mlt_ss/100,
                                                   mlt_sr/200))
nss61_emp <- merge(nss61_emp, nss61_emp_id[, c("hhuid", "d_state", "d_weight")], by = "hhuid")
emp_map <- read_xlsx(paste0(data_path, "support_data\\occupation_category.xlsx"), sheet = "sedn_68")

#Labour Force Participation
nss61_emp$lbr_force <- ifelse(nss61_emp$usual_principal_activity_sta %in% 91:99, 0, 1)

#Employment status
nss61_emp$employed <- ifelse(nss61_emp$usual_principal_activity_sta %in% 81:99, 0, 1)

#Workers by sector
nss61_emp$sector_agriculture <- ifelse(nss61_emp$usual_principal_activity_nic <= 6000, 1, 0)
nss61_emp$sector_industry <- ifelse(nss61_emp$usual_principal_activity_nic > 6000 & nss61_emp$usual_principal_activity_nic <= 45000, 1, 0)
nss61_emp$sector_service <- ifelse(nss61_emp$usual_principal_activity_nic >= 46000, 1, 0)

#Physical activity
nss61_emp <- merge(nss61_emp, emp_map[,c("nco_code", "Type")]
                   , by.x = "usual_principal_activity_nco"
                   , by.y = "nco_code"
                   , all.x = T)
nss61_emp$physical_sedentary <- ifelse(nss61_emp$Type == "Sedantary", 1, 0)
nss61_emp$physical_moderate <- ifelse(nss61_emp$Type == "Moderate", 1, 0)
nss61_emp$physical_heavy <- ifelse(nss61_emp$Type == "Heavy", 1, 0)

#Workers by wage-type
nss61_emp$wage_self_employed <- ifelse(nss61_emp$usual_principal_activity_sta %in% c(11, 12, 21), 1, 0)
nss61_emp$wage_salaried <- ifelse(nss61_emp$usual_principal_activity_sta %in% c(31), 1, 0)
nss61_emp$wage_casual_labour <- ifelse(nss61_emp$usual_principal_activity_sta %in% c(41, 51), 1, 0)

#Harmonizing with nfhs 2 state codes
nss61_st_map <- read_excel(paste0(data_path, "support_data\\state_map.xlsx"), sheet = "nss61")
nss61_emp <- merge(nss61_emp, nss61_st_map, by.x = "d_state", by.y = "nss61")

#grouping with the states
emp = group_by(nss61_emp, nfhs2) %>%
  summarise(lfpr = round(wtd.mean(lbr_force, weights = d_weight)*100,2))

#grouping with the states
wage = group_by(nss61_emp[nss61_emp$employed == 1,], nfhs2) %>%
  summarise(wage_self_employed = round(wtd.mean(wage_self_employed, weights = d_weight)*100,2)
            , wage_salaried = round(wtd.mean(wage_salaried, weights = d_weight)*100,2)
            , wage_casual_labour = round(wtd.mean(wage_casual_labour, weights = d_weight)*100,2)
            , physical_sedentary = round(wtd.mean(physical_sedentary, weights = d_weight)*100,2)
            , physical_moderate = round(wtd.mean(physical_moderate, weights = d_weight)*100,2)
            , physical_heavy = round(wtd.mean(physical_heavy, weights = d_weight)*100,2)
            , sector_agriculture = round(wtd.mean(sector_agriculture, weights = d_weight)*100,2)
            , sector_industry = round(wtd.mean(sector_industry, weights = d_weight)*100,2)
            , sector_service = round(wtd.mean(sector_service, weights = d_weight)*100,2))

wage3 = group_by(nss61_emp[nss61_emp$employed == 1,], nfhs2, sector) %>%
  summarise(physical_sedentary = round(wtd.mean(physical_sedentary, weights = d_weight)*100,2))

#Merging with original data
emp3 <- merge(emp, wage, by = "nfhs2")

# Round 4
nss68_emp_path <- "F:/St John's National Academy of Health Sciences/TATA NIN - sjri_data_repository/nss/nss68_10/nss68_10_0/"
nss68_emp <- readRDS(paste0(nss68_emp_path
                            , "working/rds_files/r68100l04_v2.RDS"))
nss68_emp_id <- readRDS(paste0(nss68_emp_path
                               , "working/rds_files/r68100l01_v1.RDS"))
nss68_emp <- merge(nss68_emp
                   , nss68_emp_id[, c("hhuid", "d_state", "d_weight")]
                   , by = "hhuid")

emp_map <- read_xlsx(paste0(data_path, "support_data\\occupation_category.xlsx")
                     , sheet = "sedn_04")

#Labour Force Participation
nss68_emp$lbr_force <- ifelse(nss68_emp$usual_principal_activity_sta %in% 91:99, 0, 1)

#Employment status
nss68_emp$employed <- ifelse(nss68_emp$usual_principal_activity_sta %in% 81:99, 0, 1)

# #JV: Tidyverse alternative
# nss68 <- nss68 %>%
#   mutate(lbr_force = case_when(usual_principal_activity_sta %in% c(91:99) ~ 0,
#                                TRUE ~ 1),
#          employed = case_when(usual_principal_activity_sta %in% c(81:99) ~ 0,
#                               TRUE ~ 1))

#Workers by sector
nss68_emp$sector_agriculture <- ifelse(nss68_emp$usual_principal_activity_nic <=
                                         5000, 1, 0)
nss68_emp$sector_industry <- ifelse(nss68_emp$usual_principal_activity_nic >
                                      5000 &
                                      nss68_emp$usual_principal_activity_nic <=
                                      45000, 1, 0)
nss68_emp$sector_service <- ifelse(nss68_emp$usual_principal_activity_nic >= 45000
                                   , 1
                                   , 0)

#Physical activity
nss68_emp <- merge(nss68_emp, emp_map[,c("nco_code", "Type")]
                   , by.x = "usual_principal_activity_nco"
                   , by.y = "nco_code"
                   , all.x = T)
nss68_emp$physical_sedentary <- ifelse(nss68_emp$Type == "Sedantary", 1, 0)
nss68_emp$physical_moderate <- ifelse(nss68_emp$Type == "Moderate", 1, 0)
nss68_emp$physical_heavy <- ifelse(nss68_emp$Type == "Heavy", 1, 0)

#Workers by wage-type
nss68_emp$wage_self_employed <- ifelse(nss68_emp$usual_principal_activity_sta %in% c(11, 12, 21), 1, 0)
nss68_emp$wage_salaried <- ifelse(nss68_emp$usual_principal_activity_sta %in% c(31), 1, 0)
nss68_emp$wage_casual_labour <- ifelse(nss68_emp$usual_principal_activity_sta %in% c(41, 51), 1, 0)

#Harmonizing with nfhs 2 state codes
nss68_st_map <- read_excel(paste0(data_path, "support_data\\state_map.xlsx"), sheet = "nss68")
nss68_emp <- merge(nss68_emp, nss68_st_map, by.x = "d_state", by.y = "nss68")

#grouping with the states
emp = group_by(nss68_emp, nfhs2) %>%
  summarise(lfpr = round(wtd.mean(lbr_force, weights = d_weight)*100,2))

#grouping with the states
wage = group_by(nss68_emp[nss68_emp$employed == 1,], nfhs2) %>%
  summarise(wage_self_employed = round(wtd.mean(wage_self_employed, weights = d_weight)*100,2)
            , wage_salaried = round(wtd.mean(wage_salaried, weights = d_weight)*100,2)
            , wage_casual_labour = round(wtd.mean(wage_casual_labour, weights = d_weight)*100,2)
            , physical_sedentary = round(wtd.mean(physical_sedentary, weights = d_weight)*100,2)
            , physical_moderate = round(wtd.mean(physical_moderate, weights = d_weight)*100,2)
            , physical_heavy = round(wtd.mean(physical_heavy, weights = d_weight)*100,2)
            , sector_agriculture = round(wtd.mean(sector_agriculture, weights = d_weight)*100,2)
            , sector_industry = round(wtd.mean(sector_industry, weights = d_weight)*100,2)
            , sector_service = round(wtd.mean(sector_service, weights = d_weight)*100,2))
wage4 = group_by(nss68_emp[nss68_emp$employed == 1,], nfhs2, sector) %>%
  summarise(physical_sedentary = round(wtd.mean(physical_sedentary, weights = d_weight)*100,2))
#Merging with original data
emp4 <- merge(emp, wage, by = "nfhs2")

################################################################################
emp2$round <- 2
emp3$round <- 3
emp4$round <- 4
wage2$round <- 2
wage3$round <- 3
wage4$round <- 4
emp <- rbind(emp2, emp3)
emp <- rbind(emp, emp4)
wage <- rbind(wage2, wage3)
wage <- rbind(wage, wage4)
wage1 <- wage[wage$sector == 1,-2]
wage2 <- wage[wage$sector == 2,-2]
colnames(wage1)[2] = "physical_sedentary_rur"
colnames(wage2)[2] = "physical_sedentary_urb"
emp = merge(emp,wage1,by = c("nfhs2", "round"))
emp = merge(emp,wage2,by = c("nfhs2", "round"))
colnames(emp)[1] = "state"
write.csv(emp, paste0(data_path,"contextual_variables\\employment.csv"), row.names = F)
