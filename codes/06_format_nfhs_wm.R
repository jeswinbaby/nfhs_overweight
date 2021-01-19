rm(list = setdiff(ls(), glo_vars))
nfhs_wm <- readRDS(paste0(data_path, "nfhs_wm.rds"))
nfhs_wi <- readRDS(paste0(data_path, "support_data/nfhs_wi.rds"))

emp <- read.csv(paste0(data_path,"contextual_variables/employment.csv"))
ntr_groups <- read.csv(paste0(data_path,"contextual_variables/consumption.csv"))
ntr_foods <- read.csv(paste0(data_path,"contextual_variables/consumption_all_median.csv"))
eco <- read.csv(paste0(data_path,"contextual_variables/economic.csv"))
nfhs2_st_map <- readxl::read_excel(paste0(data_path, "support_data/state_map.xlsx"), sheet = "nfhs_2")
nfhs_wm <- merge(nfhs_wm, nfhs_wi, by = c("round", "hhid"))
nfhs_wm <- merge(nfhs_wm, emp, by = c("state","round"))
nfhs_wm <- merge(nfhs_wm, ntr_groups, by = c("state","round"))
nfhs_wm <- merge(nfhs_wm, ntr_foods, by = c("state","round"))
nfhs_wm <- merge(nfhs_wm, eco, by = c("state","round"))
nfhs_wm <- merge(nfhs_wm, nfhs2_st_map, by.x = "state", by.y = "nfhs_2")

write_rds(nfhs_wm, paste0(data_path,"nfhs_wm_v2.rds"))

nfhs_wm_labelled <- nfhs_wm %>%
  dplyr::select(-state.y,-hv024, -hhid, -wealth_score) %>%
  mutate(round = factor(round,levels=c("2","3","4"),labels=c("NFHS 2","NFHS 3","NFHS 4")),
         education = factor(education,levels=c("0","1","2","3"),labels=c("No formal education","Primary",
                                                             "Secondary","Higher than secondary")),
         wealth_index_sm = factor(wealth_index_sm, levels=as.character(c(1:5)),
                                  labels=c("Lowest","Q2","Q3","Q4","Highest")),
         wealth_index_stateround = factor(wealth_index_stateround, levels=as.character(c(1:5)),
                                  labels=c("Lowest","Q2","Q3","Q4","Highest")),
         wealth_index_comb = factor(wealth_index_comb, levels=as.character(c(1:5)),
                                          labels=c("Lowest","Q2","Q3","Q4","Highest")),
         caste = factor(caste,levels=c(0:2),labels=c("General","SC/ST","OBC")),
         wealth_index = factor(wealth_index, levels=as.character(c(1:5)),
                                  labels=c("Lowest","Q2","Q3","Q4","Highest")),
         residence_type = factor(residence_type,levels=as.character(c(1:2)),
                                 labels=c("Urban","Rural")),
         parity = factor(parity,levels=as.character(c(0:3)),
                         labels=c("0","1","2","3 or more")),
         ovrwt = factor(ovrwt,levels=as.character(c(0:1)),labels=c("No","Yes")),
         ovrwt_23 = factor(ovrwt,levels=as.character(c(0:1)),labels=c("No","Yes")),
         age_category = factor(age_category,labels=c("18-25", "25-35", "35-50"))
         )
write_rds(nfhs_wm_labelled, paste0(data_path,"nfhs_wm_labelled.rds"))
source("code_support/07_label_variable.R")
nfhs_wm_labelled <- label_variables(nfhs_wm_labelled,domain_name = "nfhs_wm")
names(nfhs_wm_labelled) <- str_replace_all(names(nfhs_wm_labelled),"\\.+","_")
haven::write_dta(nfhs_wm_labelled, paste0(data_path, "nfhs_wm_labelled.dta"),version = 12)
