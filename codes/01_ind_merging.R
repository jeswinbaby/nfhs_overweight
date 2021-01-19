rm(list = setdiff(ls(), glo_vars))
nfhs4_wm_ide <- readRDS(paste0(nfhs4_path
                               ,"working/rds/nfhs4_i_identification.rds"))
nfhs4_wm_hlt <- readRDS(paste0(nfhs4_path,"working/rds/nfhs4_i_health.rds"))
nfhs4_wm_mrg <- readRDS(paste0(nfhs4_path
                               ,"working/rds/nfhs4_i_marriagereproduction.rds"))
nfhs4_wm_ide$hhid <- substr(nfhs4_wm_ide$caseid,1,9)
nfhs4_wm_ide$hhid <- as.double(nfhs4_wm_ide$hhid)
#selecting required data
nfhs4_wm <- nfhs4_wm_ide[,c("caseid" #women id
                            , "hhid"
                            , "v012" #age
                            , "v021" #psu
                            , "v024" #state
                            , "v025" #urban/rural
                            , "v026" #type of residence
                            , "v104" #years of residence
                            , "v106" #highest educational level
                            , "v130" #religion
                            , "s116" #caste
                            , "v137" #no of children
                            , "v190" #wealth index
                            , "sslumc"
                            , "v005")] #sample weight
nfhs4_wm <- merge(nfhs4_wm
                  , nfhs4_wm_hlt[,c("caseid" #women id
                                    , "v404" #currently breastfeeding
                                    , "v445" #bmi
                                    , "v454" #pregnant
                                    , "s716" #alcohol
                                    , "s726a" #milk or curd
                                    , "s726b" #pulses or beans
                                    , "s726c" #dark green leafy vegetable
                                    , "s726d" #fruits
                                    , "s726e" #eggs
                                    , "s726f" #fish
                                    , "s726g")] #chicken or meat
                  , by = "caseid")
nfhs4_wm <- merge(nfhs4_wm
                  , nfhs4_wm_mrg[,c("caseid"
                                    , "s301")]
                  , by = "caseid")

saveRDS(nfhs4_wm, paste0(data_path, "ind_data\\nfhs4_wm.rds"))

# NFHS 3
rm(list = setdiff(ls(), glo_vars))
nfhs3_wm_ide <- readRDS(paste0(nfhs3_path,"working/rds/nfhs3_i_identification.rds"))
nfhs3_wm_hlt <- readRDS(paste0(nfhs3_path,"working/rds/nfhs3_i_health.rds"))
nfhs3_wm_mrg <- readRDS(paste0(nfhs3_path,"working/rds/nfhs3_i_marriagereproduction.rds"))
nfhs3_wm_ide$hhid <- substr(nfhs3_wm_ide$caseid,1,12)
#selecting required data
nfhs3_wm <- nfhs3_wm_ide[,c("caseid" #women id
                            , "hhid"
                            , "v012" #age
                            , "s021" #psu
                            , "v024" #state
                            , "v025" #urban/rural
                            , "v026" #type of residence
                            , "v104" #years of residence
                            , "v105" #prev residence
                            , "v106" #highest educational level
                            , "v130" #religion
                            , "s118" #caste
                            , "v137" #no of kids
                            , "v190" #wealth index
                            , "sslumc"
                            , "v005")] #sample weight

nfhs3_wm <- merge(nfhs3_wm
                  , nfhs3_wm_hlt[,c("caseid" #women id
                                    , "v404" #currently breastfeeding
                                    , "v445" #bmi
                                    , "v454" #pregnant
                                    , "v463z" #smokes nothing
                                    , "s569" #drinks alcohol
                                    , "s558a" #milk or curd
                                    , "s558b" #pulses or beans
                                    , "s558c" #dark green leafy veggies
                                    , "s558d" #fruits
                                    , "s558e" #eggs
                                    , "s558f" #fish
                                    , "s558g")] #chicken or meat
                  , by = "caseid")
nfhs3_wm <- merge(nfhs3_wm
                  , nfhs3_wm_mrg[,c("caseid"
                                    , "s301")]
                  , by = "caseid")

saveRDS(nfhs3_wm, paste0(data_path, "ind_data\\nfhs3_wm.rds"))


#NFHS 2
rm(list = setdiff(ls(), glo_vars))
#Calculating Wealth index for NFHS 2 data
#https://www.researchgate.net/publication/267733911_Local_Distributions_of_Wealth_to_Describe_Health_Inequalities_in_India_A_New_Approach_for_Analyzing_Nationally_Representative_Household_Survey_Data_1992-2008
nfhs2_hh_ide <- readRDS(paste0(nfhs2_path,"working/rds/nfhs2_h_identification.rds"))
nfhs2_hh_sch <- readRDS(paste0(nfhs2_path,"working/rds/nfhs2_h_schedule.rds"))
nfhs2_hh <- merge(nfhs2_hh_ide[,c("hhid", "hv209", "hv211", "sh47k", "sh47g", "sh47b"
                                  ,"sh47c","sh47e","sh47j","sh35","sh47a","hv206"
                                  , "hv208", "hv207")], nfhs2_hh_sch[,c("hhid", "hv106_01")])
nfhs2_hh <- nfhs2_hh[(!is.na(nfhs2_hh$hv209) &
                        !is.na(nfhs2_hh$hv211) &
                        !is.na(nfhs2_hh$sh47k) &
                        !is.na(nfhs2_hh$sh47g) &
                        !is.na(nfhs2_hh$sh47b) &
                        !is.na(nfhs2_hh$sh47c) &
                        !is.na(nfhs2_hh$sh47e) &
                        !is.na(nfhs2_hh$sh47j) &
                        !is.na(nfhs2_hh$sh35) &
                        !is.na(nfhs2_hh$sh47a) &
                        !is.na(nfhs2_hh$hv206) &
                        !is.na(nfhs2_hh$hv208) &
                        !is.na(nfhs2_hh$hv207) &
                        !is.na(nfhs2_hh$hv106_01)), ]

nfhs2_hh$sh35[nfhs2_hh$sh35 > 4] <- 4

nfhs2_hh$wealth_score <- with(nfhs2_hh, hv106_01 * 0.25 +
                      hv209 * 0.264 +
                      hv211 * 0.245 +
                      sh47k * 0.231 +
                      sh47g * 0.309 +
                      sh47b * 0.314 +
                      sh47c * 0.294 +
                      sh47e * 0.3 +
                      sh47j * 0.235 +
                      sh35 * 0.214 +
                      sh47a * 0.275 +
                      hv206 * 0.253 +
                      hv208 * 0.316 +
                      hv207 * 0.21)


nfhs2_hh <- nfhs2_hh %>%
  dplyr::mutate(v190 = cut(wealth_score,
                                      breaks = quantile(wealth_score
                                                        , probs = seq(0,1, by = 0.2)
                                                        , na.rm = TRUE),
                                      labels = 1:5,
                                      include.lowest = TRUE))

#NFHS 2 data
nfhs2_wm_ide <- readRDS(paste0(nfhs2_path,"working/rds/nfhs2_i_identification.rds"))
nfhs2_wm_hlt <- readRDS(paste0(nfhs2_path,"working/rds/nfhs2_i_health.rds"))
nfhs2_wm_mrg <- readRDS(paste0(nfhs2_path,"working/rds/nfhs2_i_marriagereproduction.rds"))

#Creating hhid
nfhs2_wm_ide$hhid <- substr(nfhs2_wm_ide$caseid, 1, 12)

#Selecting required vars
nfhs2_wm_ide <- merge(nfhs2_wm_ide
                      , nfhs2_hh[,c("hhid"
                                    , "v190"
                      )], by = "hhid")
nfhs2_wm <- nfhs2_wm_ide[,c("caseid" #women id
                            , "hhid"
                            , "v012" #age
                            , "v021"
                            , "v024" #state
                            , "v025" #urban/rural
                            , "v026" #type
                            , "v104" #years of residence
                            , "v105" #prev residence
                            , "v106" #highest educational level
                            , "v130" #religion
                            , "v131" #caste
                            , "v137" #no of children
                            , "v190" #wealth index
                            , "v213" #currently pregnant
                            , "sslumhh" #slum household
                            , "v005")] #sample weight

nfhs2_wm <- merge(nfhs2_wm
                  , nfhs2_wm_hlt[,c("caseid" #women id
                                    , "v404" #Breastfeeding
                                    , "v445" #BMI
                                    , "s124a" #milk/curd
                                    , "s124b" #pulses or beans
                                    , "s124c" #green leafy vegetables
                                    , "s124d" #other vegetables
                                    , "s124e" #fruits
                                    , "s124f" #eggs
                                    , "s124g")] #chicken/meat/fish
                  , by = "caseid")

saveRDS(nfhs2_wm, paste0(data_path, "ind_data\\nfhs2_wm.rds"))