################################################################################
#Round level overall models
#Fixed      : Individual covariates
#Random     : Economic variables
#Levels     : ID X PSU X State X Round
################################################################################
source("code_support\\10_model_inits.R")

nfhs2_wm <- nfhs_wm[nfhs_wm$round == 2,]
nfhs2_wm_ur <- nfhs2_wm[nfhs2_wm$residence_type == 2,]
nfhs2_wm_ru <- nfhs2_wm[nfhs2_wm$residence_type == 1,]

nfhs3_wm <- nfhs_wm[nfhs_wm$round == 3,]
nfhs3_wm_ur <- nfhs3_wm[nfhs3_wm$residence_type == 2,]
nfhs3_wm_ru <- nfhs3_wm[nfhs3_wm$residence_type == 1,]

nfhs4_wm <- nfhs_wm[nfhs_wm$round == 4,]
nfhs4_wm_ur <- nfhs4_wm[nfhs4_wm$residence_type == 2,]
nfhs4_wm_ru <- nfhs4_wm[nfhs4_wm$residence_type == 1,]

################################################################################
# All Economic Variables
################################################################################
# Round 2
################################################################################
#National
formula5 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type +#Individual vars
  log(gdp) + literacy + physical_sedentary + #Eco vars
  (1| state) + (1| psu)
fit520 <- runMLwiN(Formula = formula5
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm)
saveRDS(fit520, paste0(res_path, "fit520.RDS"))
#Urban
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  log(gdp) + literacy + physical_sedentary + #Eco vars
  (1| state) + (1| psu)
fit521 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ur)
saveRDS(fit521, paste0(res_path, "fit521.RDS"))
#Rural
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  log(gdp) + literacy + physical_sedentary + #Eco vars
  (1| state) + (1| psu)
fit522 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ru)
saveRDS(fit522, paste0(res_path, "fit522.RDS"))

################################################################################
# Round 3
################################################################################
#National
formula5 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type +#Individual vars
  log(gdp) + literacy + physical_sedentary + #Eco vars
  (1| state) + (1| psu)
fit530 <- runMLwiN(Formula = formula5
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm)
saveRDS(fit530, paste0(res_path, "fit530.RDS"))
#Urban
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  log(gdp) + literacy + physical_sedentary + #Eco vars
  (1| state) + (1| psu)
fit531 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ur)
saveRDS(fit531, paste0(res_path, "fit531.RDS"))
#Rural
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  log(gdp) + literacy + physical_sedentary + #Eco vars
  (1| state) + (1| psu)
fit532 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ru)
saveRDS(fit532, paste0(res_path, "fit532.RDS"))

################################################################################
# Round 4
################################################################################
#National
formula5 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type +#Individual vars
  log(gdp) + literacy + physical_sedentary + #Eco vars
  (1| state) + (1| psu)
fit540 <- runMLwiN(Formula = formula5
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm)
saveRDS(fit540, paste0(res_path, "fit540.RDS"))
#Urban
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  log(gdp) + literacy + physical_sedentary + #Eco vars
  (1| state) + (1| psu)
fit541 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ur)
saveRDS(fit541, paste0(res_path, "fit541.RDS"))
#Rural
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  log(gdp) + literacy + physical_sedentary + #Eco vars
  (1| state) + (1| psu)
fit542 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ru)
saveRDS(fit542, paste0(res_path, "fit542.RDS"))


################################################################################
# GDP only
################################################################################
# Round 2
################################################################################
#National
formula5 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type +#Individual vars
  log(gdp) + #Eco vars
  (1| state) + (1| psu)
fit520 <- runMLwiN(Formula = formula5
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm)
saveRDS(fit520, paste0(res_path, "fit520_1.RDS"))
#Urban
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  log(gdp) + #Eco vars
  (1| state) + (1| psu)
fit521 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ur)
saveRDS(fit521, paste0(res_path, "fit521_1.RDS"))
#Rural
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  log(gdp) + #Eco vars
  (1| state) + (1| psu)
fit522 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ru)
saveRDS(fit522, paste0(res_path, "fit522_1.RDS"))

################################################################################
# Round 3
################################################################################
#National
formula5 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type +#Individual vars
  log(gdp)  + #Eco vars
  (1| state) + (1| psu)
fit530 <- runMLwiN(Formula = formula5
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm)
saveRDS(fit530, paste0(res_path, "fit530_1.RDS"))
#Urban
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  log(gdp) + #Eco vars
  (1| state) + (1| psu)
fit531 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ur)
saveRDS(fit531, paste0(res_path, "fit531_1.RDS"))
#Rural
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  log(gdp) + #Eco vars
  (1| state) + (1| psu)
fit532 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ru)
saveRDS(fit532, paste0(res_path, "fit532_1.RDS"))

################################################################################
# Round 4
################################################################################
#National
formula5 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type +#Individual vars
  log(gdp) + #Eco vars
  (1| state) + (1| psu)
fit540 <- runMLwiN(Formula = formula5
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm)
saveRDS(fit540, paste0(res_path, "fit540_1.RDS"))
#Urban
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  log(gdp) + #Eco vars
  (1| state) + (1| psu)
fit541 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ur)
saveRDS(fit541, paste0(res_path, "fit541_1.RDS"))
#Rural
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  log(gdp) + #Eco vars
  (1| state) + (1| psu)
fit542 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ru)
saveRDS(fit542, paste0(res_path, "fit542_1.RDS"))



################################################################################
# Literacy only
################################################################################
# Round 2
################################################################################
#National
formula5 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type +#Individual vars
  literacy + #Eco vars
  (1| state) + (1| psu)
fit520 <- runMLwiN(Formula = formula5
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm)
saveRDS(fit520, paste0(res_path, "fit520_2.RDS"))
#Urban
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  literacy + #Eco vars
  (1| state) + (1| psu)
fit521 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ur)
saveRDS(fit521, paste0(res_path, "fit521_2.RDS"))
#Rural
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  literacy + #Eco vars
  (1| state) + (1| psu)
fit522 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ru)
saveRDS(fit522, paste0(res_path, "fit522_2.RDS"))

################################################################################
# Round 3
################################################################################
#National
formula5 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type +#Individual vars
  literacy  + #Eco vars
  (1| state) + (1| psu)
fit530 <- runMLwiN(Formula = formula5
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm)
saveRDS(fit530, paste0(res_path, "fit530_2.RDS"))
#Urban
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  literacy + #Eco vars
  (1| state) + (1| psu)
fit531 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ur)
saveRDS(fit531, paste0(res_path, "fit531_2.RDS"))
#Rural
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  literacy + #Eco vars
  (1| state) + (1| psu)
fit532 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ru)
saveRDS(fit532, paste0(res_path, "fit532_2.RDS"))

################################################################################
# Round 4
################################################################################
#National
formula5 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type +#Individual vars
  literacy + #Eco vars
  (1| state) + (1| psu)
fit540 <- runMLwiN(Formula = formula5
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm)
saveRDS(fit540, paste0(res_path, "fit540_2.RDS"))
#Urban
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  literacy + #Eco vars
  (1| state) + (1| psu)
fit541 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ur)
saveRDS(fit541, paste0(res_path, "fit541_2.RDS"))
#Rural
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  literacy + #Eco vars
  (1| state) + (1| psu)
fit542 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ru)
saveRDS(fit542, paste0(res_path, "fit542_2.RDS"))



################################################################################
# Sedentary pop only
################################################################################
# Round 2
################################################################################
#National
formula5 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type +#Individual vars
  physical_sedentary + #Eco vars
  (1| state) + (1| psu)
fit520 <- runMLwiN(Formula = formula5
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm)
saveRDS(fit520, paste0(res_path, "fit520_3.RDS"))
#Urban
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  physical_sedentary_urb + #Eco vars
  (1| state) + (1| psu)
fit521 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ur)
saveRDS(fit521, paste0(res_path, "fit521_3_v2.RDS"))
#Rural
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  physical_sedentary_rur + #Eco vars
  (1| state) + (1| psu)
fit522 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ru)
saveRDS(fit522, paste0(res_path, "fit522_3_v2.RDS"))

################################################################################
# Round 3
################################################################################
#National
formula5 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type +#Individual vars
  physical_sedentary  + #Eco vars
  (1| state) + (1| psu)
fit530 <- runMLwiN(Formula = formula5
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm)
saveRDS(fit530, paste0(res_path, "fit530_3.RDS"))
#Urban
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  physical_sedentary_urb + #Eco vars
  (1| state) + (1| psu)
fit531 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ur)
saveRDS(fit531, paste0(res_path, "fit531_3_v2.RDS"))
#Rural
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  physical_sedentary_rur + #Eco vars
  (1| state) + (1| psu)
fit532 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ru)
saveRDS(fit532, paste0(res_path, "fit532_3_v2.RDS"))

################################################################################
# Round 4
################################################################################
#National
formula5 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type +#Individual vars
  physical_sedentary + #Eco vars
  (1| state) + (1| psu)
fit540 <- runMLwiN(Formula = formula5
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm)
saveRDS(fit540, paste0(res_path, "fit540_3.RDS"))
#Urban
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  physical_sedentary_urb + #Eco vars
  (1| state) + (1| psu)
fit541 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ur)
saveRDS(fit541, paste0(res_path, "fit541_3_v2.RDS"))
#Rural
formula51 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm +#Individual vars
  physical_sedentary_rur + #Eco vars
  (1| state) + (1| psu)
fit542 <- runMLwiN(Formula = formula51
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ru)
saveRDS(fit542, paste0(res_path, "fit542_3_v2.RDS"))

################################################################################
# Nutrition Models
################################################################################
# Round 2
################################################################################

#National
formula6 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  oils + sugar + cereal + #Nutrition vars
  (1| state) + (1| psu)
fit620 <- runMLwiN(Formula = formula6
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm)
saveRDS(fit620, paste0(res_path, "fit620.RDS"))
#Urban
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_oils + urb_sugar + urb_cereal + #Nutrition vars
  (1| state) + (1| psu)
fit621 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ur)
saveRDS(fit621, paste0(res_path, "fit621_v2.RDS"))
#Rural
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_oils + rur_sugar + rur_cereal + #Nutrition vars
  (1| state) + (1| psu)
fit622 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ru)
saveRDS(fit622, paste0(res_path, "fit622_v2.RDS"))

################################################################################
# Round 3
################################################################################

#National
formula6 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  oils + sugar + cereal + #Nutrition vars
  (1| state) + (1| psu)
fit630 <- runMLwiN(Formula = formula6
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm)
saveRDS(fit630, paste0(res_path, "fit630.RDS"))
#Urban
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_oils + urb_sugar + urb_cereal + #Nutrition vars
  (1| state) + (1| psu)
fit631 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ur)
saveRDS(fit631, paste0(res_path, "fit631_v2.RDS"))
#Rural
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_oils + rur_sugar + rur_cereal + #Nutrition vars
  (1| state) + (1| psu)
fit632 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ru)
saveRDS(fit632, paste0(res_path, "fit632_v2.RDS"))

################################################################################
# Round 4
################################################################################
#National
formula6 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  oils + sugar + cereal + #Nutrition vars
  (1| state) + (1| psu)
fit640 <- runMLwiN(Formula = formula6
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm)
saveRDS(fit640, paste0(res_path, "fit640.RDS"))
#Urban
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_oils + urb_sugar + urb_cereal + #Nutrition vars
  (1| state) + (1| psu)
fit641 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ur)
saveRDS(fit641, paste0(res_path, "fit641_v2.RDS"))
#Rural
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_oils + rur_sugar + rur_cereal + #Nutrition vars
  (1| state) + (1| psu)
fit642 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ru)
saveRDS(fit642, paste0(res_path, "fit642_v2.RDS"))

################################################################################
# oils only
################################################################################
# Round 2
################################################################################

#National
formula6 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  oils + #Nutrition vars
  (1| state) + (1| psu)
fit620 <- runMLwiN(Formula = formula6
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm)
saveRDS(fit620, paste0(res_path, "fit620_1.RDS"))
#Urban
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_oils + #Nutrition vars
  (1| state) + (1| psu)
fit621 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ur)
saveRDS(fit621, paste0(res_path, "fit621_1_v2.RDS"))
#Rural
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_oils + #Nutrition vars
  (1| state) + (1| psu)
fit622 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ru)
saveRDS(fit622, paste0(res_path, "fit622_1_v2.RDS"))

################################################################################
# Round 3
################################################################################

#National
formula6 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  oils + #Nutrition vars
  (1| state) + (1| psu)
fit630 <- runMLwiN(Formula = formula6
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm)
saveRDS(fit630, paste0(res_path, "fit630_1.RDS"))
#Urban
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_oils + #Nutrition vars
  (1| state) + (1| psu)
fit631 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ur)
saveRDS(fit631, paste0(res_path, "fit631_1_v2.RDS"))
#Rural
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_oils + #Nutrition vars
  (1| state) + (1| psu)
fit632 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ru)
saveRDS(fit632, paste0(res_path, "fit632_1_v2.RDS"))

################################################################################
# Round 4
################################################################################
#National
formula6 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  oils + #Nutrition vars
  (1| state) + (1| psu)
fit640 <- runMLwiN(Formula = formula6
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm)
saveRDS(fit640, paste0(res_path, "fit640_1.RDS"))
#Urban
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_oils + #Nutrition vars
  (1| state) + (1| psu)
fit641 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ur)
saveRDS(fit641, paste0(res_path, "fit641_1_v2.RDS"))
#Rural
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_oils + #Nutrition vars
  (1| state) + (1| psu)
fit642 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ru)
saveRDS(fit642, paste0(res_path, "fit642_1_v2.RDS"))

################################################################################
# sugar only
################################################################################
# Round 2
################################################################################

#National
formula6 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  sugar + #Nutrition vars
  (1| state) + (1| psu)
fit620 <- runMLwiN(Formula = formula6
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm)
saveRDS(fit620, paste0(res_path, "fit620_2.RDS"))
#Urban
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_sugar + #Nutrition vars
  (1| state) + (1| psu)
fit621 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ur)
saveRDS(fit621, paste0(res_path, "fit621_2_v2.RDS"))
#Rural
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_sugar + #Nutrition vars
  (1| state) + (1| psu)
fit622 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ru)
saveRDS(fit622, paste0(res_path, "fit622_2_v2.RDS"))

################################################################################
# Round 3
################################################################################

#National
formula6 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  sugar + #Nutrition vars
  (1| state) + (1| psu)
fit630 <- runMLwiN(Formula = formula6
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm)
saveRDS(fit630, paste0(res_path, "fit630_2.RDS"))
#Urban
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_sugar + #Nutrition vars
  (1| state) + (1| psu)
fit631 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ur)
saveRDS(fit631, paste0(res_path, "fit631_2_v2.RDS"))
#Rural
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_sugar + #Nutrition vars
  (1| state) + (1| psu)
fit632 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ru)
saveRDS(fit632, paste0(res_path, "fit632_2_v2.RDS"))

################################################################################
# Round 4
################################################################################
#National
formula6 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  sugar + #Nutrition vars
  (1| state) + (1| psu)
fit640 <- runMLwiN(Formula = formula6
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm)
saveRDS(fit640, paste0(res_path, "fit640_2.RDS"))
#Urban
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_sugar + #Nutrition vars
  (1| state) + (1| psu)
fit641 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ur)
saveRDS(fit641, paste0(res_path, "fit641_2_v2.RDS"))
#Rural
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_sugar + #Nutrition vars
  (1| state) + (1| psu)
fit642 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ru)
saveRDS(fit642, paste0(res_path, "fit642_2_v2.RDS"))

################################################################################
# cereal only
################################################################################
# Round 2
################################################################################

#National
formula6 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  cereal + #Nutrition vars
  (1| state) + (1| psu)
fit620 <- runMLwiN(Formula = formula6
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm)
saveRDS(fit620, paste0(res_path, "fit620_3.RDS"))
#Urban
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_cereal + #Nutrition vars
  (1| state) + (1| psu)
fit621 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ur)
saveRDS(fit621, paste0(res_path, "fit621_3_v2.RDS"))
#Rural
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_cereal + #Nutrition vars
  (1| state) + (1| psu)
fit622 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs2_wm_ru)
saveRDS(fit622, paste0(res_path, "fit622_3_v2.RDS"))

################################################################################
# Round 3
################################################################################

#National
formula6 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  cereal + #Nutrition vars
  (1| state) + (1| psu)
fit630 <- runMLwiN(Formula = formula6
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm)
saveRDS(fit630, paste0(res_path, "fit630_3.RDS"))
#Urban
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_cereal + #Nutrition vars
  (1| state) + (1| psu)
fit631 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ur)
saveRDS(fit631, paste0(res_path, "fit631_3_v2.RDS"))
#Rural
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_cereal + #Nutrition vars
  (1| state) + (1| psu)
fit632 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs3_wm_ru)
saveRDS(fit632, paste0(res_path, "fit632_3_v2.RDS"))

################################################################################
# Round 4
################################################################################
#National
formula6 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  cereal + #Nutrition vars
  (1| state) + (1| psu)
fit640 <- runMLwiN(Formula = formula6
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm)
saveRDS(fit640, paste0(res_path, "fit640_3.RDS"))
#Urban
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_cereal + #Nutrition vars
  (1| state) + (1| psu)
fit641 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ur)
saveRDS(fit641, paste0(res_path, "fit641_3_v2.RDS"))
#Rural
formula61 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_cereal + #Nutrition vars
  (1| state) + (1| psu)
fit642 <- runMLwiN(Formula = formula61
                   , D = "Binomial"
                   , estoptions = estopt2
                   , data = nfhs4_wm_ru)
saveRDS(fit642, paste0(res_path, "fit642_3_v2.RDS"))
