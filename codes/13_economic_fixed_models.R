################################################################################
#economic fixed effect models
#Fixed      : Individual covariates + Economic variables
#Random     : NIL
#Levels     : ID X PSU X State X Round
################################################################################
source("code_support\\10_model_inits.R")
formula2 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  log(gdp) + literacy + physical_sedentary + #Eco vars
  (1| round) + (1| state) + (1| psu)
fit21 <- runMLwiN(Formula = formula2
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit21, paste0(res_path, "fit21.RDS"))
formula22 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  log(gdp) + literacy + physical_sedentary + #Eco vars
  (1| round) + (1| state) + (1| psu)
fit22 <- runMLwiN(Formula = formula22
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 1,])
saveRDS(fit22, paste0(res_path, "fit22.RDS"))
fit23 <- runMLwiN(Formula = formula22
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 2,])
saveRDS(fit23, paste0(res_path, "fit23.RDS"))

################################################################################
# gdp
################################################################################
formula2 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  log(gdp) +
  (1| round) + (1| state) + (1| psu)
fit21 <- runMLwiN(Formula = formula2
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit21, paste0(res_path, "fit211.RDS"))
formula22 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  log(gdp) +
  (1| round) + (1| state) + (1| psu)
fit22 <- runMLwiN(Formula = formula22
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 1,])
saveRDS(fit22, paste0(res_path, "fit221.RDS"))
fit23 <- runMLwiN(Formula = formula22
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 2,])
saveRDS(fit23, paste0(res_path, "fit231.RDS"))

################################################################################
# literacy
################################################################################
formula2 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  literacy +
  (1| round) + (1| state) + (1| psu)
fit21 <- runMLwiN(Formula = formula2
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit21, paste0(res_path, "fit213.RDS"))
formula22 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  literacy +
  (1| round) + (1| state) + (1| psu)
fit22 <- runMLwiN(Formula = formula22
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 1,])
saveRDS(fit22, paste0(res_path, "fit223.RDS"))
fit23 <- runMLwiN(Formula = formula22
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 2,])
saveRDS(fit23, paste0(res_path, "fit233.RDS"))

################################################################################
# physical_sedentary
################################################################################
formula2 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  physical_sedentary +
  (1| round) + (1| state) + (1| psu)
fit21 <- runMLwiN(Formula = formula2
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit21, paste0(res_path, "fit214.RDS"))
formula22 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  physical_sedentary_urb +
  (1| round) + (1| state) + (1| psu)
fit22 <- runMLwiN(Formula = formula22
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 1,])
saveRDS(fit22, paste0(res_path, "fit224_v2.RDS"))
formula23 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  physical_sedentary_rur +
  (1| round) + (1| state) + (1| psu)
fit23 <- runMLwiN(Formula = formula23
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 2,])
saveRDS(fit23, paste0(res_path, "fit234_v2.RDS"))
