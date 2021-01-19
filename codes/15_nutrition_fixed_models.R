################################################################################
#nutrition fixed effect models
#Fixed      : Individual covariates + Nutrition variables
#Random     : NULL
#Levels     : ID X PSU X State X Round
################################################################################
source("code_support\\10_model_inits.R")
formula3 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type+ #Individual vars
  oils + sugar + cereal + #Nutrition vars
  (1| round) + (1| state) + (1| psu)
fit31 <- runMLwiN(Formula = formula3
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit31, paste0(res_path, "fit31.RDS"))
formula32 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_oils + urb_sugar + urb_cereal + #Nutrition vars
  (1| round) + (1| state) + (1| psu)
fit32 <- runMLwiN(Formula = formula32
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 1,])
saveRDS(fit32, paste0(res_path, "fit32_v2.RDS"))
formula33 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_oils + rur_sugar + rur_cereal + #Nutrition vars
  (1| round) + (1| state) + (1| psu)
fit33 <- runMLwiN(Formula = formula33
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 2,])
saveRDS(fit33, paste0(res_path, "fit33_v2.RDS"))

################################################################################
# Oils
################################################################################
formula3 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type+ #Individual vars
  oils +
  (1| round) + (1| state) + (1| psu)
fit31 <- runMLwiN(Formula = formula3
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit31, paste0(res_path, "fit311.RDS"))
formula32 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_oils +
  (1| round) + (1| state) + (1| psu)
fit32 <- runMLwiN(Formula = formula32
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 1,])
saveRDS(fit32, paste0(res_path, "fit321_v2.RDS"))
formula33 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_oils +
  (1| round) + (1| state) + (1| psu)
fit33 <- runMLwiN(Formula = formula33
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 2,])
saveRDS(fit33, paste0(res_path, "fit331_v2.RDS"))

################################################################################
# sugar
################################################################################
formula3 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type+ #Individual vars
  sugar +
  (1| round) + (1| state) + (1| psu)
fit31 <- runMLwiN(Formula = formula3
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit31, paste0(res_path, "fit312.RDS"))
formula32 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_sugar +
  (1| round) + (1| state) + (1| psu)
fit32 <- runMLwiN(Formula = formula32
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 1,])
saveRDS(fit32, paste0(res_path, "fit322_v2.RDS"))
formula33 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_sugar +
  (1| round) + (1| state) + (1| psu)
fit33 <- runMLwiN(Formula = formula33
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 2,])
saveRDS(fit33, paste0(res_path, "fit332_v2.RDS"))


################################################################################
# cereal
################################################################################
formula3 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type+ #Individual vars
  cereal +
  (1| round) + (1| state) + (1| psu)
fit31 <- runMLwiN(Formula = formula3
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit31, paste0(res_path, "fit313.RDS"))
formula32 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  urb_cereal +
  (1| round) + (1| state) + (1| psu)
fit32 <- runMLwiN(Formula = formula32
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 1,])
saveRDS(fit32, paste0(res_path, "fit323_v2.RDS"))
formula33 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  rur_cereal +
  (1| round) + (1| state) + (1| psu)
fit33 <- runMLwiN(Formula = formula33
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 2,])
saveRDS(fit33, paste0(res_path, "fit333_v2.RDS"))