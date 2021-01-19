################################################################################
#economic random effect models
#Fixed      : Individual covariates
#Random     : Economic variables
#Levels     : ID X PSU X State X Round
################################################################################
source("code_support\\10_model_inits.R")
formula24 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_stateround + #residence_type + #Individual vars
  #log(gdp) + #Eco vars
  (1+log(gdp)| round) + (1| state) + (1| psu)
fit24 <- runMLwiN(Formula = formula24
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit24, paste0(res_path, "fit24_v5.RDS"))

formula25 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_stateround + #residence_type + #Individual vars
  #lfpr + #Eco vars
  (1+lfpr| round) + (1| state) + (1| psu)
fit25 <- runMLwiN(Formula = formula25
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit25, paste0(res_path, "fit25_v5.RDS"))

formula26 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_stateround + #residence_type + #Individual vars
  #(literacy) + #Eco vars
  (1+literacy| round) + (1| state) + (1| psu)
fit26 <- runMLwiN(Formula = formula26
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit26, paste0(res_path, "fit26_v5.RDS"))
formula27 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_stateround + #residence_type + #Individual vars
  #(physical_sedentary) + #Eco vars
  (1+physical_sedentary| round) + (1| state) + (1| psu)
fit27 <- runMLwiN(Formula = formula27
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit27, paste0(res_path, "fit27_v5.RDS"))
