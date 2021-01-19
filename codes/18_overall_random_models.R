################################################################################
#Overall random effect models
#Fixed      : Individual covariates
#Random     : Economic variables
#Levels     : ID X PSU X State X Round
################################################################################
source("code_support\\10_model_inits.R")
formula4 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_stateround + residence_type + #Individual vars
  log(gdp) + lfpr + literacy + physical_sedentary + #Eco vars
  oils + sugar + cereal + #Nutrition vars
  (log(gdp) + lfpr + literacy + physical_sedentary + #Eco vars
     oils + sugar + cereal| round) + (1| state) + (1| psu)
fit44 <- runMLwiN(Formula = formula4
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit44, paste0(res_path, "fit44_v4.RDS"))
formula42 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_stateround + #Individual vars
  log(gdp) + lfpr + literacy + physical_sedentary + #Eco vars
  oils + sugar + cereal + #Nutrition vars
  (log(gdp) + lfpr + literacy + physical_sedentary + #Eco vars
     oils + sugar + cereal| round) + (1| state) + (1| psu)
fit45 <- runMLwiN(Formula = formula42
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 1,])
saveRDS(fit45, paste0(res_path, "fit45_v4.RDS"))

fit46 <- runMLwiN(Formula = formula42
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 2,])
saveRDS(fit46, paste0(res_path, "fit46_v4.RDS"))