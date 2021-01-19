################################################################################
#overall fixed effects models
#Fixed      : Individual covariates + Economic Variables + Nutrition Vars
#Random     : NULL
#Levels     : ID X PSU X State X Round
################################################################################
source("code_support\\10_model_inits.R")
formula4 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type + #Individual vars
  log(gdp) + literacy + physical_sedentary + #Eco vars
  oils + sugar + cereal + #Nutrition vars
  (1| round) + (1| state) + (1| psu)
fit41 <- runMLwiN(Formula = formula4
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit41, paste0(res_path, "fit41_v4.RDS"))
formula42 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  log(gdp) + literacy + physical_sedentary + #Eco vars
  oils + sugar + cereal + #Nutrition vars
  (1| round) + (1| state) + (1| psu)
fit42 <- runMLwiN(Formula = formula4
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 1,])
saveRDS(fit42, paste0(res_path, "fit42_v4.RDS"))

fit43 <- runMLwiN(Formula = formula4
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 2,])
saveRDS(fit43, paste0(res_path, "fit43_v4.RDS"))