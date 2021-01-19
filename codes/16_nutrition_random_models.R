################################################################################
#Nutrition random effect models
#Fixed      : Individual covariates
#Random     : Nutrition variables
#Levels     : ID X PSU X State X Round
################################################################################
source("code_support\\10_model_inits.R")
formula34 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_stateround + #residence_type +#Individual vars
  #oils + #Nutrition vars
  (1+oils| round) + (1| state) + (1| psu)
fit34 <- runMLwiN(Formula = formula34
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit34, paste0(res_path, "fit34_v5.RDS"))

formula35 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_stateround + #residence_type +#Individual vars
  #sugar + #Nutrition vars
  (1+sugar| round) + (1| state) + (1| psu)
fit35 <- runMLwiN(Formula = formula35
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit35, paste0(res_path, "fit35_v5.RDS"))

formula36 <- logit(ovrwt) ~ 1 +
  age_category + education + caste + parity + wealth_index_stateround + #residence_type +#Individual vars
  cereal +#Nutrition vars
  (1+cereal| round) + (1| state) + (1| psu)
fit36 <- runMLwiN(Formula = formula36
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit36, paste0(res_path, "fit36_v5.RDS"))
