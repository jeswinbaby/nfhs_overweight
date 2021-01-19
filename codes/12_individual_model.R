################################################################################
#Individual covariate model
#Fixed      : Individual covariates
#Random     : NIL
#Levels     : ID X PSU X State X Round
################################################################################
source("code_support\\10_model_inits.R")
formula1 <- logit(ovrwt_23) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + residence_type +#Individual vars
  (1| round) + (1| state) + (1| psu)
fit11 <- runMLwiN(Formula = formula1
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit11, paste0(res_path, "fit11.RDS"))

#Urban
formula2 <- logit(ovrwt_23) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  (1| round) + (1| state) + (1| psu)
fit12 <- runMLwiN(Formula = formula2
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 1,])
saveRDS(fit12, paste0(res_path, "fit12.RDS"))

#Rural
formula3 <- logit(ovrwt_23) ~ 1 +
  age_category + education + caste + parity + wealth_index_sm + #Individual vars
  (1| round) + (1| state) + (1| psu)
fit13 <- runMLwiN(Formula = formula3
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type == 2,])
saveRDS(fit13, paste0(res_path, "fit13.RDS"))