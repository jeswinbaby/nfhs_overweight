################################################################################
#Null models
#Fixed      : NIL
#Random     : NIL
#Levels     : ID X PSU X State X Round
################################################################################
source("code_support\\10_model_inits.R")
formula01 <- logit(ovrwt_23) ~ 1 +
  (1| round) + (1| state) + (1| psu)
fit01 <- runMLwiN(Formula = formula01
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm)
saveRDS(fit01, paste0(res_path, "fit01.RDS"))
fit02 <- runMLwiN(Formula = formula01
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type ==1,])
saveRDS(fit02, paste0(res_path, "fit02.RDS"))
fit03 <- runMLwiN(Formula = formula01
                  , D = "Binomial"
                  , estoptions = estopt
                  , data = nfhs_wm[nfhs_wm$residence_type ==2,])
saveRDS(fit03, paste0(res_path, "fit03.RDS"))