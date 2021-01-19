rm(list = setdiff(ls(), glo_vars))
nfhs_wm <- readRDS(paste0(data_path, "nfhs_wm_v2.rds"))
nfhs_wm <- data.frame(nfhs_wm)
nfhs_wm$lfpr <- nfhs_wm$lfpr/10
nfhs_wm$literacy <- nfhs_wm$literacy/10
nfhs_wm$physical_sedentary <- nfhs_wm$physical_sedentary/10
nfhs_wm$physical_sedentary_rur <- nfhs_wm$physical_sedentary_rur/10
nfhs_wm$physical_sedentary_urb <- nfhs_wm$physical_sedentary_urb/10
nfhs_wm$urbanisation <- nfhs_wm$urbanisation/10
nfhs_wm$cereal <- nfhs_wm$cereal/10
nfhs_wm$urb_cereal <- nfhs_wm$urb_cereal/10
nfhs_wm$rur_cereal <- nfhs_wm$rur_cereal/10
cat_data <- c("ovrwt"
              , "ovrwt_23"
              , "parity"
              , "state"
              , "round"
              , "psu"
              , "age_category"
              , "education"
              , "caste"
              , "wealth_index"
              , "wealth_index_sm"
              , "wealth_index_stateround"
              , "wealth_index_comb"
              , "residence_type"
)
nfhs_wm[,cat_data] <- lapply(nfhs_wm[,cat_data], as.factor)
options(MLwiN_path = "C:\\Program Files\\MLwiN v3.03\\mlwin.exe")
#mcmc options
it = 5000; burn = 500
result <- list()
estopt = list(optimat = TRUE
              , EstM = 1
              , resi.store = TRUE
              , mcmcMeth = list(iterations = it
                                , burnin = burn
                                , nchains = 1)
              , xc = TRUE
)
estopt2 = list(optimat = TRUE
              , EstM = 1
              , resi.store = TRUE
              , mcmcMeth = list(iterations = it
                                , burnin = burn)
              #, seed = 1000
              , xc = FALSE
)
ind_vars <- c("education", "age_category", "caste", "wealth_index_sm","parity")
eco_vars <- c("gdp", "lfpr", "literacy", "hdi", "physical_sedentary", "urbanisation")
ntr_vars <- c("oils", "sugar", "cereal")
rnd_vars <- c("state", "psu", "individual")
mod_vars <- c("DIC")
diagnostic <- function(fit) {
  out = data.table()
  for(i in colnames(fit@chains[[1]])){
    out = rbind(out,data.table(t(c(i,gelman.diag(fit@chains[,i])))))
  }
  out = out[-nrow(out),]
  colnames(out) <- c("variable", "psrf", "upper_ci")
  return(out)
}
