## Template for posologyr validation on simulated data: R workflow
library(readr)
library(posologyr)

#-------------------------------------------------------------------------------
# Function for MCMC and SIR estimation

# estimation of posterior distribution by SIR
mass_sir_estim <- function(index,model,dat){
  set.seed(42)
  print(index)
  eta <- poso_estim_sir(posologyr(model,dat[dat$ID == index,]),
                        n_sample=1e6,
                        n_resample=1e4,
                        return_model = FALSE)
  eta <- data.frame(index,eta)
  return(eta)
}

# estimation of posterior distribution by MCMC
mass_mcmc_estim <- function(index,model,dat){
  set.seed(42)
  print(index)
  eta <- poso_estim_mcmc(posologyr(model,dat[dat$ID == index,]),
                         n_chains = 2,
                         return_model = FALSE,n_iter = 5000,burn_in = 200)
  eta <- data.frame(index,eta)
  return(eta)
}

#-------------------------------------------------------------------------------
# Run 001
#-------------------------------------------------------------------------------

# Import data
data_to_fit001 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit001.csv")

# data management
names(data_to_fit001) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit001$DV[data_to_fit001$DV == "."] <- NA
data_to_fit001$DV <- as.numeric(data_to_fit001$DV)
data_to_fit001 <- data_to_fit001[data_to_fit001$ID %in% (1:12),]

sim_index <- unique(data_to_fit001$ID)

# SIR --------------------------------------------------------------------------

# SIR: apply the estimation function on every subject, and write the result
sir_list_001 <- lapply(sim_index,model=mod_dist_run001,dat=data_to_fit001,
                   FUN=mass_sir_estim)

sir_df_001 <- data.frame(do.call(rbind,sir_list_001))

write_csv(sir_df_001,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_001_sir.csv")

# MCMC -------------------------------------------------------------------------

# MCMC: apply the estimation function on every subject, and write the result
mcmc_list_001 <- lapply(sim_index,model=mod_dist_run001,dat=data_to_fit001,
                      FUN=mass_mcmc_estim)

mcmc_df_001 <- data.frame(do.call(rbind,mcmc_list_001))

write_csv(mcmc_df_001,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_001_mcmc.csv")

#-------------------------------------------------------------------------------
# Run 003
#-------------------------------------------------------------------------------

# Import data
data_to_fit003 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit003.csv")

# data management
names(data_to_fit003) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit003$DV[data_to_fit003$DV == "."] <- NA
data_to_fit003$DV <- as.numeric(data_to_fit003$DV)
data_to_fit003 <- data_to_fit003[data_to_fit003$ID %in% (1:12),]

sim_index <- unique(data_to_fit003$ID)

# SIR --------------------------------------------------------------------------

# SIR: apply the estimation function on every subject, and write the result
sir_list_003 <- lapply(sim_index,model=mod_dist_run003,dat=data_to_fit003,
                       FUN=mass_sir_estim)

sir_df_003 <- data.frame(do.call(rbind,sir_list_003))

write_csv(sir_df_003,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_003_sir.csv")

# MCMC -------------------------------------------------------------------------

# MCMC: apply the estimation function on every subject, and write the result
mcmc_list_003 <- lapply(sim_index,model=mod_dist_run003,dat=data_to_fit003,
                        FUN=mass_mcmc_estim)

mcmc_df_003 <- data.frame(do.call(rbind,mcmc_list_003))

write_csv(mcmc_df_003,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_003_mcmc.csv")

#-------------------------------------------------------------------------------
# Run 006
#-------------------------------------------------------------------------------

# Import data
data_to_fit006 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit006.csv")

# data management
names(data_to_fit006) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","RATE","MDV","DV","s2_sampling")
data_to_fit006$DV[data_to_fit006$DV == "."] <- NA
data_to_fit006$DV <- as.numeric(data_to_fit006$DV)
data_to_fit006 <- data_to_fit006[data_to_fit006$ID %in% (1:12),]

sim_index <- unique(data_to_fit006$ID)

# SIR --------------------------------------------------------------------------

# SIR: apply the estimation function on every subject, and write the result
sir_list_006 <- lapply(sim_index,model=mod_dist_run006,dat=data_to_fit006,
                       FUN=mass_sir_estim)

sir_df_006 <- data.frame(do.call(rbind,sir_list_006))

write_csv(sir_df_006,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_006_sir.csv")

# MCMC -------------------------------------------------------------------------

# MCMC: apply the estimation function on every subject, and write the result
mcmc_list_006 <- lapply(sim_index,model=mod_dist_run006,dat=data_to_fit006,
                        FUN=mass_mcmc_estim)

mcmc_df_006 <- data.frame(do.call(rbind,mcmc_list_006))

write_csv(mcmc_df_006,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_006_mcmc.csv")


#-------------------------------------------------------------------------------
# Run 101
#-------------------------------------------------------------------------------

# Import data
data_to_fit101 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit101.csv")

# data management
names(data_to_fit101) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit101$DV[data_to_fit101$DV == "."] <- NA
data_to_fit101$DV <- as.numeric(data_to_fit101$DV)
data_to_fit101 <- data_to_fit101[data_to_fit101$ID %in% (1:12),]

sim_index <- unique(data_to_fit101$ID)

# SIR --------------------------------------------------------------------------

# SIR: apply the estimation function on every subject, and write the result
sir_list_101 <- lapply(sim_index,model=mod_dist_run101,dat=data_to_fit101,
                       FUN=mass_sir_estim)

sir_df_101 <- data.frame(do.call(rbind,sir_list_101))

write_csv(sir_df_101,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_101_sir.csv")

# MCMC -------------------------------------------------------------------------

# MCMC: apply the estimation function on every subject, and write the result
mcmc_list_101 <- lapply(sim_index,model=mod_dist_run101,dat=data_to_fit101,
                        FUN=mass_mcmc_estim)

mcmc_df_101 <- data.frame(do.call(rbind,mcmc_list_101))

write_csv(mcmc_df_101,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_101_mcmc.csv")


#-------------------------------------------------------------------------------
# Run 201
#-------------------------------------------------------------------------------

# Import data
data_to_fit201 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit201.csv")

# data management
names(data_to_fit201) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit201$DV[data_to_fit201$DV == "."] <- NA
data_to_fit201$DV <- as.numeric(data_to_fit201$DV)
data_to_fit201 <- data_to_fit201[data_to_fit201$ID %in% (1:12),]

sim_index <- unique(data_to_fit201$ID)

# SIR --------------------------------------------------------------------------

# SIR: apply the estimation function on every subject, and write the result
sir_list_201 <- lapply(sim_index,model=mod_dist_run201,dat=data_to_fit201,
                       FUN=mass_sir_estim)

sir_df_201 <- data.frame(do.call(rbind,sir_list_201))

write_csv(sir_df_201,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_201_sir.csv")

# MCMC -------------------------------------------------------------------------

# MCMC: apply the estimation function on every subject, and write the result
mcmc_list_201 <- lapply(sim_index,model=mod_dist_run201,dat=data_to_fit201,
                        FUN=mass_mcmc_estim)

mcmc_df_201 <- data.frame(do.call(rbind,mcmc_list_201))

write_csv(mcmc_df_201,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_201_mcmc.csv")


#-------------------------------------------------------------------------------
# Run 207
#-------------------------------------------------------------------------------

# Import data
data_to_fit207 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit207.csv")

# data management
names(data_to_fit207) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit207$DV[data_to_fit207$DV == "."] <- NA
data_to_fit207$DV <- as.numeric(data_to_fit207$DV)
data_to_fit207 <- data_to_fit207[data_to_fit207$ID %in% (1:12),]

sim_index <- unique(data_to_fit207$ID)

# SIR --------------------------------------------------------------------------

# SIR: apply the estimation function on every subject, and write the result
sir_list_207 <- lapply(sim_index,model=mod_dist_run207,dat=data_to_fit207,
                       FUN=mass_sir_estim)

sir_df_207 <- data.frame(do.call(rbind,sir_list_207))

write_csv(sir_df_207,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_207_sir.csv")

# MCMC -------------------------------------------------------------------------

# MCMC: apply the estimation function on every subject, and write the result
mcmc_list_207 <- lapply(sim_index,model=mod_dist_run207,dat=data_to_fit207,
                        FUN=mass_mcmc_estim)

mcmc_df_207 <- data.frame(do.call(rbind,mcmc_list_207))

write_csv(mcmc_df_207,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_207_mcmc.csv")


#-------------------------------------------------------------------------------
# Run 301
#-------------------------------------------------------------------------------

# Import data
data_to_fit301 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit301.csv")

# data management
names(data_to_fit301) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","BW","SEX","s2_sampling")
data_to_fit301$DV[data_to_fit301$DV == "."] <- NA
data_to_fit301$DV <- as.numeric(data_to_fit301$DV)
data_to_fit301 <- data_to_fit301[data_to_fit301$ID %in% (1:12),]

sim_index <- unique(data_to_fit301$ID)

# SIR --------------------------------------------------------------------------

# SIR: apply the estimation function on every subject, and write the result
sir_list_301 <- lapply(sim_index,model=mod_dist_run301,dat=data_to_fit301,
                       FUN=mass_sir_estim)

sir_df_301 <- data.frame(do.call(rbind,sir_list_301))

write_csv(sir_df_301,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_301_sir.csv")

# MCMC -------------------------------------------------------------------------

# MCMC: apply the estimation function on every subject, and write the result
mcmc_list_301 <- lapply(sim_index,model=mod_dist_run301,dat=data_to_fit301,
                        FUN=mass_mcmc_estim)

mcmc_df_301 <- data.frame(do.call(rbind,mcmc_list_301))

write_csv(mcmc_df_301,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_301_mcmc.csv")


#-------------------------------------------------------------------------------
# Run 405
#-------------------------------------------------------------------------------

# Import data
data_to_fit405 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit405.csv")

# data management
names(data_to_fit405) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit405$DV[data_to_fit405$DV == "."] <- NA
data_to_fit405$DV <- as.numeric(data_to_fit405$DV)
data_to_fit405 <- data_to_fit405[data_to_fit405$ID %in% (1:12),]

sim_index <- unique(data_to_fit405$ID)

# SIR --------------------------------------------------------------------------

# SIR: apply the estimation function on every subject, and write the result
sir_list_405 <- lapply(sim_index,model=mod_dist_run405,dat=data_to_fit405,
                       FUN=mass_sir_estim)

sir_df_405 <- data.frame(do.call(rbind,sir_list_405))

write_csv(sir_df_405,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_405_sir.csv")

# MCMC -------------------------------------------------------------------------

# MCMC: apply the estimation function on every subject, and write the result
mcmc_list_405 <- lapply(sim_index,model=mod_dist_run405,dat=data_to_fit405,
                        FUN=mass_mcmc_estim)

mcmc_df_405 <- data.frame(do.call(rbind,mcmc_list_405))

write_csv(mcmc_df_405,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_405_mcmc.csv")


#-------------------------------------------------------------------------------
# Run 504
#-------------------------------------------------------------------------------

# Import data
data_to_fit504 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit504.csv")

# data management
names(data_to_fit504) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit504$DV[data_to_fit504$DV == "."] <- NA
data_to_fit504$DV <- as.numeric(data_to_fit504$DV)
data_to_fit504 <- data_to_fit504[data_to_fit504$ID %in% (1:12),]

sim_index <- unique(data_to_fit504$ID)

# SIR --------------------------------------------------------------------------

# SIR: apply the estimation function on every subject, and write the result
sir_list_504 <- lapply(sim_index,model=mod_dist_run504,dat=data_to_fit504,
                       FUN=mass_sir_estim)

sir_df_504 <- data.frame(do.call(rbind,sir_list_504))

write_csv(sir_df_504,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_504_sir.csv")

# MCMC -------------------------------------------------------------------------

# MCMC: apply the estimation function on every subject, and write the result
mcmc_list_504 <- lapply(sim_index,model=mod_dist_run504,dat=data_to_fit504,
                        FUN=mass_mcmc_estim)

mcmc_df_504 <- data.frame(do.call(rbind,mcmc_list_504))

write_csv(mcmc_df_504,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_504_mcmc.csv")


#-------------------------------------------------------------------------------
# Run 513
#-------------------------------------------------------------------------------

# Import data
data_to_fit513 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit513.csv")

# data management
names(data_to_fit513) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit513$DV[data_to_fit513$DV == "."] <- NA
data_to_fit513$DV <- as.numeric(data_to_fit513$DV)
data_to_fit513 <- data_to_fit513[data_to_fit513$ID %in% (1:12),]

sim_index <- unique(data_to_fit513$ID)

# SIR --------------------------------------------------------------------------

# SIR: apply the estimation function on every subject, and write the result
sir_list_513 <- lapply(sim_index,model=mod_dist_run513,dat=data_to_fit513,
                       FUN=mass_sir_estim)

sir_df_513 <- data.frame(do.call(rbind,sir_list_513))

write_csv(sir_df_513,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_513_sir.csv")

# MCMC -------------------------------------------------------------------------

# MCMC: apply the estimation function on every subject, and write the result
mcmc_list_513 <- lapply(sim_index,model=mod_dist_run513,dat=data_to_fit513,
                        FUN=mass_mcmc_estim)

mcmc_df_513 <- data.frame(do.call(rbind,mcmc_list_513))

write_csv(mcmc_df_513,
          "~/posologyr-pharmaceutics/posologyr-output-dist/eta_513_mcmc.csv")

