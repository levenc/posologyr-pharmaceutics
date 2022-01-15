# libraries --------------------------------------------------------------------
library(posologyr)
library(readr)

# functions for estimation, and max difference evaluation ----------------------

# estimation of ETAs
mass_map_estim <- function(index,model,dat){
  sed.seed(42)
  print(index)
  eta <- try(poso_estim_map(posologyr(model,dat[dat$ID == index,],
                                      nocb=TRUE),
                            return_ofv=TRUE,
                            return_model=F),
             silent=F)
  if (class(eta) == 'try-error') {
    return(NA)
  } else {
    return(unlist(eta))
  }
}

# maximum absolute difference of ETA
max_abs_dif <- function(index,eta_poso,eta_nm){
  eta_poso_vec <- eta_poso[index,1:ncol(eta_poso)-1]
  eta_nm_vec   <- eta_nm[index,2:(length(eta_poso_vec)+1)]
  eta_dif      <- eta_poso_vec-eta_nm_vec
  max_dif      <- max(abs(eta_dif))
  ofv_dif      <- eta_poso[index,"ofv"]-eta_nm[index,"OFV"]
  global_dif   <- cbind(max_dif,ofv_dif)
  return(global_dif)
}


# ------------------------------------------------------------------------------
# run001 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_001 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit001.csv")

# data management
names(data_to_fit_001) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_001$DV[data_to_fit_001$DV == "."] <- NA
data_to_fit_001$DV <- as.numeric(data_to_fit_001$DV)

# posologyr --------------------------------------------------------------------
sim_index_001 <- unique(data_to_fit_001$ID)

# apply the estimation function on every subject
eta_list_001 <- lapply(sim_index_001,model=mod_run001,dat=data_to_fit_001,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_001 <- do.call(rbind,eta_list_001)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_001 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run001.ofv")

# apply the difference function on every subject
dif_eta_list_001 <- lapply(sim_index_001,eta_poso=eta_table_001,
                           eta_nm=eta_nm_df_001,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_001 <- do.call(rbind,dif_eta_list_001)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_001),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_001.csv")
write_csv(as.data.frame(dif_eta_table_001),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_001.csv")


# ------------------------------------------------------------------------------
# run002 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_002 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit002.csv")

# data management
names(data_to_fit_002) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","DV","s2_sampling")
data_to_fit_002$DV[data_to_fit_002$DV == "."] <- NA
data_to_fit_002$DV <- as.numeric(data_to_fit_002$DV)

# posologyr --------------------------------------------------------------------
sim_index_002 <- unique(data_to_fit_002$ID)

# apply the estimation function on every subject
eta_list_002 <- lapply(sim_index_002,model=mod_run002,dat=data_to_fit_002,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_002 <- do.call(rbind,eta_list_002)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_002 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run002.ofv")

# apply the difference function on every subject
dif_eta_list_002 <- lapply(sim_index_002,eta_poso=eta_table_002,
                           eta_nm=eta_nm_df_002,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_002 <- do.call(rbind,dif_eta_list_002)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_002),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_002.csv")
write_csv(as.data.frame(dif_eta_table_002),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_002.csv")


# ------------------------------------------------------------------------------
# run003 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_003 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit003.csv")

# data management
names(data_to_fit_003) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_003$DV[data_to_fit_003$DV == "."] <- NA
data_to_fit_003$DV <- as.numeric(data_to_fit_003$DV)

# posologyr --------------------------------------------------------------------
sim_index_003 <- unique(data_to_fit_003$ID)

# apply the estimation function on every subject
eta_list_003 <- lapply(sim_index_003,model=mod_run003,dat=data_to_fit_003,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_003 <- do.call(rbind,eta_list_003)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_003 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run003.ofv")

# apply the difference function on every subject
dif_eta_list_003 <- lapply(sim_index_003,eta_poso=eta_table_003,
                           eta_nm=eta_nm_df_003,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_003 <- do.call(rbind,dif_eta_list_003)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_003),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_003.csv")
write_csv(as.data.frame(dif_eta_table_003),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_003.csv")


# ------------------------------------------------------------------------------
# run004 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_004 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit004.csv")

# data management
names(data_to_fit_004) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","DV","s2_sampling")
data_to_fit_004$DV[data_to_fit_004$DV == "."] <- NA
data_to_fit_004$DV <- as.numeric(data_to_fit_004$DV)

# posologyr --------------------------------------------------------------------
sim_index_004 <- unique(data_to_fit_004$ID)

# apply the estimation function on every subject
eta_list_004 <- lapply(sim_index_004,model=mod_run004,dat=data_to_fit_004,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_004 <- do.call(rbind,eta_list_004)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_004 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run004.ofv")

# apply the difference function on every subject
dif_eta_list_004 <- lapply(sim_index_004,eta_poso=eta_table_004,
                           eta_nm=eta_nm_df_004,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_004 <- do.call(rbind,dif_eta_list_004)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_004),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_004.csv")
write_csv(as.data.frame(dif_eta_table_004),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_004.csv")


# ------------------------------------------------------------------------------
# run005 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_005 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit005.csv")

# data management
names(data_to_fit_005) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","DV","s2_sampling")
data_to_fit_005$DV[data_to_fit_005$DV == "."] <- NA
data_to_fit_005$DV <- as.numeric(data_to_fit_005$DV)

# posologyr --------------------------------------------------------------------
sim_index_005 <- unique(data_to_fit_005$ID)

# apply the estimation function on every subject
eta_list_005 <- lapply(sim_index_005,model=mod_run005,dat=data_to_fit_005,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_005 <- do.call(rbind,eta_list_005)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_005 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run005.ofv")

# apply the difference function on every subject
dif_eta_list_005 <- lapply(sim_index_005,eta_poso=eta_table_005,
                           eta_nm=eta_nm_df_005,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_005 <- do.call(rbind,dif_eta_list_005)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_005),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_005.csv")
write_csv(as.data.frame(dif_eta_table_005),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_005.csv")


# ------------------------------------------------------------------------------
# run006 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_006 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit006.csv")

# data management
names(data_to_fit_006) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","DV","s2_sampling")
data_to_fit_006$DV[data_to_fit_006$DV == "."] <- NA
data_to_fit_006$DV <- as.numeric(data_to_fit_006$DV)

# posologyr --------------------------------------------------------------------
sim_index_006 <- unique(data_to_fit_006$ID)

# apply the estimation function on every subject
eta_list_006 <- lapply(sim_index_006,model=mod_run006,dat=data_to_fit_006,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_006 <- do.call(rbind,eta_list_006)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_006 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run006.ofv")

# apply the difference function on every subject
dif_eta_list_006 <- lapply(sim_index_006,eta_poso=eta_table_006,
                           eta_nm=eta_nm_df_006,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_006 <- do.call(rbind,dif_eta_list_006)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_006),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_006.csv")
write_csv(as.data.frame(dif_eta_table_006),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_006.csv")


# ------------------------------------------------------------------------------
# run007 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_007 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit007.csv")

# data management
names(data_to_fit_007) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_007$DV[data_to_fit_007$DV == "."] <- NA
data_to_fit_007$DV <- as.numeric(data_to_fit_007$DV)

# posologyr --------------------------------------------------------------------
sim_index_007 <- unique(data_to_fit_007$ID)

# apply the estimation function on every subject
eta_list_007 <- lapply(sim_index_007,model=mod_run007,dat=data_to_fit_007,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_007 <- do.call(rbind,eta_list_007)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_007 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run007.ofv")

# apply the difference function on every subject
dif_eta_list_007 <- lapply(sim_index_007,eta_poso=eta_table_007,
                           eta_nm=eta_nm_df_007,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_007 <- do.call(rbind,dif_eta_list_007)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_007),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_007.csv")
write_csv(as.data.frame(dif_eta_table_007),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_007.csv")


# ------------------------------------------------------------------------------
# run008 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_008 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit008.csv")

# data management
names(data_to_fit_008) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_008$DV[data_to_fit_008$DV == "."] <- NA
data_to_fit_008$DV <- as.numeric(data_to_fit_008$DV)

# posologyr --------------------------------------------------------------------
sim_index_008 <- unique(data_to_fit_008$ID)

# apply the estimation function on every subject
eta_list_008 <- lapply(sim_index_008,model=mod_run008,dat=data_to_fit_008,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_008 <- do.call(rbind,eta_list_008)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_008 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run008.ofv")

# apply the difference function on every subject
dif_eta_list_008 <- lapply(sim_index_008,eta_poso=eta_table_008,
                           eta_nm=eta_nm_df_008,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_008 <- do.call(rbind,dif_eta_list_008)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_008),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_008.csv")
write_csv(as.data.frame(dif_eta_table_008),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_008.csv")


# ------------------------------------------------------------------------------
# run101 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_101 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit101.csv")

# data management
names(data_to_fit_101) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_101$DV[data_to_fit_101$DV == "."] <- NA
data_to_fit_101$DV <- as.numeric(data_to_fit_101$DV)

# posologyr --------------------------------------------------------------------
sim_index_101 <- unique(data_to_fit_101$ID)

# apply the estimation function on every subject
eta_list_101 <- lapply(sim_index_101,model=mod_run101,dat=data_to_fit_101,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_101 <- do.call(rbind,eta_list_101)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_101 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run101.ofv")

# apply the difference function on every subject
dif_eta_list_101 <- lapply(sim_index_101,eta_poso=eta_table_101,
                           eta_nm=eta_nm_df_101,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_101 <- do.call(rbind,dif_eta_list_101)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_101),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_101.csv")
write_csv(as.data.frame(dif_eta_table_101),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_101.csv")


# ------------------------------------------------------------------------------
# run102 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_102 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit102.csv")

# data management
names(data_to_fit_102) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","DV","s2_sampling")
data_to_fit_102$DV[data_to_fit_102$DV == "."] <- NA
data_to_fit_102$DV <- as.numeric(data_to_fit_102$DV)

# posologyr --------------------------------------------------------------------
sim_index_102 <- unique(data_to_fit_102$ID)

# apply the estimation function on every subject
eta_list_102 <- lapply(sim_index_102,model=mod_run102,dat=data_to_fit_102,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_102 <- do.call(rbind,eta_list_102)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_102 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run102.ofv")

# apply the difference function on every subject
dif_eta_list_102 <- lapply(sim_index_102,eta_poso=eta_table_102,
                           eta_nm=eta_nm_df_102,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_102 <- do.call(rbind,dif_eta_list_102)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_102),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_102.csv")
write_csv(as.data.frame(dif_eta_table_102),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_102.csv")


# ------------------------------------------------------------------------------
# run201 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_201 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit201.csv")

# data management
names(data_to_fit_201) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_201$DV[data_to_fit_201$DV == "."] <- NA
data_to_fit_201$DV <- as.numeric(data_to_fit_201$DV)

# posologyr --------------------------------------------------------------------
sim_index_201 <- unique(data_to_fit_201$ID)

# apply the estimation function on every subject
eta_list_201 <- lapply(sim_index_201,model=mod_run201,dat=data_to_fit_201,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_201 <- do.call(rbind,eta_list_201)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_201 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run201.ofv")

# apply the difference function on every subject
dif_eta_list_201 <- lapply(sim_index_201,eta_poso=eta_table_201,
                           eta_nm=eta_nm_df_201,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_201 <- do.call(rbind,dif_eta_list_201)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_201),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_201.csv")
write_csv(as.data.frame(dif_eta_table_201),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_201.csv")


# ------------------------------------------------------------------------------
# run202 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_202 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit202.csv")

# data management
names(data_to_fit_202) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","DV","s2_sampling")
data_to_fit_202$DV[data_to_fit_202$DV == "."] <- NA
data_to_fit_202$DV <- as.numeric(data_to_fit_202$DV)

# posologyr --------------------------------------------------------------------
sim_index_202 <- unique(data_to_fit_202$ID)

# apply the estimation function on every subject
eta_list_202 <- lapply(sim_index_202,model=mod_run202,dat=data_to_fit_202,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_202 <- do.call(rbind,eta_list_202)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_202 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run202.ofv")

# apply the difference function on every subject
dif_eta_list_202 <- lapply(sim_index_202,eta_poso=eta_table_202,
                           eta_nm=eta_nm_df_202,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_202 <- do.call(rbind,dif_eta_list_202)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_202),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_202.csv")
write_csv(as.data.frame(dif_eta_table_202),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_202.csv")


# ------------------------------------------------------------------------------
# run203 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_203 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit203.csv")

# data management
names(data_to_fit_203) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_203$DV[data_to_fit_203$DV == "."] <- NA
data_to_fit_203$DV <- as.numeric(data_to_fit_203$DV)

# posologyr --------------------------------------------------------------------
sim_index_203 <- unique(data_to_fit_203$ID)

# apply the estimation function on every subject
eta_list_203 <- lapply(sim_index_203,model=mod_run203,dat=data_to_fit_203,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_203 <- do.call(rbind,eta_list_203)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_203 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run203.ofv")

# apply the difference function on every subject
dif_eta_list_203 <- lapply(sim_index_203,eta_poso=eta_table_203,
                           eta_nm=eta_nm_df_203,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_203 <- do.call(rbind,dif_eta_list_203)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_203),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_203.csv")
write_csv(as.data.frame(dif_eta_table_203),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_203.csv")


# ------------------------------------------------------------------------------
# run204 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_204 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit204.csv")

# data management
names(data_to_fit_204) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","DV","s2_sampling")
data_to_fit_204$DV[data_to_fit_204$DV == "."] <- NA
data_to_fit_204$DV <- as.numeric(data_to_fit_204$DV)

# posologyr --------------------------------------------------------------------
sim_index_204 <- unique(data_to_fit_204$ID)

# apply the estimation function on every subject
eta_list_204 <- lapply(sim_index_204,model=mod_run204,dat=data_to_fit_204,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_204 <- do.call(rbind,eta_list_204)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_204 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run204.ofv")

# apply the difference function on every subject
dif_eta_list_204 <- lapply(sim_index_204,eta_poso=eta_table_204,
                           eta_nm=eta_nm_df_204,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_204 <- do.call(rbind,dif_eta_list_204)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_204),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_204.csv")
write_csv(as.data.frame(dif_eta_table_204),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_204.csv")


# ------------------------------------------------------------------------------
# run205 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_205 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit205.csv")

# data management
names(data_to_fit_205) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_205$DV[data_to_fit_205$DV == "."] <- NA
data_to_fit_205$DV <- as.numeric(data_to_fit_205$DV)

# posologyr --------------------------------------------------------------------
sim_index_205 <- unique(data_to_fit_205$ID)

# apply the estimation function on every subject
eta_list_205 <- lapply(sim_index_205,model=mod_run205,dat=data_to_fit_205,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_205 <- do.call(rbind,eta_list_205)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_205 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run205.ofv")

# apply the difference function on every subject
dif_eta_list_205 <- lapply(sim_index_205,eta_poso=eta_table_205,
                           eta_nm=eta_nm_df_205,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_205 <- do.call(rbind,dif_eta_list_205)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_205),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_205.csv")
write_csv(as.data.frame(dif_eta_table_205),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_205.csv")


# ------------------------------------------------------------------------------
# run206 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_206 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit206.csv")

# data management
names(data_to_fit_206) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","DV","s2_sampling")
data_to_fit_206$DV[data_to_fit_206$DV == "."] <- NA
data_to_fit_206$DV <- as.numeric(data_to_fit_206$DV)

# posologyr --------------------------------------------------------------------
sim_index_206 <- unique(data_to_fit_206$ID)

# apply the estimation function on every subject
eta_list_206 <- lapply(sim_index_206,model=mod_run206,dat=data_to_fit_206,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_206 <- do.call(rbind,eta_list_206)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_206 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run206.ofv")

# apply the difference function on every subject
dif_eta_list_206 <- lapply(sim_index_206,eta_poso=eta_table_206,
                           eta_nm=eta_nm_df_206,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_206 <- do.call(rbind,dif_eta_list_206)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_206),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_206.csv")
write_csv(as.data.frame(dif_eta_table_206),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_206.csv")


# ------------------------------------------------------------------------------
# run207 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_207 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit207.csv")

# data management
names(data_to_fit_207) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_207$DV[data_to_fit_207$DV == "."] <- NA
data_to_fit_207$DV <- as.numeric(data_to_fit_207$DV)

# posologyr --------------------------------------------------------------------
sim_index_207 <- unique(data_to_fit_207$ID)

# apply the estimation function on every subject
eta_list_207 <- lapply(sim_index_207,model=mod_run207,dat=data_to_fit_207,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_207 <- do.call(rbind,eta_list_207)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_207 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run207.ofv")

# apply the difference function on every subject
dif_eta_list_207 <- lapply(sim_index_207,eta_poso=eta_table_207,
                           eta_nm=eta_nm_df_207,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_207 <- do.call(rbind,dif_eta_list_207)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_207),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_207.csv")
write_csv(as.data.frame(dif_eta_table_207),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_207.csv")


# ------------------------------------------------------------------------------
# run208 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_208 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit208.csv")

# data management
names(data_to_fit_208) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","DV","s2_sampling")
data_to_fit_208$DV[data_to_fit_208$DV == "."] <- NA
data_to_fit_208$DV <- as.numeric(data_to_fit_208$DV)

# posologyr --------------------------------------------------------------------
sim_index_208 <- unique(data_to_fit_208$ID)

# apply the estimation function on every subject
eta_list_208 <- lapply(sim_index_208,model=mod_run208,dat=data_to_fit_208,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_208 <- do.call(rbind,eta_list_208)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_208 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run208.ofv")

# apply the difference function on every subject
dif_eta_list_208 <- lapply(sim_index_208,eta_poso=eta_table_208,
                           eta_nm=eta_nm_df_208,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_208 <- do.call(rbind,dif_eta_list_208)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_208),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_208.csv")
write_csv(as.data.frame(dif_eta_table_208),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_208.csv")


# ------------------------------------------------------------------------------
# run301 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_301 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit301.csv")

# data management
names(data_to_fit_301) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","BW","SEX","s2_sampling")
data_to_fit_301$DV[data_to_fit_301$DV == "."] <- NA
data_to_fit_301$DV <- as.numeric(data_to_fit_301$DV)

# posologyr --------------------------------------------------------------------
sim_index_301 <- unique(data_to_fit_301$ID)

# apply the estimation function on every subject
eta_list_301 <- lapply(sim_index_301,model=mod_run301,dat=data_to_fit_301,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_301 <- do.call(rbind,eta_list_301)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_301 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run301.ofv")

# apply the difference function on every subject
dif_eta_list_301 <- lapply(sim_index_301,eta_poso=eta_table_301,
                           eta_nm=eta_nm_df_301,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_301 <- do.call(rbind,dif_eta_list_301)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_301),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_301.csv")
write_csv(as.data.frame(dif_eta_table_301),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_301.csv")


# ------------------------------------------------------------------------------
# run302 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_302 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit302.csv")

# data management
names(data_to_fit_302) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","DV","BW","SEX","s2_sampling")
data_to_fit_302$DV[data_to_fit_302$DV == "."] <- NA
data_to_fit_302$DV <- as.numeric(data_to_fit_302$DV)

# posologyr --------------------------------------------------------------------
sim_index_302 <- unique(data_to_fit_302$ID)

# apply the estimation function on every subject
eta_list_302 <- lapply(sim_index_302,model=mod_run302,dat=data_to_fit_302,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_302 <- do.call(rbind,eta_list_302)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_302 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run302.ofv")

# apply the difference function on every subject
dif_eta_list_302 <- lapply(sim_index_302,eta_poso=eta_table_302,
                           eta_nm=eta_nm_df_302,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_302 <- do.call(rbind,dif_eta_list_302)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_302),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_302.csv")
write_csv(as.data.frame(dif_eta_table_302),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_302.csv")


# ------------------------------------------------------------------------------
# run401 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_401 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit401.csv")

# data management
names(data_to_fit_401) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_401$DV[data_to_fit_401$DV == "."] <- NA
data_to_fit_401$DV <- as.numeric(data_to_fit_401$DV)

# posologyr --------------------------------------------------------------------
sim_index_401 <- unique(data_to_fit_401$ID)

# apply the estimation function on every subject
eta_list_401 <- lapply(sim_index_401,model=mod_run401,dat=data_to_fit_401,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_401 <- do.call(rbind,eta_list_401)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_401 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run401.ofv")

# apply the difference function on every subject
dif_eta_list_401 <- lapply(sim_index_401,eta_poso=eta_table_401,
                           eta_nm=eta_nm_df_401,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_401 <- do.call(rbind,dif_eta_list_401)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_401),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_401.csv")
write_csv(as.data.frame(dif_eta_table_401),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_401.csv")


# ------------------------------------------------------------------------------
# run402 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_402 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit402.csv")

# data management
names(data_to_fit_402) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","DV","s2_sampling")
data_to_fit_402$DV[data_to_fit_402$DV == "."] <- NA
data_to_fit_402$DV <- as.numeric(data_to_fit_402$DV)

# posologyr --------------------------------------------------------------------
sim_index_402 <- unique(data_to_fit_402$ID)

# apply the estimation function on every subject
eta_list_402 <- lapply(sim_index_402,model=mod_run402,dat=data_to_fit_402,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_402 <- do.call(rbind,eta_list_402)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_402 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run402.ofv")

# apply the difference function on every subject
dif_eta_list_402 <- lapply(sim_index_402,eta_poso=eta_table_402,
                           eta_nm=eta_nm_df_402,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_402 <- do.call(rbind,dif_eta_list_402)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_402),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_402.csv")
write_csv(as.data.frame(dif_eta_table_402),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_402.csv")


# ------------------------------------------------------------------------------
# run403 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_403 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit403.csv")

# data management
names(data_to_fit_403) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_403$DV[data_to_fit_403$DV == "."] <- NA
data_to_fit_403$DV <- as.numeric(data_to_fit_403$DV)

# posologyr --------------------------------------------------------------------
sim_index_403 <- unique(data_to_fit_403$ID)

# apply the estimation function on every subject
eta_list_403 <- lapply(sim_index_403,model=mod_run403,dat=data_to_fit_403,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_403 <- do.call(rbind,eta_list_403)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_403 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run403.ofv")

# apply the difference function on every subject
dif_eta_list_403 <- lapply(sim_index_403,eta_poso=eta_table_403,
                           eta_nm=eta_nm_df_403,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_403 <- do.call(rbind,dif_eta_list_403)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_403),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_403.csv")
write_csv(as.data.frame(dif_eta_table_403),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_403.csv")


# ------------------------------------------------------------------------------
# run404 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_404 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit404.csv")

# data management
names(data_to_fit_404) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","DV","s2_sampling")
data_to_fit_404$DV[data_to_fit_404$DV == "."] <- NA
data_to_fit_404$DV <- as.numeric(data_to_fit_404$DV)

# posologyr --------------------------------------------------------------------
sim_index_404 <- unique(data_to_fit_404$ID)

# apply the estimation function on every subject
eta_list_404 <- lapply(sim_index_404,model=mod_run404,dat=data_to_fit_404,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_404 <- do.call(rbind,eta_list_404)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_404 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run404.ofv")

# apply the difference function on every subject
dif_eta_list_404 <- lapply(sim_index_404,eta_poso=eta_table_404,
                           eta_nm=eta_nm_df_404,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_404 <- do.call(rbind,dif_eta_list_404)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_404),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_404.csv")
write_csv(as.data.frame(dif_eta_table_404),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_404.csv")


# ------------------------------------------------------------------------------
# run405 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_405 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit405.csv")

# data management
names(data_to_fit_405) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_405$DV[data_to_fit_405$DV == "."] <- NA
data_to_fit_405$DV <- as.numeric(data_to_fit_405$DV)

# posologyr --------------------------------------------------------------------
sim_index_405 <- unique(data_to_fit_405$ID)

# apply the estimation function on every subject
eta_list_405 <- lapply(sim_index_405,model=mod_run405,dat=data_to_fit_405,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_405 <- do.call(rbind,eta_list_405)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_405 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run405.ofv")

# apply the difference function on every subject
dif_eta_list_405 <- lapply(sim_index_405,eta_poso=eta_table_405,
                           eta_nm=eta_nm_df_405,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_405 <- do.call(rbind,dif_eta_list_405)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_405),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_405.csv")
write_csv(as.data.frame(dif_eta_table_405),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_405.csv")


# ------------------------------------------------------------------------------
# run406 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_406 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit406.csv")

# data management
names(data_to_fit_406) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","DV","s2_sampling")
data_to_fit_406$DV[data_to_fit_406$DV == "."] <- NA
data_to_fit_406$DV <- as.numeric(data_to_fit_406$DV)

# posologyr --------------------------------------------------------------------
sim_index_406 <- unique(data_to_fit_406$ID)

# apply the estimation function on every subject
eta_list_406 <- lapply(sim_index_406,model=mod_run406,dat=data_to_fit_406,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_406 <- do.call(rbind,eta_list_406)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_406 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run406.ofv")

# apply the difference function on every subject
dif_eta_list_406 <- lapply(sim_index_406,eta_poso=eta_table_406,
                           eta_nm=eta_nm_df_406,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_406 <- do.call(rbind,dif_eta_list_406)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_406),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_406.csv")
write_csv(as.data.frame(dif_eta_table_406),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_406.csv")


# ------------------------------------------------------------------------------
# run407 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_407 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit407.csv")

# data management
names(data_to_fit_407) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","EXPDV","DV","s2_sampling")
data_to_fit_407$DV[data_to_fit_407$DV == "."] <- NA
data_to_fit_407$DV <- as.numeric(data_to_fit_407$DV)

# posologyr --------------------------------------------------------------------
sim_index_407 <- unique(data_to_fit_407$ID)

# apply the estimation function on every subject
eta_list_407 <- lapply(sim_index_407,model=mod_run407,dat=data_to_fit_407,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_407 <- do.call(rbind,eta_list_407)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_407 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run407.ofv")

# apply the difference function on every subject
dif_eta_list_407 <- lapply(sim_index_407,eta_poso=eta_table_407,
                           eta_nm=eta_nm_df_407,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_407 <- do.call(rbind,dif_eta_list_407)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_407),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_407.csv")
write_csv(as.data.frame(dif_eta_table_407),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_407.csv")


# ------------------------------------------------------------------------------
# run408 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_408 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit408.csv")

# data management
names(data_to_fit_408) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL","RATE",
                            "MDV","EXPDV","DV","s2_sampling")
data_to_fit_408$DV[data_to_fit_408$DV == "."] <- NA
data_to_fit_408$DV <- as.numeric(data_to_fit_408$DV)

# posologyr --------------------------------------------------------------------
sim_index_408 <- unique(data_to_fit_408$ID)

# apply the estimation function on every subject
eta_list_408 <- lapply(sim_index_408,model=mod_run408,dat=data_to_fit_408,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_408 <- do.call(rbind,eta_list_408)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_408 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run408.ofv")

# apply the difference function on every subject
dif_eta_list_408 <- lapply(sim_index_408,eta_poso=eta_table_408,
                           eta_nm=eta_nm_df_408,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_408 <- do.call(rbind,dif_eta_list_408)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_408),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_408.csv")
write_csv(as.data.frame(dif_eta_table_408),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_408.csv")


# ------------------------------------------------------------------------------
# run501 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_501 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit501.csv")

# data management
names(data_to_fit_501) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_501$DV[data_to_fit_501$DV == "."] <- NA
data_to_fit_501$DV <- as.numeric(data_to_fit_501$DV)

# posologyr --------------------------------------------------------------------
sim_index_501 <- unique(data_to_fit_501$ID)

# apply the estimation function on every subject
eta_list_501 <- lapply(sim_index_501,model=mod_run501,dat=data_to_fit_501,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_501 <- do.call(rbind,eta_list_501)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_501 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run501.ofv")

# apply the difference function on every subject
dif_eta_list_501 <- lapply(sim_index_501,eta_poso=eta_table_501,
                           eta_nm=eta_nm_df_501,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_501 <- do.call(rbind,dif_eta_list_501)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_501),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_501.csv")
write_csv(as.data.frame(dif_eta_table_501),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_501.csv")


# ------------------------------------------------------------------------------
# run502 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_502 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit502.csv")

# data management
names(data_to_fit_502) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_502$DV[data_to_fit_502$DV == "."] <- NA
data_to_fit_502$DV <- as.numeric(data_to_fit_502$DV)

# posologyr --------------------------------------------------------------------
sim_index_502 <- unique(data_to_fit_502$ID)

# apply the estimation function on every subject
eta_list_502 <- lapply(sim_index_502,model=mod_run502,dat=data_to_fit_502,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_502 <- do.call(rbind,eta_list_502)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_502 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run502.ofv")

# apply the difference function on every subject
dif_eta_list_502 <- lapply(sim_index_502,eta_poso=eta_table_502,
                           eta_nm=eta_nm_df_502,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_502 <- do.call(rbind,dif_eta_list_502)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_502),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_502.csv")
write_csv(as.data.frame(dif_eta_table_502),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_502.csv")


# ------------------------------------------------------------------------------
# run503 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_503 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit503.csv")

# data management
names(data_to_fit_503) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_503$DV[data_to_fit_503$DV == "."] <- NA
data_to_fit_503$DV <- as.numeric(data_to_fit_503$DV)

# posologyr --------------------------------------------------------------------
sim_index_503 <- unique(data_to_fit_503$ID)

# apply the estimation function on every subject
eta_list_503 <- lapply(sim_index_503,model=mod_run503,dat=data_to_fit_503,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_503 <- do.call(rbind,eta_list_503)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_503 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run503.ofv")

# apply the difference function on every subject
dif_eta_list_503 <- lapply(sim_index_503,eta_poso=eta_table_503,
                           eta_nm=eta_nm_df_503,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_503 <- do.call(rbind,dif_eta_list_503)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_503),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_503.csv")
write_csv(as.data.frame(dif_eta_table_503),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_503.csv")


# ------------------------------------------------------------------------------
# run504 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_504 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit504.csv")

# data management
names(data_to_fit_504) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_504$DV[data_to_fit_504$DV == "."] <- NA
data_to_fit_504$DV <- as.numeric(data_to_fit_504$DV)

# posologyr --------------------------------------------------------------------
sim_index_504 <- unique(data_to_fit_504$ID)

# apply the estimation function on every subject
eta_list_504 <- lapply(sim_index_504,model=mod_run504,dat=data_to_fit_504,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_504 <- do.call(rbind,eta_list_504)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_504 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run504.ofv")

# apply the difference function on every subject
dif_eta_list_504 <- lapply(sim_index_504,eta_poso=eta_table_504,
                           eta_nm=eta_nm_df_504,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_504 <- do.call(rbind,dif_eta_list_504)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_504),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_504.csv")
write_csv(as.data.frame(dif_eta_table_504),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_504.csv")


# ------------------------------------------------------------------------------
# run511 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_511 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit511.csv")

# data management
names(data_to_fit_511) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_511$DV[data_to_fit_511$DV == "."] <- NA
data_to_fit_511$DV <- as.numeric(data_to_fit_511$DV)

# posologyr --------------------------------------------------------------------
sim_index_511 <- unique(data_to_fit_511$ID)

# apply the estimation function on every subject
eta_list_511 <- lapply(sim_index_511,model=mod_run511,dat=data_to_fit_511,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_511 <- do.call(rbind,eta_list_511)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_511 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run511.ofv")

# apply the difference function on every subject
dif_eta_list_511 <- lapply(sim_index_511,eta_poso=eta_table_511,
                           eta_nm=eta_nm_df_511,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_511 <- do.call(rbind,dif_eta_list_511)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_511),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_511.csv")
write_csv(as.data.frame(dif_eta_table_511),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_511.csv")


# ------------------------------------------------------------------------------
# run512 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_512 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit512.csv")

# data management
names(data_to_fit_512) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_512$DV[data_to_fit_512$DV == "."] <- NA
data_to_fit_512$DV <- as.numeric(data_to_fit_512$DV)

# posologyr --------------------------------------------------------------------
sim_index_512 <- unique(data_to_fit_512$ID)

# apply the estimation function on every subject
eta_list_512 <- lapply(sim_index_512,model=mod_run512,dat=data_to_fit_512,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_512 <- do.call(rbind,eta_list_512)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_512 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run512.ofv")

# apply the difference function on every subject
dif_eta_list_512 <- lapply(sim_index_512,eta_poso=eta_table_512,
                           eta_nm=eta_nm_df_512,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_512 <- do.call(rbind,dif_eta_list_512)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_512),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_512.csv")
write_csv(as.data.frame(dif_eta_table_512),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_512.csv")


# ------------------------------------------------------------------------------
# run513 -----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Simulated data ---------------------------------------------------------------
# Import data
data_to_fit_513 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit513.csv")

# data management
names(data_to_fit_513) <- c("ID","TIME","EVID","AMT","CMT","II","ADDL",
                            "MDV","DV","s2_sampling")
data_to_fit_513$DV[data_to_fit_513$DV == "."] <- NA
data_to_fit_513$DV <- as.numeric(data_to_fit_513$DV)

# posologyr --------------------------------------------------------------------
sim_index_513 <- unique(data_to_fit_513$ID)

# apply the estimation function on every subject
eta_list_513 <- lapply(sim_index_513,model=mod_run513,dat=data_to_fit_513,
                       FUN=mass_map_estim)

# coerce the list into a matrix
eta_table_513 <- do.call(rbind,eta_list_513)

# Compute the maximum absolute difference of ETA -------------------------------
# load nonmem ETAs
eta_nm_df_513 <- read_csv("~/posologyr-pharmaceutics/posologyr-workflow/run513.ofv")

# apply the difference function on every subject
dif_eta_list_513 <- lapply(sim_index_513,eta_poso=eta_table_513,
                           eta_nm=eta_nm_df_513,
                           FUN=max_abs_dif)

# coerce the list into a matrix
dif_eta_table_513 <- do.call(rbind,dif_eta_list_513)

# Write to file ----------------------------------------------------------------
write_csv(as.data.frame(eta_table_513),
          "~/posologyr-pharmaceutics/posologyr-output/eta_table_513.csv")
write_csv(as.data.frame(dif_eta_table_513),
          "~/posologyr-pharmaceutics/posologyr-output/dif_eta_table_513.csv")
