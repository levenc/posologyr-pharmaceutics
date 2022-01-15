library(readr)
library(data.table)
library(posologyr)

# load everything
eta_001_mlx <- read_csv("~/posologyr-pharmaceutics/monolix-models/run001/IndividualParameters/simulatedRandomEffects.txt")
eta_001_mcmc <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_001_mcmc.csv")
eta_001_sir <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_001_sir.csv")

eta_001_mlx     <- eta_001_mlx[,-1]
eta_001_mlx     <- eta_001_mlx[order(names(eta_001_mlx))]
eta_001_mcmc    <- eta_001_mcmc[order(names(eta_001_mcmc))]
eta_001_sir     <- eta_001_sir[order(names(eta_001_sir))]

names(eta_001_mcmc) <- names(eta_001_mlx)
names(eta_001_sir) <- names(eta_001_mlx)

eta_001_mlx       <- data.frame(run=001,id=eta_001_mlx$id,algo="mlx",eta_001_mlx)
eta_001_mlx       <- eta_001_mlx[,-ncol(eta_001_mlx)]
eta_001_mcmc      <- data.frame(run=001,id=eta_001_mcmc$id,algo="mcmc",eta_001_mcmc)
eta_001_mcmc      <- eta_001_mcmc[,-ncol(eta_001_mcmc)]
eta_001_sir       <- data.frame(run=001,id=eta_001_sir$id,algo="sir",eta_001_sir)
eta_001_sir       <- eta_001_sir[,-ncol(eta_001_sir)]

eta_003_mlx <- read_csv("~/posologyr-pharmaceutics/monolix-models/run003/IndividualParameters/simulatedRandomEffects.txt")
eta_003_mcmc <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_003_mcmc.csv")
eta_003_sir <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_003_sir.csv")

eta_003_mlx     <- eta_003_mlx[,-1]
eta_003_mlx     <- eta_003_mlx[order(names(eta_003_mlx))]
eta_003_mcmc    <- eta_003_mcmc[order(names(eta_003_mcmc))]
eta_003_sir     <- eta_003_sir[order(names(eta_003_sir))]

names(eta_003_mcmc) <- names(eta_003_mlx)
names(eta_003_sir) <- names(eta_003_mlx)

eta_003_mlx       <- data.frame(run=003,id=eta_003_mlx$id,algo="mlx",eta_003_mlx)
eta_003_mlx       <- eta_003_mlx[,-ncol(eta_003_mlx)]
eta_003_mcmc      <- data.frame(run=003,id=eta_003_mcmc$id,algo="mcmc",eta_003_mcmc)
eta_003_mcmc      <- eta_003_mcmc[,-ncol(eta_003_mcmc)]
eta_003_sir       <- data.frame(run=003,id=eta_003_sir$id,algo="sir",eta_003_sir)
eta_003_sir       <- eta_003_sir[,-ncol(eta_003_sir)]

eta_006_mlx <- read_csv("~/posologyr-pharmaceutics/monolix-models/run006/IndividualParameters/simulatedRandomEffects.txt")
eta_006_mcmc <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_006_mcmc.csv")
eta_006_sir <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_006_sir.csv")

eta_006_mlx     <- eta_006_mlx[,-c(1,5)]
eta_006_mlx     <- eta_006_mlx[order(names(eta_006_mlx))]
eta_006_mcmc    <- eta_006_mcmc[order(names(eta_006_mcmc))]
eta_006_sir     <- eta_006_sir[order(names(eta_006_sir))]

names(eta_006_mcmc) <- names(eta_006_mlx)
names(eta_006_sir) <- names(eta_006_mlx)

eta_006_mlx       <- data.frame(run=006,id=eta_006_mlx$id,algo="mlx",eta_006_mlx)
eta_006_mlx       <- eta_006_mlx[,-ncol(eta_006_mlx)]
eta_006_mcmc      <- data.frame(run=006,id=eta_006_mcmc$id,algo="mcmc",eta_006_mcmc)
eta_006_mcmc      <- eta_006_mcmc[,-ncol(eta_006_mcmc)]
eta_006_sir       <- data.frame(run=006,id=eta_006_sir$id,algo="sir",eta_006_sir)
eta_006_sir       <- eta_006_sir[,-ncol(eta_006_sir)]

eta_101_mlx <- read_csv("~/posologyr-pharmaceutics/monolix-models/run101/IndividualParameters/simulatedRandomEffects.txt")
eta_101_mcmc <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_101_mcmc.csv")
eta_101_sir <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_101_sir.csv")

eta_101_mlx     <- eta_101_mlx[,-c(1,6)]
eta_101_mlx     <- eta_101_mlx[order(names(eta_101_mlx))]
eta_101_mcmc    <- eta_101_mcmc[order(names(eta_101_mcmc))]
eta_101_sir     <- eta_101_sir[order(names(eta_101_sir))]

names(eta_101_mcmc) <- names(eta_101_mlx)
names(eta_101_sir) <- names(eta_101_mlx)

eta_101_mlx       <- data.frame(run=101,id=eta_101_mlx$id,algo="mlx",eta_101_mlx)
eta_101_mlx       <- eta_101_mlx[,-ncol(eta_101_mlx)]
eta_101_mcmc      <- data.frame(run=101,id=eta_101_mcmc$id,algo="mcmc",eta_101_mcmc)
eta_101_mcmc      <- eta_101_mcmc[,-ncol(eta_101_mcmc)]
eta_101_sir       <- data.frame(run=101,id=eta_101_sir$id,algo="sir",eta_101_sir)
eta_101_sir       <- eta_101_sir[,-ncol(eta_101_sir)]

eta_201_mlx <- read_csv("~/posologyr-pharmaceutics/monolix-models/run201/IndividualParameters/simulatedRandomEffects.txt")
eta_201_mcmc <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_201_mcmc.csv")
eta_201_sir <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_201_sir.csv")

eta_201_mlx     <- eta_201_mlx[,-1]
eta_201_mlx     <- eta_201_mlx[order(names(eta_201_mlx))]
eta_201_mcmc    <- eta_201_mcmc[order(names(eta_201_mcmc))]
eta_201_sir     <- eta_201_sir[order(names(eta_201_sir))]

names(eta_201_mcmc) <- names(eta_201_mlx)
names(eta_201_sir) <- names(eta_201_mlx)

eta_201_mlx       <- data.frame(run=201,id=eta_201_mlx$id,algo="mlx",eta_201_mlx)
eta_201_mlx       <- eta_201_mlx[,-ncol(eta_201_mlx)]
eta_201_mcmc      <- data.frame(run=201,id=eta_201_mcmc$id,algo="mcmc",eta_201_mcmc)
eta_201_mcmc      <- eta_201_mcmc[,-ncol(eta_201_mcmc)]
eta_201_sir       <- data.frame(run=201,id=eta_201_sir$id,algo="sir",eta_201_sir)
eta_201_sir       <- eta_201_sir[,-ncol(eta_201_sir)]

eta_207_mlx <- read_csv("~/posologyr-pharmaceutics/monolix-models/run207/IndividualParameters/simulatedRandomEffects.txt")
eta_207_mcmc <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_207_mcmc.csv")
eta_207_sir <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_207_sir.csv")

eta_207_mlx     <- eta_207_mlx[,-1]
eta_207_mlx     <- eta_207_mlx[order(names(eta_207_mlx))]
eta_207_mcmc    <- eta_207_mcmc[order(names(eta_207_mcmc))]
eta_207_sir     <- eta_207_sir[order(names(eta_207_sir))]

names(eta_207_mcmc) <- names(eta_207_mlx)
names(eta_207_sir) <- names(eta_207_mlx)

eta_207_mlx       <- data.frame(run=207,id=eta_207_mlx$id,algo="mlx",eta_207_mlx)
eta_207_mlx       <- eta_207_mlx[,-ncol(eta_207_mlx)]
eta_207_mcmc      <- data.frame(run=207,id=eta_207_mcmc$id,algo="mcmc",eta_207_mcmc)
eta_207_mcmc      <- eta_207_mcmc[,-ncol(eta_207_mcmc)]
eta_207_sir       <- data.frame(run=207,id=eta_207_sir$id,algo="sir",eta_207_sir)
eta_207_sir       <- eta_207_sir[,-ncol(eta_207_sir)]

eta_301_mlx <- read_csv("~/posologyr-pharmaceutics/monolix-models/run301/IndividualParameters/simulatedRandomEffects.txt")
eta_301_mcmc <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_301_mcmc.csv")
eta_301_sir <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_301_sir.csv")

eta_301_mlx     <- eta_301_mlx[,-1]
eta_301_mlx     <- eta_301_mlx[order(names(eta_301_mlx))]
eta_301_mcmc    <- eta_301_mcmc[order(names(eta_301_mcmc))]
eta_301_sir     <- eta_301_sir[order(names(eta_301_sir))]

names(eta_301_mcmc) <- names(eta_301_mlx)
names(eta_301_sir) <- names(eta_301_mlx)

eta_301_mlx       <- data.frame(run=301,id=eta_301_mlx$id,algo="mlx",eta_301_mlx)
eta_301_mlx       <- eta_301_mlx[,-ncol(eta_301_mlx)]
eta_301_mcmc      <- data.frame(run=301,id=eta_301_mcmc$id,algo="mcmc",eta_301_mcmc)
eta_301_mcmc      <- eta_301_mcmc[,-ncol(eta_301_mcmc)]
eta_301_sir       <- data.frame(run=301,id=eta_301_sir$id,algo="sir",eta_301_sir)
eta_301_sir       <- eta_301_sir[,-ncol(eta_301_sir)]

eta_405_mlx <- read_csv("~/posologyr-pharmaceutics/monolix-models/run405/IndividualParameters/simulatedRandomEffects.txt")
eta_405_mcmc <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_405_mcmc.csv")
eta_405_sir <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_405_sir.csv")

eta_405_mlx     <- eta_405_mlx[,-1]
eta_405_mlx     <- eta_405_mlx[order(names(eta_405_mlx))]
eta_405_mcmc    <- eta_405_mcmc[order(names(eta_405_mcmc))]
eta_405_sir     <- eta_405_sir[order(names(eta_405_sir))]

names(eta_405_mcmc) <- names(eta_405_mlx)
names(eta_405_sir) <- names(eta_405_mlx)

eta_405_mlx       <- data.frame(run=405,id=eta_405_mlx$id,algo="mlx",eta_405_mlx)
eta_405_mlx       <- eta_405_mlx[,-ncol(eta_405_mlx)]
eta_405_mcmc      <- data.frame(run=405,id=eta_405_mcmc$id,algo="mcmc",eta_405_mcmc)
eta_405_mcmc      <- eta_405_mcmc[,-ncol(eta_405_mcmc)]
eta_405_sir       <- data.frame(run=405,id=eta_405_sir$id,algo="sir",eta_405_sir)
eta_405_sir       <- eta_405_sir[,-ncol(eta_405_sir)]

eta_504_mlx <- read_csv("~/posologyr-pharmaceutics/monolix-models/run504/IndividualParameters/simulatedRandomEffects.txt")
eta_504_mcmc <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_504_mcmc.csv")
eta_504_sir <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_504_sir.csv")

eta_504_mlx     <- eta_504_mlx[,-1]
eta_504_mlx     <- eta_504_mlx[order(names(eta_504_mlx))]
eta_504_mcmc    <- eta_504_mcmc[order(names(eta_504_mcmc))]
eta_504_sir     <- eta_504_sir[order(names(eta_504_sir))]

names(eta_504_mcmc) <- names(eta_504_mlx)
names(eta_504_sir) <- names(eta_504_mlx)

eta_504_mlx       <- data.frame(run=504,id=eta_504_mlx$id,algo="mlx",eta_504_mlx)
eta_504_mlx       <- eta_504_mlx[,-ncol(eta_504_mlx)]
eta_504_mcmc      <- data.frame(run=504,id=eta_504_mcmc$id,algo="mcmc",eta_504_mcmc)
eta_504_mcmc      <- eta_504_mcmc[,-ncol(eta_504_mcmc)]
eta_504_sir       <- data.frame(run=504,id=eta_504_sir$id,algo="sir",eta_504_sir)
eta_504_sir       <- eta_504_sir[,-ncol(eta_504_sir)]

eta_513_mlx <- read_csv("~/posologyr-pharmaceutics/monolix-models/run513/IndividualParameters/simulatedRandomEffects.txt")
eta_513_mcmc <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_513_mcmc.csv")
eta_513_sir <- read_csv("~/posologyr-pharmaceutics/posologyr-output-dist/eta_513_sir.csv")

eta_513_mlx     <- eta_513_mlx[,-1]
eta_513_mlx     <- eta_513_mlx[order(names(eta_513_mlx))]
eta_513_mcmc    <- eta_513_mcmc[order(names(eta_513_mcmc))]
eta_513_sir     <- eta_513_sir[order(names(eta_513_sir))]

names(eta_513_mcmc) <- names(eta_513_mlx)
names(eta_513_sir) <- names(eta_513_mlx)

eta_513_mlx       <- data.frame(run=513,id=eta_513_mlx$id,algo="mlx",eta_513_mlx)
eta_513_mlx       <- eta_513_mlx[,-ncol(eta_513_mlx)]
eta_513_mcmc      <- data.frame(run=513,id=eta_513_mcmc$id,algo="mcmc",eta_513_mcmc)
eta_513_mcmc      <- eta_513_mcmc[,-ncol(eta_513_mcmc)]
eta_513_sir       <- data.frame(run=513,id=eta_513_sir$id,algo="sir",eta_513_sir)
eta_513_sir       <- eta_513_sir[,-ncol(eta_513_sir)]

# bind everything
eta_001_dist <- data.table(rbind(eta_001_mlx,
                      eta_001_mcmc,
                      eta_001_sir))

eta_003_dist <- data.table(rbind(eta_003_mlx,
                      eta_003_mcmc,
                      eta_003_sir))

eta_006_dist <- data.table(rbind(eta_006_mlx,
                      eta_006_mcmc,
                      eta_006_sir))

eta_101_dist <- data.table(rbind(eta_101_mlx,
                      eta_101_mcmc,
                      eta_101_sir))

eta_201_dist <- data.table(rbind(eta_201_mlx,
                      eta_201_mcmc,
                      eta_201_sir))

eta_207_dist <- data.table(rbind(eta_207_mlx,
                      eta_207_mcmc,
                      eta_207_sir))

eta_301_dist <- data.table(rbind(eta_301_mlx,
                      eta_301_mcmc,
                      eta_301_sir))

eta_405_dist <- data.table(rbind(eta_405_mlx,
                      eta_405_mcmc,
                      eta_405_sir))

eta_504_dist <- data.table(rbind(eta_504_mlx,
                      eta_504_mcmc,
                      eta_504_sir))

eta_513_dist <- data.table(rbind(eta_513_mlx,
                      eta_513_mcmc,
                      eta_513_sir))

#-------------------------------------------------------------------------------
# Probabilistic MIPD evaluation
#-------------------------------------------------------------------------------

# Import data -----------------------------------------------------------------
data_to_fit001 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit001.csv")
data_to_fit003 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit003.csv")
data_to_fit006 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit006.csv")
data_to_fit101 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit101.csv")
data_to_fit201 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit201.csv")
data_to_fit207 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit207.csv")
data_to_fit301 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit301.csv")
data_to_fit405 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit405.csv")
data_to_fit504 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit504.csv")
data_to_fit513 <- read_csv("~/posologyr-pharmaceutics/mapbayr-CPTPSP-2021-main/data/data_to_fit513.csv")

# data management --------------------------------------------------------------
sim_index <- 1:20

# run001
names(data_to_fit001) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit001$DV[data_to_fit001$DV == "."] <- NA
data_to_fit001$DV <- as.numeric(data_to_fit001$DV)
data_to_fit001 <- data_to_fit001[data_to_fit001$ID %in% (sim_index),]

# run003
names(data_to_fit003) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit003$DV[data_to_fit003$DV == "."] <- NA
data_to_fit003$DV <- as.numeric(data_to_fit003$DV)
data_to_fit003 <- data_to_fit003[data_to_fit003$ID %in% (sim_index),]

# run006
names(data_to_fit006) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","RATE","MDV","DV","s2_sampling")
data_to_fit006$DV[data_to_fit006$DV == "."] <- NA
data_to_fit006$DV <- as.numeric(data_to_fit006$DV)
data_to_fit006 <- data_to_fit006[data_to_fit006$ID %in% (sim_index),]

# run101
names(data_to_fit101) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit101$DV[data_to_fit101$DV == "."] <- NA
data_to_fit101$DV <- as.numeric(data_to_fit101$DV)
data_to_fit101 <- data_to_fit101[data_to_fit101$ID %in% (sim_index),]

# run201
names(data_to_fit201) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit201$DV[data_to_fit201$DV == "."] <- NA
data_to_fit201$DV <- as.numeric(data_to_fit201$DV)
data_to_fit201 <- data_to_fit201[data_to_fit201$ID %in% (sim_index),]

# run207
names(data_to_fit207) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit207$DV[data_to_fit207$DV == "."] <- NA
data_to_fit207$DV <- as.numeric(data_to_fit207$DV)
data_to_fit207 <- data_to_fit207[data_to_fit207$ID %in% (sim_index),]

# run301
names(data_to_fit301) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","BW","SEX","s2_sampling")
data_to_fit301$DV[data_to_fit301$DV == "."] <- NA
data_to_fit301$DV <- as.numeric(data_to_fit301$DV)
data_to_fit301 <- data_to_fit301[data_to_fit301$ID %in% (sim_index),]

# run405
names(data_to_fit405) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit405$DV[data_to_fit405$DV == "."] <- NA
data_to_fit405$DV <- as.numeric(data_to_fit405$DV)
data_to_fit405 <- data_to_fit405[data_to_fit405$ID %in% (sim_index),]

# run504
names(data_to_fit504) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit504$DV[data_to_fit504$DV == "."] <- NA
data_to_fit504$DV <- as.numeric(data_to_fit504$DV)
data_to_fit504 <- data_to_fit504[data_to_fit504$ID %in% (sim_index),]

# run513
names(data_to_fit513) <- c("ID","TIME","EVID","AMT","CMT",
                           "II","ADDL","MDV","DV","s2_sampling")
data_to_fit513$DV[data_to_fit513$DV == "."] <- NA
data_to_fit513$DV <- as.numeric(data_to_fit513$DV)
data_to_fit513 <- data_to_fit513[data_to_fit513$ID %in% (sim_index),]

# optimal dose for a target concentration --------------------------------------

mass_dose_conc <- function(index,model,dat,param){
  set.seed(42)
  print(index)
  mlx <- poso_dose_conc(posologyr(model,dat[dat$ID == index,]),
                        time_c = 3,
                        target_conc = 30,
                        p = .89,
                        indiv_param = as.data.frame(param[algo == "mlx" &
                                                            id == index,-(1:3)]))
  mcmc <- poso_dose_conc(posologyr(model,dat[dat$ID == index,]),
                        time_c = 3,
                        target_conc = 30,
                        p = .89,
                        indiv_param = as.data.frame(param[algo == "mcmc" &
                                                            id == index,-(1:3)]))
  sir <- poso_dose_conc(posologyr(model,dat[dat$ID == index,]),
                        time_c = 3,
                        target_conc = 30,
                        p = .89,
                        indiv_param = as.data.frame(param[algo == "sir" &
                                                            id == index,-(1:3)]))
  dose <- data.frame(index,mlx$dose,mcmc$dose,sir$dose)
  return(dose)
}

# perform the dose selection for every subject, and write the results
#run001
dose_conc_001 <- lapply(sim_index,model=mod_dist_run001,dat=data_to_fit001,
                        param=data.table(eta_001_dist,
                                         THETA_Cl=4,
                                         THETA_Ka=1,
                                         THETA_Vc=70),
                        FUN=mass_dose_conc)

dose_conc_001_dt <- data.table(run=001,do.call(rbind,dose_conc_001))

#run003
dose_conc_003 <- lapply(sim_index,model=mod_dist_run003,dat=data_to_fit003,
                        param=data.table(eta_003_dist,
                                         THETA_Cl=4.0,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0,
                                         THETA_ALAG=1.0),
                        FUN=mass_dose_conc)

dose_conc_003_dt <- data.table(run=003,do.call(rbind,dose_conc_003))

#run006
dose_conc_006 <- lapply(sim_index,model=mod_dist_run006,dat=data_to_fit006,
                        param=data.table(eta_006_dist,
                                         THETA_Cl=4.0,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0,
                                         THETA_D2=4.0),
                        FUN=mass_dose_conc)

dose_conc_006_dt <- data.table(run=006,do.call(rbind,dose_conc_006))

#run101
dose_conc_101 <- lapply(sim_index,model=mod_dist_run101,dat=data_to_fit101,
                        param=data.table(eta_101_dist,
                                         THETA_Cl=4.0,
                                         THETA_Ka=1.0,
                                         THETA_Vc=70.0,
                                         THETA_Vp=50.0,
                                         Q=4),
                        FUN=mass_dose_conc)

dose_conc_101_dt <- data.table(run=101,do.call(rbind,dose_conc_101))

#run201
dose_conc_201 <- lapply(sim_index,model=mod_dist_run201,dat=data_to_fit201,
                        param=data.table(eta_201_dist,
                                         THETA_Vmax=10000,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0,
                                         THETA_Km=2500),
                        FUN=mass_dose_conc)

dose_conc_201_dt <- data.table(run=201,do.call(rbind,dose_conc_201))

#run207
dose_conc_207 <- lapply(sim_index,model=mod_dist_run207,dat=data_to_fit207,
                        param=data.table(eta_207_dist,
                                         THETA_Vmax=10000,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0,
                                         THETA_Km=2500,
                                         THETA_Cl = 4.0),
                        FUN=mass_dose_conc)

dose_conc_207_dt <- data.table(run=207,do.call(rbind,dose_conc_207))

#run301
dose_conc_301 <- lapply(sim_index,model=mod_dist_run301,dat=data_to_fit301,
                        param=data.table(eta_301_dist,
                                         THETA_Cl=4.0,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0,
                                         BW=75.0,
                                         SEX=1),
                        FUN=mass_dose_conc)

dose_conc_301_dt <- data.table(run=301,do.call(rbind,dose_conc_301))

#run405
dose_conc_405 <- lapply(sim_index,model=mod_dist_run405,dat=data_to_fit405,
                        param=data.table(eta_405_dist,
                                         THETA_Cl=4.0,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0),
                        FUN=mass_dose_conc)

dose_conc_405_dt <- data.table(run=405,do.call(rbind,dose_conc_405))

#run504
dose_conc_504 <- lapply(sim_index,model=mod_dist_run504,dat=data_to_fit504,
                        param=data.table(eta_504_dist,
                                         THETA_Cl=4.0,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0),
                        FUN=mass_dose_conc)

dose_conc_504_dt <- data.table(run=504,do.call(rbind,dose_conc_504))

#run513
dose_conc_513 <- lapply(sim_index,model=mod_dist_run513,dat=data_to_fit513,
                        param=data.table(eta_513_dist,
                                         THETA_Cl=4.0,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0),
                        FUN=mass_dose_conc)

dose_conc_513_dt <- data.table(run=513,do.call(rbind,dose_conc_513))

#merge all dose tables
all_dose_conc <- rbind(dose_conc_001_dt,
                       dose_conc_003_dt,
                       dose_conc_006_dt,
                       dose_conc_101_dt,
                       dose_conc_201_dt,
                       dose_conc_207_dt,
                       dose_conc_301_dt,
                       dose_conc_405_dt,
                       dose_conc_504_dt,
                       dose_conc_513_dt)

#compute bias
all_dose_conc[,mlx_sir := ((sir.dose-mlx.dose)/mlx.dose)]
all_dose_conc[,mlx_mcmc := ((mcmc.dose-mlx.dose)/mlx.dose)]

# write it down
write_csv(all_dose_conc,
          "~/posologyr-pharmaceutics/posologyr-output-dist/all_dose_conc.csv")

# optimal dose for a target auc --------------------------------------

mass_dose_auc <- function(index,model,dat,param){
  set.seed(42)
  print(index)
  mlx <- poso_dose_auc(posologyr(model,dat[dat$ID == index,]),
                        time_auc = 12,
                        target_auc = 500,
                        p = .89,
                        indiv_param = as.data.frame(param[algo == "mlx" &
                                                            id == index,-(1:3)]))
  mcmc <- poso_dose_auc(posologyr(model,dat[dat$ID == index,]),
                         time_auc = 12,
                         target_auc = 500,
                         p = .89,
                         indiv_param = as.data.frame(param[algo == "mcmc" &
                                                             id == index,-(1:3)]))
  sir <- poso_dose_auc(posologyr(model,dat[dat$ID == index,]),
                        time_auc = 12,
                        target_auc = 500,
                        p = .89,
                        indiv_param = as.data.frame(param[algo == "sir" &
                                                            id == index,-(1:3)]))
  dose <- data.frame(index,mlx$dose,mcmc$dose,sir$dose)
  return(dose)
}

# perform the dose selection for every subject, and write the results
#run001
dose_auc_001 <- lapply(sim_index,model=mod_dist_run001,dat=data_to_fit001,
                       param=data.table(eta_001_dist,
                                        THETA_Cl=4,
                                        THETA_Ka=1,
                                        THETA_Vc=70),
                       FUN=mass_dose_auc)

dose_auc_001_dt <- data.table(run=001,do.call(rbind,dose_auc_001))

#run003
dose_auc_003 <- lapply(sim_index,model=mod_dist_run003,dat=data_to_fit003,
                       param=data.table(eta_003_dist,
                                        THETA_Cl=4.0,
                                        THETA_Vc=70.0,
                                        THETA_Ka=1.0,
                                        THETA_ALAG=1.0),
                       FUN=mass_dose_auc)

dose_auc_003_dt <- data.table(run=003,do.call(rbind,dose_auc_003))

#run006
dose_auc_006 <- lapply(sim_index,model=mod_dist_run006,dat=data_to_fit006,
                       param=data.table(eta_006_dist,
                                        THETA_Cl=4.0,
                                        THETA_Vc=70.0,
                                        THETA_Ka=1.0,
                                        THETA_D2=4.0),
                       FUN=mass_dose_auc)

dose_auc_006_dt <- data.table(run=006,do.call(rbind,dose_auc_006))

#run101
dose_auc_101 <- lapply(sim_index,model=mod_dist_run101,dat=data_to_fit101,
                       param=data.table(eta_101_dist,
                                        THETA_Cl=4.0,
                                        THETA_Vc=70.0,
                                        THETA_Ka=1.0,
                                        THETA_Vp=50.0,
                                        Q=4),
                       FUN=mass_dose_auc)

dose_auc_101_dt <- data.table(run=101,do.call(rbind,dose_auc_101))

#run201
dose_auc_201 <- lapply(sim_index,model=mod_dist_run201,dat=data_to_fit201,
                       param=data.table(eta_201_dist,
                                        THETA_Vmax=10000,
                                        THETA_Vc=70.0,
                                        THETA_Ka=1.0,
                                        THETA_Km=2500),
                       FUN=mass_dose_auc)

dose_auc_201_dt <- data.table(run=201,do.call(rbind,dose_auc_201))

#run207
dose_auc_207 <- lapply(sim_index,model=mod_dist_run207,dat=data_to_fit207,
                       param=data.table(eta_207_dist,
                                        THETA_Vmax=10000,
                                        THETA_Vc=70.0,
                                        THETA_Ka=1.0,
                                        THETA_Km=2500,
                                        THETA_Cl = 4.0),
                       FUN=mass_dose_auc)

dose_auc_207_dt <- data.table(run=207,do.call(rbind,dose_auc_207))

#run301
dose_auc_301 <- lapply(sim_index,model=mod_dist_run301,dat=data_to_fit301,
                       param=data.table(eta_301_dist,
                                        THETA_Cl=4.0,
                                        THETA_Vc=70.0,
                                        THETA_Ka=1.0,
                                        BW=75.0,
                                        SEX=1),
                       FUN=mass_dose_auc)

dose_auc_301_dt <- data.table(run=301,do.call(rbind,dose_auc_301))

#run405
dose_auc_405 <- lapply(sim_index,model=mod_dist_run405,dat=data_to_fit405,
                       param=data.table(eta_405_dist,
                                        THETA_Cl=4.0,
                                        THETA_Vc=70.0,
                                        THETA_Ka=1.0),
                       FUN=mass_dose_auc)

dose_auc_405_dt <- data.table(run=405,do.call(rbind,dose_auc_405))

#run504
dose_auc_504 <- lapply(sim_index,model=mod_dist_run504,dat=data_to_fit504,
                       param=data.table(eta_504_dist,
                                        THETA_Cl=4.0,
                                        THETA_Vc=70.0,
                                        THETA_Ka=1.0),
                       FUN=mass_dose_auc)

dose_auc_504_dt <- data.table(run=504,do.call(rbind,dose_auc_504))

#run513
dose_auc_513 <- lapply(sim_index,model=mod_dist_run513,dat=data_to_fit513,
                       param=data.table(eta_513_dist,
                                        THETA_Cl=4.0,
                                        THETA_Vc=70.0,
                                        THETA_Ka=1.0),
                       FUN=mass_dose_auc)

dose_auc_513_dt <- data.table(run=513,do.call(rbind,dose_auc_513))

#merge all dose tables
all_dose_auc <- rbind(dose_auc_001_dt,
                      dose_auc_003_dt,
                      dose_auc_006_dt,
                      dose_auc_101_dt,
                      dose_auc_201_dt,
                      dose_auc_207_dt,
                      dose_auc_301_dt,
                      dose_auc_405_dt,
                      dose_auc_504_dt,
                      dose_auc_513_dt)

#compute bias
all_dose_auc[,mlx_sir := ((sir.dose-mlx.dose)/mlx.dose)]
all_dose_auc[,mlx_mcmc := ((mcmc.dose-mlx.dose)/mlx.dose)]

# write it down
write_csv(all_dose_auc,
          "~/posologyr-pharmaceutics/posologyr-output-dist/all_dose_auc.csv")

# time to a selected cmin --------------------------------------

mass_time_cmin <- function(index,model,dat,param){
  set.seed(42)
  print(index)
  mlx <- poso_time_cmin(posologyr(model,dat[dat$ID == index,]),
                       dose = 100,
                       target_cmin = .5,
                       greater_than = FALSE,
                       from = 3,
                       last_time = 400,
                       p = .89,
                       indiv_param = as.data.frame(param[algo == "mlx" &
                                                           id == index,-(1:3)]))
  mcmc <- poso_time_cmin(posologyr(model,dat[dat$ID == index,]),
                        dose = 100,
                        target_cmin = .5,
                        greater_than = FALSE,
                        from = 3,
                        last_time = 400,
                        p = .89,
                        indiv_param = as.data.frame(param[algo == "mcmc" &
                                                            id == index,-(1:3)]))
  sir <- poso_time_cmin(posologyr(model,dat[dat$ID == index,]),
                       dose = 100,
                       target_cmin = .5,
                       greater_than = FALSE,
                       from = 3,
                       last_time = 400,
                       p = .89,
                       indiv_param = as.data.frame(param[algo == "sir" &
                                                           id == index,-(1:3)]))
  time_estim <- data.frame(index,mlx$time,mcmc$time,sir$time)
  return(time_estim)
}

# perform the dose selection for every subject, and write the results
#run001
time_cmin_001 <- lapply(sim_index,model=mod_dist_run001,dat=data_to_fit001,
                        param=data.table(eta_001_dist,
                                         THETA_Cl=4,
                                         THETA_Ka=1,
                                         THETA_Vc=70),
                        FUN=mass_time_cmin)

time_cmin_001_dt <- data.table(run=001,do.call(rbind,time_cmin_001))

#run003
time_cmin_003 <- lapply(sim_index,model=mod_dist_run003,dat=data_to_fit003,
                        param=data.table(eta_003_dist,
                                         THETA_Cl=4.0,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0,
                                         THETA_ALAG=1.0),
                        FUN=mass_time_cmin)

time_cmin_003_dt <- data.table(run=003,do.call(rbind,time_cmin_003))

#run006
time_cmin_006 <- lapply(sim_index,model=mod_dist_run006,dat=data_to_fit006,
                        param=data.table(eta_006_dist,
                                         THETA_Cl=4.0,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0,
                                         THETA_D2=4.0),
                        FUN=mass_time_cmin)

time_cmin_006_dt <- data.table(run=006,do.call(rbind,time_cmin_006))

#run101
time_cmin_101 <- lapply(sim_index,model=mod_dist_run101,dat=data_to_fit101,
                        param=data.table(eta_101_dist,
                                         THETA_Cl=4.0,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0,
                                         THETA_Vp=50.0,
                                         Q=4),
                        FUN=mass_time_cmin)

time_cmin_101_dt <- data.table(run=101,do.call(rbind,time_cmin_101))

#run201
time_cmin_201 <- lapply(sim_index,model=mod_dist_run201,dat=data_to_fit201,
                        param=data.table(eta_201_dist,
                                         THETA_Vmax=10000,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0,
                                         THETA_Km=2500),
                        FUN=mass_time_cmin)

time_cmin_201_dt <- data.table(run=201,do.call(rbind,time_cmin_201))

#run207
time_cmin_207 <- lapply(sim_index,model=mod_dist_run207,dat=data_to_fit207,
                        param=data.table(eta_207_dist,
                                         THETA_Vmax=10000,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0,
                                         THETA_Km=2500,
                                         THETA_Cl = 4.0),
                        FUN=mass_time_cmin)

time_cmin_207_dt <- data.table(run=207,do.call(rbind,time_cmin_207))

#run301
time_cmin_301 <- lapply(sim_index,model=mod_dist_run301,dat=data_to_fit301,
                        param=data.table(eta_301_dist,
                                         THETA_Cl=4.0,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0,
                                         BW=75.0,
                                         SEX=1),
                        FUN=mass_time_cmin)

time_cmin_301_dt <- data.table(run=301,do.call(rbind,time_cmin_301))

#run405
time_cmin_405 <- lapply(sim_index,model=mod_dist_run405,dat=data_to_fit405,
                        param=data.table(eta_405_dist,
                                         THETA_Cl=4.0,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0),
                        FUN=mass_time_cmin)

time_cmin_405_dt <- data.table(run=405,do.call(rbind,time_cmin_405))

#run504
time_cmin_504 <- lapply(sim_index,model=mod_dist_run504,dat=data_to_fit504,
                        param=data.table(eta_504_dist,
                                         THETA_Cl=4.0,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0),
                        FUN=mass_time_cmin)

time_cmin_504_dt <- data.table(run=504,do.call(rbind,time_cmin_504))

#run513
time_cmin_513 <- lapply(sim_index,model=mod_dist_run513,dat=data_to_fit513,
                        param=data.table(eta_513_dist,
                                         THETA_Cl=4.0,
                                         THETA_Vc=70.0,
                                         THETA_Ka=1.0),
                        FUN=mass_time_cmin)

time_cmin_513_dt <- data.table(run=513,do.call(rbind,time_cmin_513))

#merge all tables
all_time_cmin <- rbind(time_cmin_001_dt,
                       time_cmin_003_dt,
                       time_cmin_006_dt,
                       time_cmin_101_dt,
                       time_cmin_201_dt,
                       time_cmin_207_dt,
                       time_cmin_301_dt,
                       time_cmin_405_dt,
                       time_cmin_504_dt,
                       time_cmin_513_dt)

#compute bias
all_time_cmin[,mlx_sir := ((sir.time-mlx.time)/mlx.time)]
all_time_cmin[,mlx_mcmc := ((mcmc.time-mlx.time)/mlx.time)]

# write it down
write_csv(all_time_cmin,
          "~/posologyr-pharmaceutics/posologyr-output-dist/all_time_cmin.csv")

#-------------------------------------------------------------------------------
# dose AUC
# plot something ---------------------------------------------------------------
dt_dif_plot <- data.table::data.table("run"=sort(rep(unique(all_dose_auc$run),2)),
                                      "level"=c(1,2),
                                      "prop"=0)

#discordant
dt_dif_plot <- merge(dt_dif_plot,all_dose_auc[abs(mlx_sir)>.1][,.N,by=run],
                     by="run",all.x=TRUE)
#acceptable
dt_dif_plot <- merge(dt_dif_plot,all_dose_auc[abs(mlx_sir)<=.1][,.N,by=run],
                     by="run",all.x=TRUE)

#set props
dt_dif_plot[level==1,prop:=(N.y*5)] #acceptable
dt_dif_plot[level==2,prop:=(N.x*5)] #discordant
dt_dif_plot[is.na(prop)]$prop <- 0  #no discordant estimate, prop == 0
dt_dif_plot[,run:=as.factor(run)]
dt_dif_plot[,prop:=as.numeric(prop)]
dt_dif_plot[,level:=as.factor(level)]

#sort by less discordances
setorder(dt_dif_plot,cols="N.x")
dt_dif_plot[,sort_by_perf:=sort(rep(1:10,2))]
dt_dif_plot[,sort_by_perf:=as.factor(sort_by_perf)]
levels(dt_dif_plot$sort_by_perf) <- unique(dt_dif_plot$run)

#splt data.table
dt1 <- dt_dif_plot[level!=2] # not discordant
dt2 <- dt_dif_plot[level==2] # discordant

#Plot
library(ggplot2)

ggplot() +
  geom_bar(data=dt2,
           aes(x = run, y=-prop, fill = level,
           ),
           position="stack", stat="identity") +
  geom_bar(data=dt1,
           aes(x = run, y=prop, fill = level,
           ),
           position="stack", stat="identity") +
  ylim(-20,100) +
  #scale_x_discrete(limits = rev(levels(dt1$run))) +
  scale_x_discrete(limits = rev(levels(dt2$sort_by_perf))) +
  geom_hline(yintercept = 0, color =c("black"))+
  theme_light() +
  coord_flip() +
  #guides(fill=guide_legend(title="",reverse=FALSE)) +
  scale_fill_brewer(palette="Paired", name="",labels=c("Acceptable",
                                                       "Discordant"),
                    direction=-1) +
  labs(y="Percentages of acceptable dose proposition",x="")

#-------------------------------------------------------------------------------
# dose conc
# plot something ---------------------------------------------------------------
dt_dif_plot <- data.table::data.table("run"=sort(rep(unique(all_dose_conc$run),2)),
                                      "level"=c(1,2),
                                      "prop"=0)

#discordant
dt_dif_plot <- merge(dt_dif_plot,all_dose_conc[abs(mlx_sir)>.1][,.N,by=run],
                     by="run",all.x=TRUE)
#acceptable
dt_dif_plot <- merge(dt_dif_plot,all_dose_conc[abs(mlx_sir)<=.1][,.N,by=run],
                     by="run",all.x=TRUE)

#set props
dt_dif_plot[level==1,prop:=(N.y*5)] #acceptable
dt_dif_plot[level==2,prop:=(N.x*5)] #discordant
dt_dif_plot[is.na(prop)]$prop <- 0     #no discordant estimate, prop == 0
dt_dif_plot[,run:=as.factor(run)]
dt_dif_plot[,prop:=as.numeric(prop)]
dt_dif_plot[,level:=as.factor(level)]

#sort by less discordances
setorder(dt_dif_plot,cols="N.x")
dt_dif_plot[,sort_by_perf:=sort(rep(1:10,2))]
dt_dif_plot[,sort_by_perf:=as.factor(sort_by_perf)]
levels(dt_dif_plot$sort_by_perf) <- unique(dt_dif_plot$run)

#splt data.table
dt1 <- dt_dif_plot[level!=2] # not discordant
dt2 <- dt_dif_plot[level==2] # discordant

#Plot
library(ggplot2)

ggplot() +
  geom_bar(data=dt2,
           aes(x = run, y=-prop, fill = level,
           ),
           position="stack", stat="identity") +
  geom_bar(data=dt1,
           aes(x = run, y=prop, fill = level,
           ),
           position="stack", stat="identity") +
  ylim(-20,100) +
  #scale_x_discrete(limits = rev(levels(dt1$run))) +
  scale_x_discrete(limits = rev(levels(dt2$sort_by_perf))) +
  geom_hline(yintercept = 0, color =c("black"))+
  theme_light() +
  coord_flip() +
  #guides(fill=guide_legend(title="",reverse=FALSE)) +
  scale_fill_brewer(palette="Paired", name="",labels=c("Acceptable",
                                                       "Discordant"),
                    direction=-1) +
  labs(y="Percentages of acceptable dose proposition",x="")

#------------------------------------------------------------------------------
# Density plots
#

# Multiple plot function
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# ggplot objects can be passed in ~., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(~., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ~. arguments and plotlist
  plots <- c(list(~.), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


eta_003_dist[,algorithm:=algo]

Cl_model003 <- ggplot(eta_003_dist[id==1],
                    aes(x=eta_Cl,colour=algorithm)) +
  geom_density() +
  theme_bw() +
  ggtitle("Posterior distribution of eta_Cl")

Ka_model003 <- ggplot(eta_003_dist[id==1],
                    aes(x=eta_Ka,colour=algorithm)) +
  geom_density() +
  theme_bw() +
  ggtitle("Posterior distribution of eta_Ka")

Vc_model003 <- ggplot(eta_003_dist[id==1],
                    aes(x=eta_Vc,colour=algorithm)) +
  geom_density() +
  theme_bw() +
  ggtitle("Posterior distribution of eta_Vc")

ALAG_model003 <- ggplot(eta_003_dist[id==1],
                      aes(x=eta_ALAG,colour=algorithm)) +
  geom_density() +
  theme_bw() +
  ggtitle("Posterior distribution of eta_ALAG")

multiplot(Cl_model003,Ka_model003,Vc_model003,ALAG_model003,cols=2)

