library(tidyverse)

sens_hosps<-function(statuses, pop_size){
  ids_hosp <- rep(NA, pop_size)
  for(i in 1:length(statuses)){for(j in 1:pop_size){if(statuses[[i]]$I3[j] == 1){ids_hosp[j] <- j}}}
  out <- unique(ids_hosp)
  return(out)
}

sens_inf<-function(statuses, pop_size){
  ids_inf <- rep(NA, pop_size)
  for(i in 1:length(statuses)){for(j in 1:pop_size){if(statuses[[i]]$I2[j] == 1){ids_inf[j] <- j}}}
  out <- unique(ids_inf)
  return(out)
}

sens_fatig<-function(concern_ids, pop_size, days){
  conc <- rep(NA, pop_size)
  for(j in 1:pop_size){run <- rle(unlist(lapply(concern_ids, `[[`, j))); if(sum(run$values) > 1){if(max(run$lengths[run$values == 1]) > days){conc[j] <- j}}}
  out <- unique(conc)
  return(out)
}
df <- list.files(path=("C:/Data/Sensitization/output/results4/COV_Starters"), pattern = ".RDS")
ParOut <- read.csv("C:/Data/Sensitization/output/results4/COV_Starters/COV_mod.csv")

ParOut$rep <- rep(3, nrow(ParOut))
ParOut <- as.data.frame(lapply(ParOut, rep, ParOut$rep))
ParOut$rep <- rep(c(1:3), 2)
df2 <- paste0("C:/Data/Sensitization/output/results4/COV_Starters/", df)
ParOut$id <- df2

#Create empty variables
peak<-numeric()
whenpeak<-numeric()
exp<-numeric()
hosp<-numeric()

#Create empty matrices
peak_comm<-matrix(nr=10,nc=(length(df)))
whenpeak_comm<-matrix(nr=10,nc=(length(df)))
exp_comm<-matrix(nr=10,nc=(length(df)))
hosp_comm<-matrix(nr=10,nc=(length(df)))
minconc_com<-matrix(nr=10,nc=(length(df)))
whenmico_com<-matrix(nr=10,nc=(length(df)))
maxconc_com<-matrix(nr=10,nc=(length(df)))
whenmaco_com<-matrix(nr=10,nc=(length(df)))

#lists
rm(sens_H)
rm(sens_F90)
rm(sens_I)
rm(sens_F60)
sens_H <- list()
sens_F90 <- list()
sens_I <- list()
sens_F60 <- list()
sens_F120 <- list()

getData <- function(id,resfold=""){
  if(resfold=="")
    return(readRDS(paste0(id)))
  else
    return(readRDS(paste0(file.path(resfold,id))))
}


for(i in 1:length(ParOut$id)){
  test <- getData(ParOut$id[i])
  
  peak[i]<-max(colSums(test[[3]]))
  whenpeak[i]<-which.max(colSums(test[[3]]))
  exp[i]<-max(colSums(test[[2]]))
  hosp[i]<-max(colSums(test[[4]]))

  peak_comm[,i]<-apply(test[[3]],1,max)
  whenpeak_comm[,i]<-apply(test[[3]],1,which.max)
  exp_comm[,i]<-apply(test[[2]],1,max)
  hosp_comm[,i]<-apply(test[[4]],1,max)
  minconc_com[,i]<-apply(test[[1]],1,min)
  whenmico_com[,i]<-apply(test[[1]],1,which.min)
  maxconc_com[,i]<-apply(test[[1]],1,max)
  whenmaco_com[,i]<-apply(test[[1]],1,which.max)
  
  sens_F60[[i]] <- sens_fatig(test$concern_ids, 2000, 60)
  sens_H[[i]] <- sens_hosps(test$statuses, 2000)
  sens_F90[[i]] <- sens_fatig(test$concern_ids, 2000, 90)
  sens_F120[[i]] <- sens_fatig(test$concern_ids, 2000, 120)
  sens_I[[i]] <- sens_inf(test$statuses, 2000)
}

saveRDS(sens_I, "C:/Data/Sensitization/params/sens_I.RDS")
saveRDS(sens_H, "C:/Data/Sensitization/params/sens_H.RDS")

