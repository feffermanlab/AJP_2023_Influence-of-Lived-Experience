library(tidyverse)

df <- list.files(path=("C:/Data/Sensitization/output/results4"), pattern = ".RDS")
ParOut <- read.csv("C:/Data/Sensitization/params/Meas_Params_new.csv")
runs <- nrow(ParOut)
 ParOut$rep <- rep(10, runs)
 ParOut <- as.data.frame(lapply(ParOut, rep, ParOut$rep))
 ParOut$rep <- rep(c(1:10), runs)
df2 <- paste0("C:/Data/Sensitization/output/results4/", df)
df2 <- as.data.frame(df2)
df2$mod <- df2[,1] %>% str_replace("rep.*", "")
df2$mod <- df2$mod %>% str_replace(".*mods", "")
df2$mod <- as.numeric(df2$mod)
df2 <- df2[order(df2$mod),]
ParOut$id <- df2$df2

#Create empty variables
peak<-numeric()
whenpeak<-numeric()
exp<-numeric()
hosp<-numeric()
minconc<-numeric()

#Create empty matrices
peak_comm<-matrix(nr=10,nc=(length(df)))
whenpeak_comm<-matrix(nr=10,nc=(length(df)))
exp_comm<-matrix(nr=10,nc=(length(df)))
hosp_comm<-matrix(nr=10,nc=(length(df)))
minconc_com<-matrix(nr=10,nc=(length(df)))
whenmico_com<-matrix(nr=10,nc=(length(df)))
maxconc_com<-matrix(nr=10,nc=(length(df)))
whenmaco_com<-matrix(nr=10,nc=(length(df)))

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
  minconc[i]<-min(colSums(test[[1]]))

  peak_comm[,i]<-apply(test[[3]],1,max)
  whenpeak_comm[,i]<-apply(test[[3]],1,which.max)
  exp_comm[,i]<-apply(test[[2]],1,max)
  hosp_comm[,i]<-apply(test[[4]],1,max)
  minconc_com[,i]<-apply(test[[1]],1,min)
  whenmico_com[,i]<-apply(test[[1]],1,which.min)
  maxconc_com[,i]<-apply(test[[1]],1,max)
  whenmaco_com[,i]<-apply(test[[1]],1,which.max)
  
  }
ParOut$Peak <- peak

ParOut$facet[ParOut$H == 0 & ParOut$I == 0] <- "Null"
ParOut$facet[ParOut$H == 1 & ParOut$I == 0] <- "Hospitalized (100%)"
ParOut$facet[ParOut$H == 0.5 & ParOut$I == 0] <- "Hospitalized (50%)"

ParOut$facet = factor(ParOut$facet, levels = c("Hospitalized (100%)", "Null", "Hospitalized (50%)"))

ggplot(data = ParOut[ParOut$stvT != 10 & ParOut$stvT != 0.1,], aes(y=Peak, x=as.factor(stvT), 
       fill=as.factor(cut_mod), color=as.factor(l_inf))) + geom_boxplot(lwd=0.8) + 
  scale_fill_brewer(palette="RdYlBu") + scale_color_brewer(palette="Greys") + 
  facet_grid(~ParOut$facet[ParOut$stvT != 10 & ParOut$stvT != 0.1]) + theme_classic() + 
  labs(fill = "Proportion of \nContacts Cut", color="Awareness & \nSocial Construction", size=21, x = "Sensitivity", y = "Maximum Peak Infections") + 
  theme(text=element_text(size=17)) + ylim(175,300)

dupe <- ParOut[ParOut$facet=="Null",]
dupe$facet <- "Hospitalized (50%)"

ParOut2 <- ParOut
ParOut2$facet[ParOut2$facet == "Null"] <- "Hospitalized (100%)"
ParOut2 <- rbind(ParOut2, dupe)
ParOut2$stvT[ParOut$facet == "Null"] <- 1

meds <- c(median(ParOut$Peak[ParOut$facet == "Null" & ParOut$l_inf==0.1]), 
          median(ParOut$Peak[ParOut$facet == "Null" & ParOut$l_inf==0.2]),
          median(ParOut$Peak[ParOut$facet == "Null" & ParOut$l_inf==0.3]))

iqrs <- c(IQR(ParOut$Peak[ParOut$facet == "Null" & ParOut$l_inf==0.1]), 
          IQR(ParOut$Peak[ParOut$facet == "Null" & ParOut$l_inf==0.2]),
          IQR(ParOut$Peak[ParOut$facet == "Null" & ParOut$l_inf==0.3]))

rect <- cbind(meds, iqrs)
rect <- as.data.frame(rect)
rect$mins <- meds-(iqrs/2)
rect$maxs <- meds+(iqrs/2)

ggplot(data = ParOut2, aes(y=Peak, x=as.factor(stvT), fill=as.factor(cut_mod), color=as.factor(l_inf))) + 
  geom_segment(aes(x = 0.5, y = rect$meds[1], xend = 7.5, yend = rect$meds[1], color=as.factor(0.1)), linetype="dashed", lwd=1.2) + 
  geom_segment(aes(x = 0.5, y = rect$meds[2], xend = 7.5, yend = rect$meds[2], color=as.factor(0.2)), linetype="dashed", lwd=1.2) + 
  geom_segment(aes(x = 0.5, y = rect$meds[3], xend = 7.5, yend = rect$meds[3], color=as.factor(0.3)), linetype="dashed", lwd=1.2) + 
  geom_segment(aes(x = 0.5, y = rect$mins[1], xend = 7.5, yend = rect$mins[1], color=as.factor(0.1)), linetype="dotted", lwd=0.8) + 
  geom_segment(aes(x = 0.5, y = rect$mins[2], xend = 7.5, yend = rect$mins[2], color=as.factor(0.2)), linetype="dotted", lwd=0.8) + 
  geom_segment(aes(x = 0.5, y = rect$mins[3], xend = 7.5, yend = rect$mins[3], color=as.factor(0.3)), linetype="dotted", lwd=0.8) + 
  geom_segment(aes(x = 0.5, y = rect$maxs[1], xend = 7.5, yend = rect$maxs[1], color=as.factor(0.1)), linetype="dotted", lwd=0.8) + 
  geom_segment(aes(x = 0.5, y = rect$maxs[2], xend = 7.5, yend = rect$maxs[2], color=as.factor(0.2)), linetype="dotted", lwd=0.8) + 
  geom_segment(aes(x = 0.5, y = rect$maxs[3], xend = 7.5, yend = rect$maxs[3], color=as.factor(0.3)), linetype="dotted", lwd=0.8) + 
  geom_boxplot(lwd=1.0) + 
  scale_fill_brewer(palette="RdYlBu") + scale_color_manual(values=c("#CCCCCC", "#999999","#000000")) + 
  facet_grid(ParOut2$facet) + theme_classic() + 
  labs(fill = "Proportion of \nContacts Cut", color="Awareness & \nSocial Construction", size=21, x = "Sensitivity", y = "Maximum Peak Infections") + 
  theme(text=element_text(size=17)) + scale_x_discrete(labels=c("1" = "Null"))  + ylim(175,300)


ggplot(data = ParOut2, aes(y=Peak, x=as.factor(stvT), fill=as.factor(cut_mod), color=as.factor(l_inf))) + 
  geom_segment(aes(x = 0.5, y = rect$meds[1], xend = 7.5, yend = rect$meds[1], color=as.factor(0.1)), linetype="dashed", lwd=1.2) + 
  geom_segment(aes(x = 0.5, y = rect$meds[2], xend = 7.5, yend = rect$meds[2], color=as.factor(0.2)), linetype="dashed", lwd=1.2) + 
  geom_segment(aes(x = 0.5, y = rect$meds[3], xend = 7.5, yend = rect$meds[3], color=as.factor(0.3)), linetype="dashed", lwd=1.2) + 
  geom_boxplot(lwd=1.0) + 
  scale_fill_brewer(palette="RdYlBu") + scale_color_manual(values=c("#CCCCCC", "#999999","#000000")) + 
  facet_grid(ParOut2$facet) + theme_classic() + 
  labs(fill = "Proportion of \nContacts Cut", color="Awareness & \nSocial Construction", size=21, x = "Sensitivity", y = "Maximum Peak Infections") + 
  theme(text=element_text(size=20)) + scale_x_discrete(labels=c("1" = "Null"))  + ylim(175,300)  +  theme(strip.text.y = element_text(face = "bold"), panel.spacing = unit(2, "lines")) +
  xlab(expression("Desensitization  " %<-% "  No Sensitization  " %->% "  Sensitization   "))+ theme(legend.position="bottom") +     theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))

