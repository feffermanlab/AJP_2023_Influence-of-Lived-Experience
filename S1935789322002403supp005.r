#The majority of this code is by Matt Silk
set.seed(1)
library(rlist)
library(igraph)

library(boot)
setwd("C:/Data/Sensitization")
#where functions and parameter csvs are located
sourcefolder <- "C:/Data/Sensitization/"
paramsfolder <- "C:/Data/Sensitization/params/"

#where the networks are located
networksfolder <-file.path(paramsfolder,"networks2")

#where you want to save the results 
outputfolder<-file.path("output","results4")
if(!dir.exists(outputfolder)) dir.create(outputfolder,recursive = T) #if output folder doesn't exists, create it.
    
source(file.path(sourcefolder,"Measles_Sens_Functions.R"))

params1<-read.csv(file.path(paramsfolder,"COV_mod_nets.csv"))
params2<-read.csv(file.path(paramsfolder,"Meas_Params_new.csv"))
#params3<-read.csv(file.path(paramsfolder,"neigh_he_params.csv"))
sens_pick<-read.csv(file.path(paramsfolder,"which_sens.csv"))

net_params<-read.csv(file.path(paramsfolder,"COV_net.csv"))
sens_I<-readRDS(file.path(paramsfolder,"sens_I.RDS"))
sens_H<-readRDS(file.path(paramsfolder,"sens_H.RDS"))

#h_che<-1

nt <- 1

#start loop over networks
for(md in 1:126){
  sensitives<-numeric()
  prior <- params2$Prior[md]
  if(params2$I[md] == 1){sensitives <- c(sensitives, as.numeric(unlist(sens_I[prior])))}
  if(params2$H[md] == 1){sensitives <- c(sensitives, as.numeric(unlist(sens_H[prior])))}
  if(params2$H[md] == 0.5){sensitives <- c(sensitives, as.numeric(unlist(sens_H[prior]))); sensitives <- sample(sensitives, (length(sensitives)/2), replace=FALSE)}
  
  sensitives <- unique(sensitives[!is.na(sensitives)])
  
    stvT <- params2[md,14]
    cut_mod <- params2[md,18]

par_id<-params1[1,2]

pop_size=2000
ncomms=10
prop_belA=net_params$prop_belA[net_params$NetworkID == par_id]
prop_old=0.13
prop_young=0.63
prop_child=0.24
sens <- rep(0, pop_size)

pop_info<-pop_gen(pop_size,ncomms,prop_belA,prop_old,prop_young,prop_child)
sensitives <- sensitives[sensitives %in% which(pop_info$child != 1)]
sens[sensitives] <- 1
pop_info$sens <- sens
############################################

dis_input<-readRDS(file.path(networksfolder,paste0(params1[1,3],"net_and_parents.RDS")))
info_input<-readRDS(file.path(networksfolder,paste0(params1[1,2],"net_and_parents.RDS")))

parents<-info_input[[2]]
dis_mat<-dis_input[[1]]
info_mat<-info_input[[1]]

dis_mat<-par_ex(pop_info=pop_info,parents=parents,dis_mat=dis_mat)

############################################


#for(md in 15){
# Here we define the prior beliefs of young adults (which will be used as probabilities in a bernoulli draw)
# e.g. currently there is a 50% chance a young adult of political belief A is concerned about the virus
A_concern_y<-params2[md,2]
B_concern_y<-params2[md,3]

#Here we define an additive effect of being old (to accomodate the fact they may be more likely to start concerned)
# e.g. There will be a 70% chance of an old adult of political belief A being concerned about the virus
A_concern_o<-params2[md,4]
B_concern_o<-params2[md,5]

#Here we define a daily extrinsic input into the belief of each political belief (I figured this would suffice to represent exposure to politicians/news/wider social media)
#N.B. These numbers are already defined on a logit scale. But our starting assumption is that concern of political belief A gets puched up and political belief B gets ushed down by these extrinsic factors
#(can obviously set to zero if preferred)
lA_ex<- params2[md,6]
lB_ex<- params2[md,7]

#SC_Define a linear relationship between proportion of connections concerned (at the previous time-step) and concern levels in young adults
l_conc<-params2[md,8]
#and an additive effect used to calculate the same parameter for old adults
l_conc_o<-params2[md,9]

#AW_Define a linear relationship between number of connections infected (at the previous time-step) and concern levels in young adults
l_inf<-params2[md,10]
#and an additive effect used to calculate the same parameter for old adults
l_inf_o<-params2[md,11]

for(r in 1:10){
#for(r in 1){
#RE_Define a threshold relationship whereby concern decreases while all immediate network conncections are fully healthy (at the previous time-step) and concern levels in young adults
l_hea<-params2[md,12]
#and an additive effect used to calculate the same parameter for old adults
l_hea_o<-params2[md,13]

#h_che<-h_che+1

start<-concern_setup(A_concern_y,B_concern_y,A_concern_o,B_concern_o,pop_info,info_mat,stvT)

concern<-list()
belief<-list()

belief[[1]]<-start[[1]]
concern[[1]]<-start[[2]]


############################################

#Probability of becoming exposed having contacted an infectious individual (daily)
#Need to work out R0 based on other parameters

###################################################################################Will need to reconfigure
#https://www.cdc.gov/measles/transmission.html?CDC_AA_refVal=https%3A%2F%2Fwww.cdc.gov%2Fmeasles%2Fabout%2Ftransmission.html
#mean(rec/2000) for 20 runs with no awareness/construction
#0.9001 average 90% transmission
S_E<-1.45/mean(colSums(dis_mat))
#S_E<-0.2
#https://www.cdc.gov/vaccines/pubs/surv-manual/chpt07-measles.pdf median days
#lambda for a Poisson draw for the length of this period ###########################this should be a log normal
E_I1<-11.5

#probability of transitioning to serious case for young (daily)
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4905815/ n/N
#0.0862 / average duration of infection (2.5 days)
yI1_I2<-0.03448
#and same for old 
oI1_I2<-0.03448

#probability of transitioning to critical (HOSPITALISED) case for young (daily)
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4905815/ n/N
#0.1669 / average duration of infection (2.5 days)
yI2_I3<-0.06676
#and same for old
oI2_I3<-0.06676

#probability of death for young (daily)
#https://www.cdc.gov/vaccines/pubs/surv-manual/chpt07-measles.pdf (1-3 out of every 1000)
yI3_D<-0.001
#and same for old
oI3_D<-0.001

#lambda for a Poisson draw for duration of a pre-symptomatic/mild infection - this is now misnamed as impossible to recover - simply transition to I2.
#https://www.cdc.gov/vaccines/pubs/surv-manual/chpt07-measles.pdf median days before rash
yI1_R<-2.5
#and same for old
oI1_R<-2.5

#lambda for a Poisson draw for duration of a serious infection
#https://www.turkarchpediatr.org/Content/files/sayilar/113/Turk%20Arch%20Pediatr%202021_%2056(4)_328-331.pdf
yI2_R<-5
#and same for old
#https://pubmed.ncbi.nlm.nih.gov/8213869/
oI2_R<-6.8

#lambda for a Poisson draw for duration of a critical/hospitalised infection
#https://www.turkarchpediatr.org/Content/files/sayilar/113/Turk%20Arch%20Pediatr%202021_%2056(4)_328-331.pdf
yI3_R<-5
#and same for old
#https://pubmed.ncbi.nlm.nih.gov/8213869/
oI3_R<-6.8

############################################

#start with 2 infected individuals
exp<-sample(1:pop_info$pop,5,replace=FALSE)

#create dataframe to store disease state
S<-rep(1,pop_info$pop)
E<-rep(0,pop_info$pop)
I1<-rep(0,pop_info$pop)
I2<-rep(0,pop_info$pop)
I3<-rep(0,pop_info$pop)
R<-rep(0,pop_info$pop)
D<-rep(0,pop_info$pop)
status<-data.frame(S,E,I1,I2,I3,R,D)

status$S[exp]<-0
status$E[exp]<-1

d_exp<-rep(NA,pop_info$pop)
d_inf1<-rep(NA,pop_info$pop)
d_inf2<-rep(NA,pop_info$pop)
d_inf3<-rep(NA,pop_info$pop)

d_exp[status$E==1]<-rpois(sum(status$E),E_I1)

time<-300

statuses<-list()
statuses[[1]]<-status

progression<-matrix(0,nr=time+1,nc=ncol(status))
progression[1,]<-colSums(status)


############################################

#be ready to change the network in this
for(t in 2:time){
  
  if(t==2){
    
    #time-step 2
    
    dis_mat<-network_rewire_concern(net=dis_mat,concern=start[[2]],concern.prev=rep(0,pop_info$pop),pop_info=pop_info,
                                    prop_cut=0.5,cut_to=0.001,cut_mod)
    dis_mat<-network_rewire_infectionS(net=dis_mat,status=statuses[[t-1]],
                                       pop_info=pop_info,cut_to=0.001)
    #dis_mat<-network_rewire_infectionM(net=dis_mat,status=statuses[[t-1]],
    #                                   pop_info=pop_info,cut_to=0.001)
    dis_mat<-network_rewire_infectionR(net=dis_mat,status=statuses[[t-1]],
                                       pop_info=pop_info,cut_to=0.001)
    
    inf<-cbind(sign(statuses[[t-1]]$I2+statuses[[t-1]]$I3),sign(statuses[[t-1]]$I2+statuses[[t-1]]$I3))
    
    current<-concern_timestep(pop_info=pop_info,net_b=info_mat,net_d=dis_mat,belief=start[[1]],concern=start[[2]],inf=inf,lA_ex,lB_ex,l_conc,l_conc_o,l_inf,l_inf_o,l_hea,l_hea_o,stvT)
    
    belief[[2]]<-current[[1]]
    concern[[2]]<-current[[2]]

    dis<-infection_timestep(pop_info=pop_info,status=statuses[[t-1]],net=dis_mat,d_exp=d_exp,d_inf1=d_inf1,d_inf2=d_inf2,d_inf3=d_inf3,S_E=S_E,E_I1=E_I1,yI1_I2=yI1_I2,oI1_I2=oI1_I2,yI2_I3=yI2_I3,oI2_I3=oI2_I3,yI3_D=yI3_D,oI3_D=oI3_D,yI1_R=yI1_R,oI1_R=oI1_R,yI2_R=yI2_R,oI2_R=oI2_R,yI3_R=yI3_R,oI3_R=oI3_R)
    
  }
  if(t>2){
    
    dis_mat<-network_rewire_concern(net=dis_mat,concern=current[[2]],concern.prev=concern[[t-2]],pop_info=pop_info,
                                    prop_cut=0.5,cut_to=0.001,cut_mod)
    dis_mat<-network_rewire_infectionS(net=dis_mat,status=statuses[[t-1]],
                                       pop_info=pop_info,cut_to=0.001)
    #dis_mat<-network_rewire_infectionM(net=dis_mat,status=statuses[[t-1]],
    #                                   pop_info=pop_info,cut_to=0.001)
    dis_mat<-network_rewire_infectionR(net=dis_mat,status=statuses[[t-1]],
                                       pop_info=pop_info,cut_to=0.001)
    
    inf<-cbind(sign(statuses[[t-1]]$I2+statuses[[t-1]]$I3),sign(statuses[[t-1]]$I2+statuses[[t-1]]$I3))
  
    current<-concern_timestep(pop_info=pop_info,net_b=info_mat,net_d=dis_mat,belief=current[[1]],
                              concern=current[[2]],inf=inf,lA_ex,lB_ex,l_conc,l_conc_o,l_inf,
                              l_inf_o,l_hea,l_hea_o,stvT)
    
    belief[[t]]<-current[[1]]
    concern[[t]]<-current[[2]]  
  
    dis<-infection_timestep(pop_info=pop_info,status=statuses[[t-1]],net=dis_mat,d_exp=dis[[2]],d_inf1=dis[[3]],d_inf2=dis[[4]],d_inf3=dis[[5]],S_E=S_E,E_I1=E_I1,yI1_I2=yI1_I2,oI1_I2=oI1_I2,yI2_I3=yI2_I3,oI2_I3=oI2_I3,yI3_D=yI3_D,oI3_D=oI3_D,yI1_R=yI1_R,oI1_R=oI1_R,yI2_R=yI2_R,oI2_R=oI2_R,yI3_R=yI3_R,oI3_R=oI3_R)
  }
  print(colSums(dis$status))
  progression[t,]<-colSums(dis$status)
  statuses[[t]]<-dis$status
  
  if(sum(colSums(statuses[[t]])[c(2,3,4,5)])==0){break()}
  
}


########################################
########################################

mod_concerns<-matrix(0,nr=10,nc=length(concern))

for(i in 1:length(concern)){
  mod_concerns[,i]<-aggregate(concern[[i]],by=list(pop_info$comms),mean)[,2]
}

mod_exps<-matrix(0,nr=10,nc=length(statuses))

for(i in 1:length(statuses)){
  mod_exps[,i]<-aggregate(statuses[[i]][,2],by=list(pop_info$comms),sum)[,2]
}

mod_infs<-matrix(0,nr=10,nc=length(statuses))

for(i in 1:length(statuses)){
  mod_infs[,i]<-aggregate(statuses[[i]][,4],by=list(pop_info$comms),sum)[,2]
}

mod_hosps<-matrix(0,nr=10,nc=length(statuses))

for(i in 1:length(statuses)){
  mod_hosps[,i]<-aggregate(statuses[[i]][,5],by=list(pop_info$comms),sum)[,2]
}

OUT<-list(mod_concerns,mod_exps,mod_infs,mod_hosps,l_hea, statuses, concern)
names(OUT)<-c("concern","exps","infs","hosps","he", "statuses", "concern_ids")
#debugging


 if(md < 10)
 {saveRDS(OUT, file.path(outputfolder,paste0("Measles_", "nets",params1$NetSelect[nt],"mods0",params2$ModSelect[md],"rep",r,".RDS")))}
 if(md > 9)
 {saveRDS(OUT, file.path(outputfolder,paste0("Measles_", "nets",params1$NetSelect[nt],"mods",params2$ModSelect[md],"rep",r,".RDS")))}

###################################
###################################

cols=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")

plot(NULL,xlim=c(0,250),ylim=c(0,1))
for(i in 1:10){
  lines(x=seq(1,length(concern)),y=mod_concerns[i,],col=cols[i],lwd=3)
}

plot(NULL,xlim=c(0,250),ylim=c(0,50))
for(i in 1:10){
  lines(x=seq(1,length(statuses)),y=mod_infs[i,],col=cols[i],lwd=3)
}

#debugging
#browser()
#plot(sapply(belief, mean, na.rm = TRUE))

} #end r loop

} #end md loop

