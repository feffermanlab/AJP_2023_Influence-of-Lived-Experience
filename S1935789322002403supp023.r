#The majority of this code is by Matt Silk

###Population generation

pop_gen<-function(pop_size=1000,ncomms=10,prop_belA=0.5,prop_old=0.3,prop_young=0.5, prop_child=0.2){
  
  pop<-pop_size
  
  #Work out number of people of each political belief in each age category 
  n_Ac<-round(prop_child*prop_belA*pop)
  n_Ay<-round(prop_young*prop_belA*pop)
  n_Ao<-round(prop_old*prop_belA*pop)
  
  n_Bc<-round(prop_child*(1-prop_belA)*pop)
  n_By<-round(prop_young*(1-prop_belA)*pop)
  n_Bo<-round(prop_old*(1-prop_belA)*pop)
  
  #recalculate population size in case it has changed slightly (due to rounding)
  pop<-n_Ac+n_Ay+n_Ao+n_Bc+n_By+n_Bo
  
  #Vector to identify each individual's community membership
  comms<-rep(1:ncomms,pop/ncomms)
  
  #Create useful vectors used in the network generation and models
  #Information on political belief of each individual
  AorB<-c(rep("A",n_Ac),rep("B",n_Bc),rep("A",n_Ay),rep("B",n_By),rep("A",n_Ao),rep("B",n_Bo))
  #Indicator of child or not
  child<-c(rep(1,n_Ac+n_Bc),rep(0,n_Ay+n_By+n_Ao+n_Bo))
  #Indicator of adult or not
  adult<-c(rep(0,n_Ac+n_Bc),rep(1,n_Ay+n_By+n_Ao+n_Bo))
  #Indicator if old or not
  old<-c(rep(0,n_Ac+n_Bc+n_Ay+n_By),rep(1,n_Ao+n_Bo))
  
  pop_info<-list(pop,comms,n_Ac,n_Bc,n_Ay,n_By,n_Ao,n_Bo,AorB,child,adult,old)
  names(pop_info)<-c("pop","comms","n_Ac","n_Bc","n_Ay","n_By","n_Ao","n_Bo","AorB","child","adult","old")
  return(pop_info)
  
}

##################################################
##################################################

##Parent Exchange

par_ex<-function(pop_info,parents,dis_mat){

 Ap1<-parents[1:pop_info$n_Ac,1]
 Ap2<-parents[1:pop_info$n_Ac,2]
 Bp1<-parents[(pop_info$n_Ac+1):(pop_info$n_Ac+pop_info$n_Bc),1]
 Bp2<-parents[(pop_info$n_Ac+1):(pop_info$n_Ac+pop_info$n_Bc),2]

 dis_mat[(1:(pop_info$n_Ac+pop_info$n_Bc)),(pop_info$n_Ac+pop_info$n_Bc+1):pop_info$pop]<-dis_mat[(pop_info$n_Ac+pop_info$n_Bc+1):pop_info$pop,(1:(pop_info$n_Ac+pop_info$n_Bc))]<-0

 Acs<-seq(1,pop_info$n_Ac,1)
 Bcs<-seq(1,pop_info$n_Bc,1)

 #We now fill in the child-parent edges into the new supra-adjacency matrix for both political beliefs
 for(i in 1:length(Acs)){
  dis_mat[Acs[i],Ap1[i]+pop_info$n_Ac+pop_info$n_Bc]<-dis_mat[Ap1[i]+pop_info$n_Ac+pop_info$n_Bc,Acs[i]]<-1
  dis_mat[Acs[i],Ap2[i]+pop_info$n_Ac+pop_info$n_Bc]<-dis_mat[Ap2[i]+pop_info$n_Ac+pop_info$n_Bc,Acs[i]]<-1
 }
 for(i in 1:length(Bcs)){
  dis_mat[pop_info$n_Ac+Bcs[i],Bp1[i]+pop_info$n_Ay+pop_info$n_Ac+pop_info$n_Bc]<-dis_mat[Bp1[i]+pop_info$n_Ay+pop_info$n_Ac+pop_info$n_Bc,pop_info$n_Ac+Bcs[i]]<-1
  dis_mat[pop_info$n_Ac+Bcs[i],Bp2[i]+pop_info$n_Ay+pop_info$n_Ac+pop_info$n_Bc]<-dis_mat[Bp2[i]+pop_info$n_Ay+pop_info$n_Ac+pop_info$n_Bc,pop_info$n_Ac+Bcs[i]]<-1
 }

 #Add connections from children to the same old adults as their parents
 #For political belief A
 for(i in 1:pop_info$n_Ac){
  t_ycon<-which(dis_mat[i,(pop_info$n_Ac+pop_info$n_Bc+1):(pop_info$n_Ac+pop_info$n_Bc+pop_info$n_Ay)]==1)
  t_ycon2<-t_ycon+pop_info$n_Ac+pop_info$n_Bc
  for(j in 1:length(t_ycon2)){
    t_ycon3<-which(dis_mat[t_ycon2[j],(pop_info$pop-pop_info$n_Ao-pop_info$n_Bo+1):pop_info$pop]==1)
    t_ycon4<-sort(t_ycon3+pop_info$pop-pop_info$n_Ao-pop_info$n_Bo)
    dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
  }
 }

#For political belief B
 for(i in (pop_info$n_Ac+1):(pop_info$n_Ac+pop_info$n_Bc)){
  t_ycon<-which(dis_mat[i,(pop_info$n_Ac+pop_info$n_Bc+pop_info$n_Ay+1):(pop_info$n_Ac+pop_info$n_Bc+pop_info$n_Ay+pop_info$n_By)]==1)
  t_ycon2<-t_ycon+pop_info$n_Ac+pop_info$n_Bc+pop_info$n_Ay
  for(j in 1:length(t_ycon2)){
    t_ycon3<-which(dis_mat[t_ycon2[j],(pop_info$pop-pop_info$n_Ao-pop_info$n_Bo+1):pop_info$pop]==1)
    t_ycon4<-sort(t_ycon3+pop_info$pop-pop_info$n_Ao-pop_info$n_Bo)
    dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
  }
 }
 
 return(dis_mat)

 
} #end function

###############################################
###############################################


concern_setup<-function(A_concern_y,B_concern_y,A_concern_o,B_concern_o,pop_info,mat_cyo,stvT){
  
  #Calculate versions of initial belief on logit scale
  lA_concern_y<-logit(A_concern_y)
  lB_concern_y<-logit(B_concern_y)
  lA_concern_o<-logit(A_concern_y+A_concern_o)
  lB_concern_o<-logit(B_concern_y+B_concern_o)
  
  #Calculate starting beliefs of each person depending on the parameters defined above
  belief<-c(rep(NA,pop_info$n_Ac),rep(NA,pop_info$n_Bc),rep(lA_concern_y,pop_info$n_Ay),
            rep(lB_concern_y,pop_info$n_By),rep(lA_concern_o,pop_info$n_Ao),rep(lB_concern_o,pop_info$n_Bo))
  belief[pop_info$sens == 1] <- rep(logit(A_concern_y*stvT),length(pop_info[pop_info$sens == 1]))
  #Use these starting beliefs to calculate an initial indication of concern (using a bernoulli draw)
  initial_conc<-c(rep(NA,pop_info$n_Ac+pop_info$n_Bc),rbinom(pop_info$pop-pop_info$n_Ac-pop_info$n_Bc,1,inv.logit(belief[pop_info$n_Ac+pop_info$n_Bc+1:pop_info$pop])))
  
  #Here we set children as taking on the concern-level of their most concerned parent
  for(i in 1:(pop_info$n_Ac+pop_info$n_Bc)){
    t_par<-which(mat_cyo[i,(pop_info$n_Ac+pop_info$n_Bc+1):(pop_info$n_Ac+pop_info$n_Bc+pop_info$n_Ay+pop_info$n_By)]==1)
    initial_conc[i]<-max(initial_conc[t_par+pop_info$n_Ac+pop_info$n_Bc])
  }
  
  #And here is the fully calculated measure of current concern about the virus
  conc<-initial_conc
  
  output<-list(belief,conc)
  names(output)<-c("belief","concern")
  return(output)
  
}

concern_timestep<-function(pop_info,net_b,net_d,belief,concern,inf,lA_ex,lB_ex,l_conc,l_conc_o,l_inf,l_inf_o,l_hea,l_hea_o,stvT){
  
  net1<-net_b
  net2<-net_d
  
  conc<-concern

  #Calculate changes in belief caused by concern of network connections in belief sharing network
  for(i in (pop_info$n_Ac+pop_info$n_Bc+1):(pop_info$pop)){
    t_con<-which(net1[i,]==1)
    t_con<-t_con[t_con>(pop_info$n_Ac+pop_info$n_Bc)]
    if(length(t_con)>0){
      t_prop<-sum(conc[t_con])/length(t_con)
      if(pop_info$sens[i] == 1){
      belief[i]<-belief[i]+t_prop*l_conc*stvT+t_prop*l_conc_o*pop_info$old[i]}
      if(pop_info$sens[i] != 1){
      belief[i]<-belief[i]+t_prop*l_conc+t_prop*l_conc_o*pop_info$old[i]}
    }
  }
  
  #Calculate changes in belief caused infection status of network connections
  for(i in (pop_info$n_Ac+pop_info$n_Bc+1):(pop_info$pop)){
    t_con<-which(net1[i,]==1)
    #t_con<-t_con[t_con>(pop_info$n_Ac+pop_info$n_Bc)]
    if(length(t_con)>0){
      t_inf<-sum(inf[t_con,2])
      if(pop_info$sens[i] == 1){
        belief[i]<-belief[i]+t_inf*l_inf*stvT+t_inf*l_inf_o*pop_info$old[i]}
      if(pop_info$sens[i] != 1){
      belief[i]<-belief[i]+t_inf*l_inf+t_inf*l_inf_o*pop_info$old[i]}
    }
  }
  
  #Calculate changes in belief caused healthiness of all network connections
  for(i in (pop_info$n_Ac+pop_info$n_Bc+1):(pop_info$pop)){
    t_con<-which(net1[i,]==1)
    #t_con<-t_con[t_con>(pop_info$n_Ac+pop_info$n_Bc)]
    if(length(t_con)>0){
      t_inf<-sum(inf[t_con,1])
      if(t_inf==0){
       if(pop_info$AorB[i] == "A"){
        belief[i]<-belief[i]+(l_hea+lA_ex)+t_inf*l_hea_o*pop_info$old[i]}
        if(pop_info$AorB[i] == "B"){
        belief[i]<-belief[i]+(l_hea+lB_ex)+t_inf*l_hea_o*pop_info$old[i]}}
        }
      }
  
  

  #Work out new states of concern (minus children)
  conc<-c(rep(NA,pop_info$n_Ac+pop_info$n_Bc),
          rbinom(pop_info$pop-pop_info$n_Ac-pop_info$n_Bc,1,
                 inv.logit(belief[pop_info$n_Ac+pop_info$n_Bc+1:pop_info$pop])))
  
  #Add in concern of children (based on their most conerned parent)
  for(i in 1:(pop_info$n_Ac+pop_info$n_Bc)){
    t_par<-which(net1[i,(pop_info$n_Ac+pop_info$n_Bc+1):
                        (pop_info$n_Ac+pop_info$n_Bc+pop_info$n_Ay+pop_info$n_By)]==1)
    conc[i]<-max(conc[t_par+pop_info$n_Ac+pop_info$n_Bc])
  }
  
  #Compare mean concern of political belief's A and B
  #print(aggregate(conc,list(AorB),mean))
  
  #output<-list(belief,conc)
  #debugging
  output<-list(belief,conc)
  
  #names(output)<-c("belief","concern")
  #debugging
  names(output)<-c("belief","concern")
  
  return(output)
  
} #end of function

###############################################
###############################################


infection_timestep<-function(pop_info,status,net,d_exp,d_inf1,d_inf2,d_inf3,S_E,E_I1,yI1_I2,oI1_I2,yI2_I3,oI2_I3,yI3_D,oI3_D,yI1_R,oI1_R,yI2_R,oI2_R,yI3_R,oI3_R){
  
  mat_cyo<-net
  old<-pop_info$old
  child<-pop_info$child
  young<-sign(1-old-child)
  
  #this will block individuals changing twice in the same time-step
  f2c<-rep(1,pop_info$pop)
  inf<-sign(status$I1+status$I2+status$I3)
  risk<-rep(0,pop_info$pop)
  risk[as.numeric(names(table(which(inf*mat_cyo==1,arr.ind=TRUE)[,2])))]<-table(which(inf*mat_cyo==1,arr.ind=TRUE)[,2])
  prob<-(1-S_E)^risk
  infect<-rbinom(pop_info$pop,1,1-prob)*status$S
  status$S[which(infect==1)]<-0
  status$E[which(infect==1)]<-1
  d_exp[!is.na(d_exp)&d_exp>0]<-d_exp[!is.na(d_exp)&d_exp>0]-1
  d_exp[which(infect==1)]<-rpois(length(which(infect==1)),E_I1)
  f2c[which(infect==1)]<-0
  EI1<-which(status$E==1&!is.na(d_exp)&d_exp==0)
  status$E[EI1]<-0
  status$I1[EI1]<-1
  f2c[EI1]<-0
  I1I2<-which(status$I1==1&!is.na(d_inf1)&d_inf1==0)
  status$I1[I1I2]<-0
  status$I2[I1I2]<-1
  f2c[I1I2]<-0
  I2I3<-rbinom(pop_info$pop,1,child*0+young*yI2_I3+old*oI2_I3)*status$I2*f2c
  status$I2[which(I2I3==1)]<-0
  status$I3[which(I2I3==1)]<-1  
  f2c[which(I2I3==1)]<-0
  I3D<-rbinom(pop_info$pop,1,young*yI3_D+old*oI3_D)*status$I3*f2c
  status$I3[which(I3D==1)]<-0
  status$D[which(I3D==1)]<-1
  f2c[which(I3D==1)]<-0
  
  EI1y<-EI1[EI1%in%which(young==1|child==1)]
  EI1o<-EI1[EI1%in%which(old==1)]
  d_inf1[!is.na(d_inf1)&d_inf1>0]<-d_inf1[!is.na(d_inf1)&d_inf1>0]-1
  if(length(EI1y)>0){
    d_inf1[EI1y]<-rpois(length(EI1y),yI1_R)
  }
  if(length(EI1o)>0){
    d_inf1[EI1o]<-rpois(length(EI1o),oI1_R)
  }
  
  I1I2y<-I1I2[I1I2%in%which(young==1|child==1)]
  I1I2o<-I1I2[I1I2%in%which(old==1)]
  d_inf2[!is.na(d_inf2)&d_inf2>0]<-d_inf2[!is.na(d_inf2)&d_inf2>0]-1
  #d_inf2[which(I1I2==1&young==1)]<-rpois(length(which(I1I2==1&young==1)),yI2_R)
  #d_inf2[which(I1I2==1&old==1)]<-rpois(length(which(I1I2==1&old==1)),oI2_R)
  if(length(I1I2y)>0){
    d_inf2[I1I2y]<-rpois(length(I1I2y),yI2_R)
  }
  if(length(I1I2o)>0){
    d_inf2[I1I2o]<-rpois(length(I1I2o),oI2_R)
  }
  
  d_inf3[!is.na(d_inf3)&d_inf3>0]<-d_inf3[!is.na(d_inf3)&d_inf3>0]-1
  d_inf3[which(I2I3==1&young==1)]<-rpois(length(which(I2I3==1&young==1)),yI3_R)
  d_inf3[which(I2I3==1&old==1)]<-rpois(length(which(I2I3==1&old==1)),oI3_R)
  
  #I1R<-which(status$I1==1&!is.na(d_inf1)&d_inf1==0)
  #status$I1[I1R]<-0
  #status$R[I1R]<-1
  #f2c[I1R]<-0
  I2R<-which(status$I2==1&!is.na(d_inf2)&d_inf2==0)
  status$I2[I2R]<-0
  status$R[I2R]<-1
  f2c[I2R]<-0
  I3R<-which(status$I3==1&!is.na(d_inf3)&d_inf3==0)
  status$I3[I3R]<-0
  status$R[I3R]<-1
  f2c[I3R]<-0
  
  output<-list(status,d_exp,d_inf1,d_inf2,d_inf3)
  names(output)<-c("status","d_exp","d_inf1","d_inf2","d_inf3")
  return(output)
  
} #end of function  

###############################################
###############################################


network_rewire_concern<-function(net,concern,concern.prev,pop_info,prop_cut,cut_to,cut_mod){
  for(i in 1:nrow(net)){
    if(concern[i]-concern.prev[i]==-1){
      if(pop_info$child[i]==1){
        t_con<-which(net[i,which(pop_info$child==1)]==cut_to)
        lt_con<-length(t_con)
        t_cut<-rbinom(lt_con,1,1)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-1
      }
      if(pop_info$child[i]==0&pop_info$old[i]==0){
        t_con<-which(net[i,]>0&pop_info$child==0)
        lt_con<-length(t_con)
        t_cut<-rbinom(lt_con,1,1)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-1
        t_par<-which(net[i,]>0&pop_info$child==1)
        t_con2<-which(net[i,]==1&pop_info$old==1)
        if(length(t_par)>0){
          net[t_par,t_con2]<-net[t_con2,t_par]<-1
        }
      }
      if(pop_info$old[i]==1){
        t_con<-which(net[i,]>0&pop_info$child==0)
        lt_con<-length(t_con)
        t_cut<-rbinom(lt_con,1,1)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-1
        t_y<-which(net[i,]==1&pop_info$child==0&pop_info$old==0)
        if(length(t_y)==1){
          t_con2<-unique(which(net[t_y,1:(pop_info$n_Ac+pop_info$n_Bc)]>0))
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-1
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(pop_info$n_Ac+pop_info$n_Bc)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-1
          }
        }
      }  
    }
    if(concern[i]-concern.prev[i]==1){
      if(pop_info$child[i]==1){
        t_con<-which(net[i,which(pop_info$child==1)]>0)
        lt_con<-length(t_con)
        t_cut<-rbinom(lt_con,1,prop_cut)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(pop_info$child[i]==0&pop_info$old[i]==0){
        t_con<-which(net[i,]>0&pop_info$child==0)
        lt_con<-length(t_con)
        if(pop_info$sens[i] == 1){
          t_cut<-rbinom(lt_con,1,cut_mod)
        }
        if(pop_info$sens[i] == 0){
          t_cut<-rbinom(lt_con,1,prop_cut)
        }
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_par<-which(net[i,]>0&pop_info$child==1)
        t_con2<-which(net[i,]==cut_to&pop_info$old==1)
        if(length(t_par)>0){
          net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
        }
      }
      if(pop_info$old[i]==1){
        t_con<-which(net[i,]>0&pop_info$child==0)
        lt_con<-length(t_con)
        if(pop_info$sens[i] == 1){
          t_cut<-rbinom(lt_con,1,cut_mod)
        }
        if(pop_info$sens[i] == 0){
          t_cut<-rbinom(lt_con,1,prop_cut)
        }
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&pop_info$child==0&pop_info$old==0)
        if(length(t_y)==1){
          t_con2<-unique(which(net[t_y,1:(pop_info$n_Ac+pop_info$n_Bc)]>0))
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(pop_info$n_Ac+pop_info$n_Bc)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
  }
  return(net)
}

network_rewire_infectionS<-function(net,status,pop_info,cut_to){
  to_cut<-which(status$I2+status$I3>0)
  if(length(to_cut)>0){
    for(i in 1:length(to_cut)){
      t_c<-which(net[to_cut[i],]==1)
      net[to_cut[i],t_c]<-net[t_c,to_cut[i]]<-cut_to
    }
  }
  return(net)
}

network_rewire_infectionM<-function(net,status,pop_info,cut_to){
  to_cut<-which(status$I1>0)
  if(length(to_cut)>0){
    for(i in 1:length(to_cut)){
      t_c<-which(net[to_cut[i],]==1)
      if(pop_info$child[to_cut[i]]==1){
        t_c<-t_c[which(t_c%in%which(pop_info$child==0&pop_info$old==0)==FALSE)]
      }
      if(pop_info$child[to_cut[i]]==0&pop_info$old[to_cut[i]]==0){
        t_c<-t_c[which(t_c%in%which(pop_info$child==1)==FALSE)]
      }
      net[to_cut[i],t_c]<-net[t_c,to_cut[i]]<-cut_to
    }
  }
  return(net)
}

network_rewire_infectionR<-function(net,status,pop_info,cut_to){
  to_add<-which(status$R>0)
  if(length(to_add)>0){
    for(i in 1:length(to_add)){
      t_c<-which(net[to_add[i],]==cut_to)
      net[to_add[i],t_c]<-net[t_c,to_add[i]]<-1
    }
  }
  return(net)
}


###############################
#############################

ep_start<-function(input,thresh=5){
  return(min(which(input>(thresh-1))))
}
