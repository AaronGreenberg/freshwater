library('arules')
library('lubridate')

#read lakes list
masterlist<-read.csv("RB_tool_master_list.csv")
mainpars<-read.csv('model_20post_mainpars.csv')
lakepars<-read.csv('model_20post_lakepars.csv')

#WBID
#Input from tool wbid <- 00209BRID
wbid="00372KOTR"
i1=which(masterlist$WATERBODY_IDENTIFIER==wbid)
lake_area<-masterlist$AREA_HA[i1]
assessed<-read.csv('assessed_wbid.csv',colClasses=c("character"))
i2=any(assessed==wbid)
if(i2==TRUE)
{i3=which(assessed==wbid);K<-lakepars[,i3];Linf<-lakepars[,(91+i3)]} else{K<-mainpars[,1];Linf<-mainpars[,2]}

#Stocking size
#Input from tool lwts - stocking weight in g - 2
lwts=2
kf=1.1
L0=round(((lwts/(kf/100000))^(1/3)),0)/10 #converting weight in g to length in cm
in_hatch=read.csv('inhatchery_results.csv')
lwd=as.character(discretize(lwts,"fixed",categories = in_hatch$cat,labels=in_hatch$labs))
ind=which(in_hatch$labs==lwd) #adding error around stocking size
cv=ifelse(length(ind)==1,in_hatch$cv[ind],0.1)
sig=cv*L0
tau=1/(sig*sig)
lfs=ifelse(lwts<=4,1,2)
fy=lfs-1

#Stocking density
#Input from tool sum of numbers stocked - 1000 #stocking density unlike stocking numbers
sden<-300
#sden<-snum/lake_area #stocking density - total numbers/area
dens0<-(sden*L0^2)/10^5 #density expressed in NL2
drb<-ifelse(lfs==1,dens0-0.0375,dens0-0.2)
dos<-0

#Precipation
map<-masterlist$MAP[i1]/1000-0.5796408

#Strain and Ploidy input
#Input from tool for strain and ploidy
#Need the following comment:
#For strains: Blackwater=1,Carp=2,Gerrard=3,Multiple=4,Pennask=5
#For ploidy: 3n=1,Multiple=2,2n=3
strain=5
ploidy=3

#delta_t calculation
#Input from tool - release date - rda<-"2015/10/15"
#Input from tool - evaluation date eda<-"2017/6/15"
rda<-"2015/10/15"
eda<-"2017/6/15"
x1=seq.Date(as.Date(rda),as.Date(eda),by='day')
y1=month(x1)
mm=mm2=vector(mode="numeric",length=12)
for(j in 1:12)
{
  mm[j]=ifelse(length(which(y1==j))<1,0,length(which(y1==j)))
  mm2[j]=ifelse(length(i1)<1,NA,mm[j]/30*masterlist[i1,9+j])
}
dt=sum(mm2)/1000

#model inputs
#L0,tau,dt,map,drb,dos,strain,ploidy,lfs,fy

#model output
#L_hat in the model below

#jags model (does not run because all nodes are fixed)
model {
  ### Priors
  # Hyperparameters for Linf 
  ln_Linf_mean <- log(60.31)
  tau2_Linf <- pow(0.364,-2)
  pi<-3.14159
  
  Linf ~ dlnorm(ln_Linf_mean,tau2_Linf)
  K~dbeta(1.386,3.869)
  
  
  # Growth parameters
  yef ~ dnorm(0.192,0.033)
  ppt ~ dnorm(-0.538,0.175)
  osp ~ dnorm(0.06,0.02)
  bet[1] ~ dnorm(1.049,0.296)
  bet[2]~ dnorm(0.293,0.038)
  
  stn[1]~ dnorm(0.028,0.024)
  stn[2]~ dnorm(-0.354,0.128)
  stn[3]~ dnorm(-0.09,0.034)
  stn[4]~ dnorm(-0.023,0.037)
  stn[5]<-0
  
  ple[1]~ dnorm(0.076,0.017)
  ple[2]~ dnorm(0.062,0.019)
  ple[3]<-0
  
  # Measurement errors
  tau2 <- pow(3.415,-2)
  
  L0_hatp ~ dnorm(L0,tau)
  log.nup <- yef*fy+stn[strain]+ple[ploidy]
  nup <- exp(log.nup)
  K_Lp <- K*nup
  
  lgmp<-ppt*map
  gmp<-exp(lgmp)
  Linf_L <- (Linf*gmp)/(1+bet[lfs]*drb+osp*dos)
  
  L_hat <- L0_hatp*exp(-K_Lp*dt)+Linf_L*(1-exp(-K_Lp*dt))
  
}



