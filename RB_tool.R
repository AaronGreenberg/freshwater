library('arules')
library('lubridate')
rm(list=ls())
                                        #inputs

## wbid="00209BRID"
lwts=2
kf=1.1
## snum<-500 
## straintype="Blackwater"
## ploidytype="Multiple"
## rda<-"2015/10/15"
## eda<-"2017/6/15"



model <- function(wbid,lwts,straintype,ploidytype,rda,eda,snum)
{
#read lakes list
masterlist<-read.csv("RB_tool_master_list.csv")
kf=1.1
#WBID
#Input from tool wbid <- 00209BRID

i1=which(masterlist$WATERBODY_IDENTIFIER==wbid)
lake_area<-masterlist$AREA_HA[i1]


#Stocking size
#Input from tool lwts - stocking weight in g - 2

L0=round(((lwts/(kf/100000))^(1/3)),0)/10 #converting weight in g to length in cm
in_hatch=read.csv('inhatchery_results.csv')
lwd=as.character(discretize(lwts,"fixed",categories = in_hatch$cat,labels=in_hatch$labs))
ind=which(in_hatch$labs==lwd) #adding error around stocking size
cv=ifelse(length(ind)==1,in_hatch$cv[ind],0.1)
sig=cv*L0
tau=1/(sig*sig)
    lfs=ifelse(lwts<=4,"type1","type2")

getfy=function(lfs)
    {#returns NA if lfs is not "type1" or "type2"
    switch(lfs,
           "type1"=1,
           "type2"=0,
           NA)
    }
    #compute fy
    fy=getfy(lfs)

#Stocking density
#Input from tool sum of numbers stocked - 1000

sden<-snum/lake_area #stocking density - total numbers/area
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

#delta_t calculation
#Input from tool - release date - rda<-"2015/10/15"
#Input from tool - evaluation date eda<-"2017/6/15"

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
  # Hyperparameters for Linf 
  ln_Linf_mean <- log(60.31)
  tau2_Linf <- 0.364
  pi<-3.14159
  samples <- 1e3
  Linf<-rlnorm(samples,ln_Linf_mean,tau2_Linf)
  K <- rbeta(samples,1.386,3.869)
  
  
  # Growth parameters
  yef <- rnorm(samples,0.192,0.033)
  ppt <- rnorm(samples,-0.538,0.175)
  osp <- rnorm(samples,0.06,0.02)
  beta<-list(type1=vector(),type2=vector())

  beta$type1<-rnorm(samples,1.049,0.296)
  beta$type2<-rnorm(samples,0.293,0.038)

    #Blackwater=1,Carp=2,Gerrard=3,Multiple=4,Pennask=5
    
  strain <- list(Blackwater=vector(),Carp=vector(),Gerrard=vector(),Multiple=vector(),Pennask=vector())
  strain$Blackwater<-rnorm(samples,0.028,0.024)
  strain$Carp<-rnorm(samples,-0.354,0.128)
  strain$Gerrard<-rnorm(samples,-0.09,0.034)
  strain$Multiple<-rnorm(samples,-0.023,0.037)
  strain$Pennask<-1:samples*0

  #n3=1,Multiple=2,n2=3
  ploidy <- list(n3=vector(),Multiple=vector(),n2=vector())
  ploidy$n3 <-rnorm(samples,0.076,0.017)
  ploidy$Multiple<-rnorm(samples,0.062,0.019)
  ploidy$n2<-1:samples*0
  
  # Measurement errors
  tau2 <- 3.415^(-2)
  
  L0_hatp <- rnorm(samples,L0,sig)
  log.nup <- yef*fy+strain[[straintype]]+ploidy[[ploidytype]]
  nup <- exp(log.nup)
  K_Lp <- K*nup
  
  lgmp<-ppt*map
  gmp<-exp(lgmp)
  Linf_L <- (Linf*gmp)/(1+beta[[lfs]]*drb+osp*dos)
  
  L_hat <- L0_hatp*exp(-K_Lp*dt)+Linf_L*(1-exp(-K_Lp*dt))
}




plotdist <- function(dist,wbid,straintype,ploidytype,rda,eda)
{
    masterlist<-read.csv("RB_tool_master_list.csv")
    i1=which(masterlist$WATERBODY_IDENTIFIER==wbid)
    op <- par(mar = c(5,7,4,2) + 0.1)
    plot(density(dist),axes=TRUE,main="",xlab="",ylab="", ylim=c(0,1.02*max(density(dist)$y)),cex.lab=1.2)
    polygon(density(dist), col=rgb(.8,0,.1, alpha=.1), border="grey")
    title(main=paste("Water body =",masterlist$GAZETTED_NAME[i1],"  ",wbid))
    title(xlab=paste("Predicted length of ",straintype, "in cm \n"),line=4)
    legend("topright", paste("mean (red): \n",signif(mean(dist),3)," cm \n","median (blue): \n",
    signif(median(dist),3)," cm \n","1st decile: \n",
    signif(quantile(dist,prob=.1),3)," cm \n","9th decile: \n",signif(quantile(dist,prob=c(.9)),3),sep=""," cm"),bty="n")
    abline(v=mean(dist),col="red")
    abline(v=median(dist),col="blue")
    abline(v=quantile(dist,prob=c(.1,.9)),col="black")
}

plottable <-function(dist,wbid,straintype,ploidytype,rda,eda,snum,lwts)
{
    masterlist<-read.csv("RB_tool_master_list.csv")
    i1=which(masterlist$WATERBODY_IDENTIFIER==wbid)
    string1 <- paste("Mean Length  =",(signif(mean(dist),3)),"cm")
    string2 <- paste("&sigma; length = ",(signif(sd(dist),3)),"cm")
    string3 <- paste("1st decile =",(signif(quantile(dist,probs=.1),3)),"cm")
    if(masterlist$MAP[i1]<=1000)
    {
        string4 <- paste("Mean Annual Precipitation=",masterlist$MAP[i1],"mm")
    }else
    {
        string4 <- paste("Mean Annual Precipitation=",masterlist$MAP[i1]/10,"cm")
    }
    string5 <- paste("Annual Growing Days=", masterlist$DD5[i1])
    string6 <- paste("Size Stocked =",lwts,"grams")
    string7 <- paste("Stocking Density =",(signif(snum/masterlist$AREA_HA[i1], 3)),"Fish per HA")
    table <- HTML(paste(string1,string2,string3,string4,string5,string6,string7,sep="<br/>"))
return(table)
}

plotbox <- function(dist,wbid,straintype,ploidytype,rda,eda)
{ 
boxplot(dist)
}




