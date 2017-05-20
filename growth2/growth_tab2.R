library('arules')
library('lubridate')

#Input from tool wbid <- 00209BRID
wbid="00372KOTR" #Region and names instead of WBID

strain=2
ploidy=3
lfs=1

main<-function(wbid,strain,ploidy,lfs)
{
#read lakes list
masterlist<-read.csv("RB_tool_master_list.csv")
mainpars<-read.csv('model_17_mainpars.csv')
lakepars<-read.csv('model_17_lakepars.csv')

#WBID

i1=which(masterlist$WATERBODY_IDENTIFIER==wbid)
lake_area<-masterlist$AREA_HA[i1]
assessed<-read.csv('assessed_wbid.csv',colClasses=c("character"))
i2=any(assessed==wbid)
if(i2==TRUE)
{i3=which(assessed==wbid);K<-lakepars[,i3];Linf<-lakepars[,(91+i3)]} else{K<-mainpars[,1];Linf<-mainpars[,2]}

bet=rep(0,2)
stn=rep(0,5)
ple=rep(0,3)

bet[1]<-1.169340805
bet[2]<-0.386799787
osp<-	0.234824569
ple[1]<--0.06223357
ple[2]<--0.118868371
ple[3]<-0
ppt <-	-0.502634529

stn[1]<-0.105100698
stn[2]<--0.344376194
stn[3]<-0.429821161
stn[4]<--0.064977055
stn[5]<-0.085471247
stn[6]<-0
yef<-0.132252879


eda<-c("2012/6/15","2013/6/15","2014/6/15","2015/6/15","2016/6/15")
sden<-c(20,50,100,200,500)
lwts=c(2,10)
kf=1.1
L0=round(((lwts/(kf/100000))^(1/3)),0)/10 #converting weight in g to length in cm
drbfry<-((sden*L0[1]^2)/10^5)-0.0375 #density expressed in NL2
drbye<-((sden*L0[2]^2)/10^5)-0.2
dos<-0
age=seq(2,6)
fy<-lfs-1

#delta_t calculation #no inputs from gui
if(lfs==1) rda="2010/10/15" else rda="2011/06/01"
eda<-c("2012/6/15","2013/6/15","2014/6/15","2015/6/15","2016/6/15")
fy<-lfs-1
dtfry=dtye=vector(mode="numeric",length=length(eda))
for(i in 1:length(eda))
{
  x1=seq.Date(as.Date(rda),as.Date(eda[i]),by='day')
  y1=month(x1)
  mm=mm2=vector(mode="numeric",length=12)
  for(j in 1:12)
  {
    mm[j]=ifelse(length(which(y1==j))<1,0,length(which(y1==j)))
    mm2[j]=ifelse(length(i1)<1,NA,mm[j]/30*masterlist[i1,9+j])
  }
  dtfry[i]=sum(mm2)/1000
  
}
#Precipation
map<-masterlist$MAP[i1]/1000-0.5796408

Linf_Lfry <- (Linf*exp(ppt*map))/(1+bet[lfs]*drbfry+osp*dos)
K_Lpfry <- K*exp(yef*fy+stn[strain]+ple[ploidy])
L_hat_fry<-matrix(data=0,nrow=length(Linf_Lfry),ncol=length(eda))
for(i in 1:length(dtfry))
{
  L_hat_fry[,i] <- L0[1]*exp(-K_Lpfry*dtfry[i])+Linf_Lfry*(1-exp(-K_Lpfry*dtfry[i]))
}
colnames(L_hat_fry)<-paste0("age_",age)
rownames(L_hat_fry)<-paste0("sden_",sden)



fy<-lfs-1


#delta_t calculation #no inputs from gui
if(lfs==1) rda="2010/10/15" else rda="2011/06/01"
eda<-c("2012/6/15","2013/6/15","2014/6/15","2015/6/15","2016/6/15")
dtye=vector(length=length(eda))
for(i in 1:length(eda))
{
  x1=seq.Date(as.Date(rda),as.Date(eda[i]),by='day')
  y1=month(x1)
  mm=mm2=vector(mode="numeric",length=12)
  for(j in 1:12)
  {
    mm[j]=ifelse(length(which(y1==j))<1,0,length(which(y1==j)))
    mm2[j]=ifelse(length(i1)<1,NA,mm[j]/30*masterlist[i1,9+j])
  }
  dtye[i]=sum(mm2)/1000
  
}
Linf_Lye <- (Linf*exp(ppt*map))/(1+bet[lfs]*drbye+osp*dos)
K_Lpye <- K*exp(yef*fy+stn[strain]+ple[ploidy])
L_hat_ye<-matrix(data=0,nrow=length(dtye),ncol=length(eda))
for(i in 1:length(dtye))
{
  L_hat_ye[,i] <- L0[2]*exp(-K_Lpye*dtye[i])+Linf_Lye*(1-exp(-K_Lpye*dtye[i]))
}
colnames(L_hat_ye)<-paste0("age_",age)
    rownames(L_hat_ye)<-paste0("sden_",sden)

out=list(L_hat_ye=L_hat_ye,L_hat_fry=L_hat_fry)
}


###
#Output 2 figures one for fry and one for yearlings
plottab2 <- function(L)
{
    age=seq(2,6)
par(mfrow=c(1,2))
m1 <- max(max(L$L_hat_fry[1,]),max(L$L_hat_fry[2,]),max(L$L_hat_fry[3,]),max(L$L_hat_fry[3,]),max(L$L_hat_fry[4,]),max(L$L_hat_fry[5,]))
m1 <- m1+5
m2 <- max(max(L$L_hat_ye[1,]),max(L$L_hat_ye[2,]),max(L$L_hat_ye[3,]),max(L$L_hat_ye[3,]),max(L$L_hat_ye[4,]),max(L$L_hat_ye[5,]))
m2 <- m2+5
may=max(c(m2,m1))
m1b <- min(min(L$L_hat_fry[1,]),min(L$L_hat_fry[2,]),min(L$L_hat_fry[3,]),min(L$L_hat_fry[3,]),min(L$L_hat_fry[4,]),min(L$L_hat_fry[5,]))
m1b <- m1b-1
m2b <- min(min(L$L_hat_ye[1,]),min(L$L_hat_ye[2,]),min(L$L_hat_ye[3,]),min(L$L_hat_ye[3,]),min(L$L_hat_ye[4,]),min(L$L_hat_ye[5,]))
m2b <- m2b-1    
my=min(c(m2b,m1b))

  plot(age,L$L_hat_fry[1,],type='o',ylim=c(my,may),las=1,cex=.2,xlab="Age", ylab=" Fry",main="L_hat") 
  lines(age,L$L_hat_fry[2,],type='o',cex=.2)
  lines(age,L$L_hat_fry[3,],type='o',cex=.2)
  lines(age,L$L_hat_fry[4,],type='o',cex=.2)
  lines(age,L$L_hat_fry[5,],type='o',cex=.2)

  plot(age,L$L_hat_ye[1,],type='o',ylim=c(my,may),las=1,cex=.2,xlab="Age", ylab="Yearling",main="L_hat") 
  lines(age,L$L_hat_ye[2,],type='o',cex=.2)
  lines(age,L$L_hat_ye[3,],type='o',cex=.2)
  lines(age,L$L_hat_ye[4,],type='o',cex=.2)
  lines(age,L$L_hat_ye[5,],type='o',cex=.2)
}  
#fix yaxis limits such that they are the same for each plot.                                         #sliding label
# write a function that outputs csvs of L_hat_ye and L_hat_fry
s=main(wbid,strain,ploidy,lfs)
plottab2(s)
