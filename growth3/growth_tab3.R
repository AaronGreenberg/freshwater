library('arules')
library('lubridate')



mainpars<-read.csv('model_17_mainpars.csv')
K<-mainpars[,1];
Linf<-mainpars[,2]

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

sden<-c(20,50,100,200,500,1000)
dos<-0
strain=6
ploidy=3

lwts=c(2,10)
kf=1.1
L0=round(((lwts/(kf/100000))^(1/3)),0)/10 #converting weight in g to length in cm
map=seq(400,1100,by=50)/1000
gdd=seq(700,1100,by=50)/1000
myvars<-expand.grid(sden,map,gdd)
colnames(myvars)<-c('sden','map','gdd')
#time since stocking
#bullet for this time
dtfry=3
dtye=3
#fry
lfs=1
fy<-lfs-1
drbfry<-((myvars$sden*L0[2]^2)/10^5)-0.0375 #density expressed in NL2
Linf_Lfry <- (Linf*exp(ppt*(myvars$map-0.578)))/(1+bet[lfs]*drbfry+osp*dos)
K_Lpfry <- K*exp(yef*fy+stn[strain]+ple[ploidy])
L_hat_fry<- L0[1]*exp(-K_Lpfry*dtfry*myvars$gdd)+Linf_Lfry*(1-exp(-K_Lpfry*dtfry*myvars$gdd))




#ye
lfs=2
yf<-lfs-1
drbye<-((myvars$sden*L0[2]^2)/10^5)-0.2
Linf_Lye <- (Linf*exp(ppt*map))/(1+bet[lfs]*drbye+osp*dos)
K_Lpye <- K*exp(yef*fy+stn[strain]+ple[ploidy])
L_hat_ye <- L0[2]*exp(-K_Lpye*dtye*myvars$gdd)+Linf_Lye*(1-exp(-K_Lpye*dtye*myvars$gdd))

#Output 2 figures one for fry and one for yearlings
library(ggplot2)
test <- data.frame(
  x = myvars$map,
  y = myvars$gdd,
  w = myvars$sden,
  z1 = L_hat_fry,
  z2=L_hat_ye)
colors <- colorRampPalette(c("blue", "green", "yellow", "red"))(42)
fryplot<-ggplot(test, aes(x=x, y=y, fill = z1)) + geom_tile()+ 
  scale_fill_gradient(low = "white",high = "steelblue")+facet_wrap(~w)
plot(fryplot)

yrplot<-ggplot(test, aes(x=x, y=y, fill = z2)) + geom_tile()+ 
  scale_fill_gradient(low = "white",high = "steelblue")+facet_wrap(~w)
plot(yrplot)



