devtools::install_github("yutannihilation/gglabeledcontour")
library('arules')
library('lubridate')
library('ggplot2')
library('gglabeledcontour')



# mainpars<-read.csv('model_17_mainpars.csv')
# K<-mainpars[,1];Linf<-mainpars[,2]

sden<-c(20,50,100,200,500,1000)
age <- 1

main <- function(sden,age)
    {


Linf=57.9669
K=0.286355
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
dtfry=3
dtye=2.7
#fry
lfs=1
fy<-lfs-1
drbfry<-((myvars$sden*L0[1]^2)/10^5)-0.0375 #density expressed in NL2
Linf_Lfry <- (Linf*exp(ppt*(myvars$map-0.578)))/(1+bet[lfs]*drbfry+osp*dos)
K_Lpfry <- K*exp(yef*fy+stn[strain]+ple[ploidy])
L_hat_fry<- L0[1]*exp(-K_Lpfry*dtfry*myvars$gdd)+Linf_Lfry*(1-exp(-K_Lpfry*dtfry*myvars$gdd))




#ye
lfs=2
yf<-lfs-1
drbye<-((myvars$sden*L0[2]^2)/10^5)-0.2
Linf_Lye <- (Linf*exp(ppt*(myvars$map-0.578)))/(1+bet[lfs]*drbye+osp*dos)
K_Lpye <- K*exp(yef*fy+stn[strain]+ple[ploidy])
L_hat_ye <- L0[2]*exp(-K_Lpye*dtye*myvars$gdd)+Linf_Lye*(1-exp(-K_Lpye*dtye*myvars$gdd))






mydata <- data.frame(
  x = myvars$map*1000,
  y = myvars$gdd*1000,
  w = myvars$sden,
  z1 = L_hat_fry,z2=L_hat_ye)

return(mydata)
    }

plot1 <- function(mydata)
    {

d1 <- ggplot(mydata, aes(x, y, z = z1)) +
  geom_labeled_contour(colour="grey15")+facet_wrap(~w)

d1+theme_bw()+
  labs(title="Length at age 3+", x="Mean Annual Precipitation (in mm)", y="Annual Growing Degree Days")

ggsave(file="fryplot_contour.jpg",height=15,width=20,unit="cm",dpi=500)


d2 <- ggplot(mydata, aes(x, y, z = z2)) +
  geom_labeled_contour(colour="grey15")+facet_wrap(~w)

d2+theme_bw()+
  labs(title="Length at age 3+", x="Mean Annual Precipitation (in mm)", y="Annual Growing Degree Days")
plot(d2)

}
        
## ggsave(file="yeplot_contour.jpg",height=15,width=20,unit="cm",dpi=500)
s=main(sden=200,age=2)
plot1(s)


