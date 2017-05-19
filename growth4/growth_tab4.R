library(maps)
library(mapdata)

sden<-c(20,50,100,200,500)
eda<-"2018/6/15"

#main <- sden and eda fry and yearling 
main <- function(sden=sden, eda=eda)
{
haspclist<-read.csv("RB_haspc2017_master_list.csv")
assessed<-scan('assessed_wbid.csv',what="character",skip=1)
lakepars<-read.csv('model_17post_lakepars.csv')
mainpars<-read.csv('model_17post_mainpars.csv')


gnpars<-match(haspclist$WATERBODY_IDENTIFIER,assessed)
vK=vLinf=vector(length=nrow(haspclist))
for(i in 1:length(gnpars))
{
  if(is.na(gnpars[i])==TRUE)
    {vK[i]<-mainpars[1,1];vLinf[i]<-mainpars[1,2]} else
    {i3=gnpars[i];vK[i]<-lakepars[1,i3];vLinf[i]<-lakepars[1,(91+i3)]}
}

lwts=c(2,10)
kf=1.1
L0=round(((lwts/(kf/100000))^(1/3)),0)/10 #converting weight in g to length in cm

drbfry<-((sden*L0[1]^2)/10^5)-0.0375 #density expressed in NL2
drbye<-((sden*L0[2]^2)/10^5)-0.2
beta_fry=1.169
beta_ye=0.3868
osp<-	0.234824569
ppt <-	-0.502634529

yef<-0.132252879


matLinf_ye=matLinf_fry=matLinf_ye=matrix(data=0,nrow=length(vLinf),ncol=length(sden))
  for(j in 1:length(sden))
  {
    matLinf_fry[,j]<-(vLinf*exp(ppt*(haspclist$MAP/1000-0.5796408)))/(1+beta_fry*drbfry[j])
  } 
  
  for(j in 1:length(sden))
  {
     matLinf_ye[,j]<-(vLinf*exp(ppt*(haspclist$MAP/1000-0.5796408)))/(1+beta_ye*drbye[j])
  } 

vKye<-vK*exp(yef)
library('lubridate')

rda_fry<-"2015/10/15"
rda_ye<-"2016/06/01"

x1=seq.Date(as.Date(rda_fry),as.Date(eda),by='day')
y1=month(x1)
mm=vector(mode="numeric",length=12)
mm2=matrix(data=0,nrow=nrow(haspclist),ncol=12)
for(j in 1:12)
{
  mm[j]=ifelse(length(which(y1==j))<1,0,length(which(y1==j)))
  mm2[,j]=mm[j]/30*haspclist[,11+j]
}
dt_fry=rowSums(mm2)/1000



x1=seq.Date(as.Date(rda_ye),as.Date(eda),by='day')
y1=month(x1)
mm=vector(mode="numeric",length=12)
mm2=matrix(data=0,nrow=nrow(haspclist),ncol=12)
for(j in 1:12)
{
  mm[j]=ifelse(length(which(y1==j))<1,0,length(which(y1==j)))
  mm2[,j]=mm[j]/30*haspclist[,11+j]
}
dt_ye=rowSums(mm2)/1000

lt_fry<-L0*exp(-vK*dt_fry)+matLinf_fry*(1-exp(-vK*dt_fry))
lt_ye<-L0*exp(-vKye*dt_ye)+matLinf_ye*(1-exp(-vKye*dt_ye))

colnames(lt_fry)<-paste0("sden_",sden)
colnames(lt_ye)<-paste0("sden_",sden)
    out <- list(lt_fry=lt_fry, lt_ye=lt_ye)
    return(out)
}

                                        #jpeg("img1.jpg",width=9,height=6,units="in",res=600)
plotfry <- function(inp,stock)
{
haspclist<-read.csv("RB_haspc2017_master_list.csv")
lonlim<-c(-130,-115)#range(c(obj@lakex,obj@pcx))+c(0.5,-0.5)
latlim<-c(48.5,56.5)#range(c(obj@lakey,obj@pcy))+c(-0.5,0.5)
# lcol<-rep("#99999970",obj@nl)
# llcol=rep('#99999970',obj@nl)

lakepoints<-data.frame(X=haspclist$LONGITUDE,Y=haspclist$LATITUDE)
EID=seq(1:nrow(haspclist))


plot(lonlim,latlim,col="white",axes=F,xlab="",ylab="")
map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,fill=T,col='white',mar=rep(0.3,4),add=T,lwd=1)#make a first plot of the mapto define the range of the plot 
cexy=inp$lt_fry[,stock]/10
cols<-ifelse(inp$lt_fry[,stock]<25,"red",ifelse(inp$lt_fry[,stock]<35,"orange",ifelse(inp$lt_fry[,stock]<40,"green","lightsteelblue")))
text(-121.4425,49.3830, "Hope",cex=.5)
text(-122.768215,53.912015, "Prince George",cex=.5)
points(haspclist$LONGITUDE,haspclist$LATITUDE,pch=".",cex=3,col=cols)
}
#dev.off()

                                        #jpeg("img2.jpg",width=9,height=6,units="in",res=600)

plotye <- function(inp,stock)
{

haspclist<-read.csv("RB_haspc2017_master_list.csv")
lonlim<-c(-130,-115)#range(c(obj@lakex,obj@pcx))+c(0.5,-0.5)
latlim<-c(48.5,56.5)#range(c(obj@lakey,obj@pcy))+c(-0.5,0.5)
# lcol<-rep("#99999970",obj@nl)
# llcol=rep('#99999970',obj@nl)

lakepoints<-data.frame(X=haspclist$LONGITUDE,Y=haspclist$LATITUDE)
EID=seq(1:nrow(haspclist))

haspclist<-read.csv("RB_haspc2017_master_list.csv")
plot(lonlim,latlim,col="white",axes=F,xlab="",ylab="")
map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,fill=T,col='white',mar=rep(0.3,4),add=T,lwd=1)#make a first plot of the mapto define the range of the plot 
cexy=inp$lt_fry[,stock]/10
cols<-ifelse(inp$lt_ye[,stock]<25,"red",ifelse(inp$lt_ye[,stock]<35,"orange",ifelse(inp$lt_ye[,stock]<40,"green","lightsteelblue")))
text(-121.4425,49.3830, "Hope",cex=.5)
text(-122.768215,53.912015, "Prince George",cex=.5)
points(haspclist$LONGITUDE,haspclist$LATITUDE,pch=".",cex=3,col=cols)
}

s <- main(sden=sden,eda=eda)
x11()
plotfry(s,2)
print("hum")
x11()
plotye(s,1)

## print(lt_fry)
## print(lt_ye)

## write.csv(lt_fry,"Predicted size at age for fry stockings.csv")
## write.csv(lt_ye,"Predicted size at age for fry stockings.csv")
