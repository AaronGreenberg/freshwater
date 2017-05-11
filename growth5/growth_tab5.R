library(maps)
library(mapdata)

main <- function()
{            
haspclist<-read.csv("RB_haspc2017_master_list.csv")
assessed<-scan('assessed_wbid.csv',what="character",skip=1)
lakepars<-read.csv('model_17_lakepars.csv',header=T)
mainpars<-read.csv('model_17_mainpars.csv',header=T)

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

beta_fry=1.169
beta_ye=0.242
ppt=-0.53
ye=0.132

vKye<-vK*exp(ye)
library('lubridate')

rda_fry<-"2015/10/15"
rda_ye<-"2016/06/01"
eda<-"2018/6/15"
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

target<-40 #input from gui
#region
s1=vLinf*exp(ppt*(haspclist$MAP/1000-0.5796408))*(1-exp(-vK*dt_fry))
s2=s1/(target-(L0[1]*exp(-vK*dt_fry)))
s3=s2-1
s4=s3/beta_fry
s5=s4+0.0375
s6_fry=(s5*10^5)/L0[1]^2

s1=vLinf*exp(ppt*(haspclist$MAP/1000-0.5796408))*(1-exp(-vKye*dt_ye))
s2=s1/(target-(L0[2]*exp(-vKye*dt_ye)))
s3=s2-1
s4=s3/beta_ye
s5=s4+0.2
    s6_ye=(s5*10^5)/L0[2]^2
out <- list(s6_ye=s6_ye,s6_fry=s6_fry)
}



plots6fry <- function(inp)
{                 
haspclist<-read.csv("RB_haspc2017_master_list.csv")
lonlim<-c(-130,-115)#range(c(obj@lakex,obj@pcx))+c(0.5,-0.5)
latlim<-c(48.4,56.5)#range(c(obj@lakey,obj@pcy))+c(-0.5,0.5)
# lcol<-rep("#99999970",obj@nl)
# llcol=rep('#99999970',obj@nl)

lakepoints<-data.frame(X=haspclist$LONGITUDE,Y=haspclist$LATITUDE)
EID=seq(1:nrow(haspclist))


#jpeg("img_fry.jpg",width=9,height=6,units="in",res=600)
plot(lonlim,latlim,col="white",axes=F,xlab="",ylab="")
map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,fill=T,col='white',mar=rep(0.3,4),add=T,lwd=1)#make a first plot of the mapto define the range of the plot 
cols<-ifelse(inp$s6_fry<=0,"grey",ifelse(inp$s6_fry<100,"red",ifelse(inp$s6_fry<200,"orange",ifelse(inp$s6_fry<500,"green","blue"))))
points(haspclist$LONGITUDE,haspclist$LATITUDE,pch=1,cex=1,col=cols)
#dev.off()
}

plots6ye <- function(inp)
{
haspclist<-read.csv("RB_haspc2017_master_list.csv")    
lonlim<-c(-130,-115)#range(c(obj@lakex,obj@pcx))+c(0.5,-0.5)
latlim<-c(48.4,56.5)#range(c(obj@lakey,obj@pcy))+c(-0.5,0.5)
# lcol<-rep("#99999970",obj@nl)
# llcol=rep('#99999970',obj@nl)

lakepoints<-data.frame(X=haspclist$LONGITUDE,Y=haspclist$LATITUDE)
EID=seq(1:nrow(haspclist))


#jpeg("img_ye.jpg",width=9,height=6,units="in",res=600)
plot(lonlim,latlim,col="white",axes=F,xlab="",ylab="")
map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,fill=T,col='white',mar=rep(0.3,4),add=T,lwd=1)#make a first plot of the mapto define the range of the plot 
cols<-ifelse(inp$s6_ye<=0,"grey",ifelse(inp$s6_ye<100,"red",ifelse(inp$s6_ye<200,"orange",ifelse(s6_ye<500,"green","blue"))))
points(haspclist$LONGITUDE,haspclist$LATITUDE,pch=1,cex=1,col=cols)

}


s <- main()
plots6ye(s)
x11()
plots6fry(s)



#dev.off()

## s1=vLinf*exp(ppt*(map/1000-0.5796408))*(1-exp(-vKye*dt_ye))
## s2=s1/(target-(10*exp(-vK*dt_ye)))
## s6=(s5*10^5)/10^2
