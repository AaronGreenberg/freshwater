library('arules')

library('lubridate')
rm(list=ls())

main <- function(wbid,lwts,kf,sden,strain,ploidy)
{            
#read lakes list
masterlist<-read.csv("RB_tool_master_list.csv")
mainpars_post<-read.csv('model_17post_mainpars.csv')
lakepars_post<-read.csv('model_17post_lakepars.csv')

 #parameters

bet_post<-new('list');
ple_post<-new('list');
stn_post<-new('list')

bet_post[[1]]<-mainpars_post$bet1
bet_post[[2]]<-mainpars_post$bet2
osp_post<-mainpars_post$osp

ple_post[[1]]<-mainpars_post$ple1
ple_post[[2]]<-mainpars_post$ple2
ple_post[[3]]<-0
ppt_post <-	mainpars_post$ppt

stn_post[[1]]<-mainpars_post$stn1
stn_post[[2]]<-mainpars_post$stn2
stn_post[[3]]<-mainpars_post$stn3
stn_post[[4]]<-mainpars_post$stn4
stn_post[[5]]<-mainpars_post$stn5
stn_post[[6]]<-0

yef_post<-mainpars_post$yef



i1=which(masterlist$WATERBODY_IDENTIFIER==wbid)
lake_area<-masterlist$AREA_HA[i1]
assessed<-read.csv('assessed_wbid.csv',colClasses=c("character"))
i2=any(assessed==wbid)
if(i2==TRUE)
{i3=which(assessed==wbid);K<-lakepars_post[,i3];Linf<-lakepars_post[,(91+i3)]} else{K<-mainpars_post[,1];Linf<-mainpars_post[,2]}



L0=round(((lwts/(kf/100000))^(1/3)),0)/10 #converting weight in g to length in cm
in_hatch=read.csv('inhatchery_results.csv')
lwd=as.character(discretize(lwts,"fixed",categories = in_hatch$cat,labels=in_hatch$labs))
ind=which(in_hatch$labs==lwd) #adding error around stocking size
cv=ifelse(length(ind)==1,in_hatch$cv[ind],0.1)
sig=cv*L0
tau=1/(sig*sig)
lfs=ifelse(lwts<=4,1,2)
fy=lfs-1
L0_vec<-rnorm(length(Linf),mean=L0,sd=sig)


#sden<-snum/lake_area #stocking density - total numbers/area
dens0<-(sden*L0^2)/10^5 #density expressed in NL2
drb<-ifelse(lfs==1,dens0-0.0375,dens0-0.2)
dos<-0

#Precipation
map<-masterlist$MAP[i1]/1000-0.5796408

#delta_t calculation #no inputs from gui
if(lfs==1) rda="2010/10/15" else rda="2011/06/01"
eda<-c("2012/6/15","2013/6/15","2014/6/15","2015/6/15","2016/6/15")
dt=vector(length=length(eda))
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
  dt[i]=sum(mm2)/1000

}

Linf_L <- (Linf*exp(ppt_post*map))/(1+bet_post[[lfs]]*drb+osp_post*dos)
K_Lp <- K*exp(yef_post*fy+stn_post[[strain]]+ple_post[[ploidy]])
L_hat<-matrix(data=0,nrow=length(Linf),ncol=length(eda))
## print("silly strain")
## print(strain)
## print(stn_post[[strain]])
## print(K_Lp)

for(i in 1:length(eda))
{
  L_hat[,i] <- L0_vec*exp(-K_Lp*dt[i])+Linf_L*(1-exp(-K_Lp*dt[i]))
}
age=seq(2,6)
colnames(L_hat)<-paste0("age_",age)
## print("Silly idiot")
## print(head(L_hat))
out=(L_hat)
}




main2 <- function(wbid,lwts,kf,sden,strain,ploidy,assessed,in_hatch,masterlist,mainpars_post,lakepars_post)
{            
#read lakes list

#parameters

bet_post<-new('list');ple_post<-new('list');stn_post<-new('list')
bet_post[[1]]<-mainpars_post$bet1
bet_post[[2]]<-mainpars_post$bet2
osp_post<-mainpars_post$osp
ple_post[[1]]<-mainpars_post$ple1
ple_post[[2]]<-mainpars_post$ple2
ple_post[[3]]<-0
ppt_post <-	mainpars_post$ppt

stn_post[[1]]<-mainpars_post$stn1
stn_post[[2]]<-mainpars_post$stn2
stn_post[[3]]<-mainpars_post$stn3
stn_post[[4]]<-mainpars_post$stn4
stn_post[[5]]<-mainpars_post$stn5
stn_post[[6]]<-0
yef_post<-mainpars_post$yef



i1=which(masterlist$WATERBODY_IDENTIFIER==wbid)
lake_area<-masterlist$AREA_HA[i1]

i2=any(assessed==wbid)
if(i2==TRUE)
{i3=which(assessed==wbid);K<-lakepars_post[,i3];Linf<-lakepars_post[,(91+i3)]} else{K<-mainpars_post[,1];Linf<-mainpars_post[,2]}



L0=round(((lwts/(kf/100000))^(1/3)),0)/10 #converting weight in g to length in cm

lwd=as.character(discretize(lwts,"fixed",categories = in_hatch$cat,labels=in_hatch$labs))
ind=which(in_hatch$labs==lwd) #adding error around stocking size
cv=ifelse(length(ind)==1,in_hatch$cv[ind],0.1)
sig=cv*L0
tau=1/(sig*sig)
lfs=ifelse(lwts<=4,1,2)
fy=lfs-1
L0_vec<-rnorm(length(Linf),mean=L0,sd=sig)


#sden<-snum/lake_area #stocking density - total numbers/area
dens0<-(sden*L0^2)/10^5 #density expressed in NL2
drb<-ifelse(lfs==1,dens0-0.0375,dens0-0.2)
dos<-0

#Precipation
map<-masterlist$MAP[i1]/1000-0.5796408

#delta_t calculation #no inputs from gui
if(lfs==1) rda="2010/10/15" else rda="2011/06/01"
eda<-c("2012/6/15","2013/6/15","2014/6/15","2015/6/15","2016/6/15")
dt=vector(length=length(eda))
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
  dt[i]=sum(mm2)/1000

}

Linf_L <- (Linf*exp(ppt_post*map))/(1+bet_post[[lfs]]*drb+osp_post*dos)
K_Lp <- K*exp(yef_post*fy+stn_post[[strain]]+ple_post[[ploidy]])
L_hat<-matrix(data=0,nrow=length(Linf),ncol=length(eda))
for(i in 1:length(eda))
{
  L_hat[,i] <- L0_vec*exp(-K_Lp*dt[i])+Linf_L*(1-exp(-K_Lp*dt[i]))
}
age=seq(2,6)
colnames(L_hat)<-paste0("age_",age)
                                        #figure output
#print(head(L_hat))
    out=(L_hat)
    
}



main3 <-  function(wbid,lwts,kf,target,strain,ploidy,age)
{
    assessed<-read.csv('assessed_wbid.csv',colClasses=c("character"))
    in_hatch=read.csv('inhatchery_results.csv')
    masterlist<-read.csv("RB_tool_master_list.csv")
    mainpars_post<-read.csv('model_17post_mainpars.csv')
    lakepars_post<-read.csv('model_17post_lakepars.csv')
age=age-1 
    
    f <- function(sden,age){mean(main2(wbid,lwts,kf,sden,strain,ploidy,assessed,in_hatch,masterlist,mainpars_post,lakepars_post)[,age])-target}
  z <- tryCatch(
        {
            
            uniroot(f,c(0,1000),age,tol=0.009)
        },
        error=function(cond) {
            message(paste("I have a bad feeling about this. \n"))
            message(paste("Are you sure that you entered a plausible target size?! \n"))
            message(paste("I cannot find a stocking density that works!. \n"))
            message("Here's the original error message:")
            message(paste(cond,"\n"))
            message("Sorry:\n")
            # Choose a return value in case of error
            root=10
            return(list(root=root))
        }
)
   
    

return(z)
}


fig1 <- function(L_hat,wbid,density,target)
{
    masterlist<-read.csv("RB_tool_master_list.csv")
    i1=which(masterlist$WATERBODY_IDENTIFIER==wbid)
    ## print("yo")
    ## print("wbid")
    ## print(wbid)
    means=round(apply(L_hat,2,mean),1)
    sds=round(apply(L_hat,2,sd),2)
    boxplot(L_hat,las=1,xlab="Age",ylab="Length (cm)",ylim=c(0,max(means+8*sds)),frame=FALSE)
    text(1:6, means+5*sds, paste("mu=",means))
    text(1:6, means-5*sds, paste("sd=",sds))
    title(main=paste("Name=: ",masterlist$GAZETTED_NAME[i1], "  WBID:= ", masterlist$WATERBODY_IDENTIFIER[i1]))
    if(!missing(target))
    {
        abline(h=target,col="red")
    }
    legend("topleft",legend=paste("Stocking Density:=",signif(density,4)),bty="n")
#age should be 2 to 6
}

tab1 <- function(L_hat)
{
#table output
    sds=round(apply(L_hat,2,sd),2)
    means=round(apply(L_hat,2,mean),1)
    qs=apply( L_hat , 2 ,quantile , probs=c(0.025,0.25,.5,0.75,0.975),na.rm=TRUE)
    qs <- rbind(qs,means)
    qs <- rbind(qs,sds)
   #print(qs)
}


# plot(c(0,age),c(0,means),xlab="Age",ylab="Length (cm)",ylim=c(10,60),las=1,xlim=c(0.5,5),type='l',lwd=2)
# lines(c(0,age),c(0,qs[2,]),lty=2)
# lines(c(0,age),c(0,qs[3,]),lty=2)
# lines(c(0,age),c(0,qs[1,]),lty=2,col="grey")
# lines(c(0,age),c(0,qs[4,]),lty=2,col="grey")

#myres<-data.frame(age=age,means=means,sds=sds,qs=t(round(qs,1)))
                  
## #Please include summaries shown on tab of the tool                  
## #WBID _ Choose from Creation of drop down menu for region and name of lake
## #Input from tool wbid <- 00209BRID
## wbid="00372KOTR"
## #Stocking size
## #Input from tool lwts - stocking weight in g - 2
## lwts=2
## kf=1.1
## #Stocking density
## #Input from tool sum of numbers stocked - 1000 #stocking density unlike stocking numbers
## sden<-3000


## #For ploidy: 3n=1,Multiple=2,2n=3
## strain=2
## ploidy=3

## #Strain and Ploidy input
## #Input from tool for strain and ploidy
## #Need the following comment:
## #For strains: #Blackwater=1, Carp=2, Fraser Valley = 3, Gerrard=4, Multiple=5, Pennask=6


## s=main(wbid,lwts,kf,sden,strain,ploidy)

## fig1(s,sden)
## tab1(s )

## ## #main 3 does the inverse!



## ## make it so that age 3 has mean size 14 cm
## s2=main3(wbid,lwts,kf,14,strain,ploidy,3)
## s=main(wbid,lwts,kf,s2$root,strain,ploidy)
## tab1(s$L_hat)

## ## make it so that age three has mean 10
## s2=main3(wbid,lwts,kf,10,strain,ploidy,3)
## s=main(wbid,lwts,kf,s2$root,strain,ploidy)
## tab1(s$L_hat)
## fig1(s$L_hat,s2$root,10)

## ## make it so that age two has mean 9

## s2=main3(wbid,lwts,kf,9,strain,ploidy,2)
## s=main(wbid,lwts,kf,s2$root,strain,ploidy)
## tab1(s$L_hat)
## fig1(s$L_hat,s2$root,9)
