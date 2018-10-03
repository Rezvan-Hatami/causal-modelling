rm(list=ls())

##Set working directory according to the filepath where data files are stored
setwd("D:/PhD/write papers/paper 4/Treatment plant project/New project")
getwd()

###Load packages######
library(vegan)
library(car)
library(lattice)
library(ecodist)
library(BiodiversityR)
library(latticeExtra)
library(WhatIf)
library(bnlearn)
#########
library(Hmisc)
library(car)
library(bnlearn)
library(pspearman)
library(Rgraphviz)
library("Rgraphviz")

###Read data########
myrbug<-read.csv("myrbug.csv",header=TRUE,nrows=32)
View(myrbug)
dim(myrbug)
myrenv<-read.csv("myrenv.csv",header=TRUE,nrows=32)
View(myrenv)
diversity<-read.csv("myrdiversity.csv",header=TRUE,nrows=32)

# Ordered community table
vegemite(myrbug,1:32,scale="log") #

## ”€”€”€”€”€Exploratory data analysis”€”€”€”€”€#########
## ”€”€”€”€”€Environmental variables graphs
#€*€*€*€*#nitrogen and phosphate graph #  #*€*€*€*€*
tiff(file="Environmental variables graphs-part1.tif",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mfrow=c(3,2), mar=c(0.1,0.1,0.1,0.1),cex=0.6,cex.axis=0.8,las=1)
par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"tn"],type = "b",col="black",ylim=c(0,3),pch=19,cex=1.5,xlab="Distance(km)",ylab="TN(mg/L)",main="a) Total Nitrogen plotted against spatial position",lwd=3,lty=5)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"tn"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"tn"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"tn"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"no2"],type = "b",ylim=c(0,.1),pch=19,cex=1.5,xlab="Distance(km)",ylab="NO2(mg/L)",main="b) Nitrite plotted against spatial position",lwd=3,lty=5)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"no2"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"no2"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"no2"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"no3"],type = "b",ylim=c(0,2.5),pch=19,cex=1.5,xlab="Distance(km)",ylab="NO3(mg/L)",main="c) Nitrate plotted against spatial position",lwd=3,lty=5)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"no3"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"no3"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"no3"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"nh3"],type = "b",ylim=c(0,0.2),pch=19,cex=1.5,xlab="Distance(km)",ylab="NH3(mg/L)",main="d) Ammonia plotted against spatial position",lwd=3,lty=5)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"nh3"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"nh3"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"nh3"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"tp"],type = "b",ylim=c(0,0.5),pch=19,cex=1.5,xlab="Distance(km)",ylab="TP",main="e) Total Phosphorus plotted against spatial position",lwd=3,lty=5)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"tp"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"tp"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"tp"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"op"],type = "b",ylim=c(0,0.1),pch=19,cex=1.5,xlab="Distance(km)",ylab="Orthoposphate(mg/L)",main="f) Orthophosphate plotted against spatial position",lwd=3,lty=5)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"op"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"op"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"op"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)
dev.off()

#### ph, temperature, conductivity, alkalinity, toc, and chla
tiff(file="Environmental variables graphs-part2.tif",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mfrow=c(3,2), mar=c(0.1,0.1,0.1,0.1),cex=0.6,cex.axis=0.8,las=1)
par(mar=c(5,5,4,2),cex=0.9)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"temp"],type = "b",ylim=c(0,30),pch=19,cex=1.5,xlab="Distance(km)",ylab="Temperature (°C)",main="a) Temperature plotted against spatial position",lwd=3,lty=5)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"temp"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"temp"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"temp"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"ph2"],type = "b",ylim=c(6.5,9),pch=19,cex=1.5,xlab="Distance(km)",ylab="pH",main="b) pH plotted against spatial position",lwd=3,lty=5)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"ph2"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"ph2"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"ph2"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"alk"],type = "b",ylim=c(5,25),pch=19,cex=1.5,xlab="Distance(km)",ylab="Alkalinity (mg/L)",main="c) Alkalinity plotted against spatial position",lwd=3,lty=5)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"alk"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"alk"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"alk"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"chla"],type = "b",ylim=c(0,20),pch=19,cex=1.5,xlab="Distance(km)",ylab="Chlorophyll A (mg/L)",main="d) Chlorophyll A plotted against spatial position",lwd=3,lty=1)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"chla"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"chla"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"chla"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"toc"],type = "b",ylim=c(0,12),pch=19,cex=1.5,xlab="Distance(km)",ylab="TOC(mg/L)",main="e) Total Organic Carbon plotted against spatial position",lwd=3,lty=1)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"toc"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"toc"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"toc"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"cond"],type = "b",ylim=c(20,60),pch=19,cex=1.5,xlab="Distance(km)",ylab="Conductivity (mg/L)",main="f) Conductivity plotted against spatial position",lwd=3,lty=1)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"cond"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"cond"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"cond"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)
dev.off()

### DO, canop, velocity and turbidity
tiff(file="Environmental variables graphs-part3.tif",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mfrow=c(3,2), mar=c(0.1,0.1,0.1,0.1),cex=0.6,cex.axis=0.8,las=1)
par(mar=c(5,5,4,2),cex=0.9)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"vel"],type = "b",ylim=c(0,2),pch=19,cex=1.5,xlab="Distance(km)",ylab="Velocity (m/s)",main="a) Velocity plotted against spatial position",lwd=3,lty=4)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"vel"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"vel"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"vel"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"turb"],type = "b",ylim=c(0,180),pch=19,cex=1.5,xlab="Distance(km)",ylab="Turbidity (NTU)",main="b) Turbidity plotted against spatial position",lwd=3,lty=4)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"turb"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"turb"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"turb"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"sed"],type = "b",ylim=c(0,100),pch=19,cex=1.5,xlab="Distance(km)",ylab="Sediment size (mm)",main="c) Sediment size plotted against spatial position",lwd=3,lty=4)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"sed"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"sed"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"sed"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"canop"],type = "b",ylim=c(0,50),pch=19,cex=1.5,xlab="Distance(km)",ylab="Canopy cover (%)",main="d) Canopy cover plotted against spatial position",lwd=3,lty=4)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"canop"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"canop"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"canop"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"do"],type = "b",ylim=c(4,20),pch=19,cex=1.5,xlab="Distance(km)",ylab="Dissolved Oxygen (mg/L)",main="e) DO plotted against spatial position",lwd=3,lty=4)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"do"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"do"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"do"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"cfpom"],type = "b",ylim=c(0,50),pch=19,cex=1.5,xlab="Distance(km)",ylab="CPOM/ FPOM",main="f) CPOM/FPOM plotted against spatial position",lwd=3,lty=4)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"cfpom"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"cfpom"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"cfpom"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)
dev.off()

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"depth"],type = "b",ylim=c(0,100),pch=19,cex=1.5,xlab="Distance(km)",ylab="Depth",main="f) Depth against spatial position",lwd=3,lty=4)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"depth"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"depth"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"depth"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(myrenv[myrenv$day==1,"dist"],myrenv[myrenv$day==1,"width"],type = "b",ylim=c(0,100),pch=19,cex=1.5,xlab="Distance(km)",ylab="Width",main="f) Width against spatial position",lwd=3,lty=4)
points(myrenv[myrenv$day==92,"dist"],myrenv[myrenv$day==92,"width"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(myrenv[myrenv$day==210,"dist"],myrenv[myrenv$day==210,"width"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(myrenv[myrenv$day==283,"dist"],myrenv[myrenv$day==283,"width"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

###”€”€”€”€”€ PCO (Principal coordinate analysis)”€”€”€”€”€ #######

## PCO ## 
myrbug.BC<-vegdist(sqrt(myrbug)) # Bray-Curtis distance
n<-dim(myrbug)[1]
p<-n-1
myrbug.mds<-cmdscale(myrbug.BC, k = p, eig = TRUE, add = TRUE, x.ret = FALSE)# PCoA
pco.varpercent<-round(myrbug.mds$eig/sum(myrbug.mds$eig)*100,digits=1) # Percentage of variation explained by each successive PCO axis
round(cumsum(myrbug.mds$eig)/sum(myrbug.mds$eig)*100,digits=2)
edit(myrbug.mds$points)
pmyrbug<-myrbug.mds$points
dim(pmyrbug)
colnames(pmyrbug)<-c(paste("pco",sep="",1:p))
edit(pmyrbug)

## Diagnostics for PCO
lambda<-myrbug.mds$eig # eigenvalues
D=as.matrix(vegdist(sqrt(myrbug),diag=TRUE,upper=TRUE)) # symmetric BC dissimilarity matrix
n=dim(D)[1] # sample size
p=n-1 # no of PCO axes
nmax=20 #max no. of axes to plot
Y=sqrt(myrbug)
## Percentage of variation explained by each successive PCO axis:
round ( 100*lambda/sum(lambda) , digits=2)

###”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€
## Scree plot with expectations for eigenvalues under the broken stick model
plot(1:n, 100*lambda/sum(lambda), type="b", xlab="PCO axis number",
     ylab="Percent variation explained", pch=19, las=1,
     main="a) Broken stick model", xlim=c(0,nmax))
abline(h=0)
broken.stick=rep(0,n)
for (k in 1:p) broken.stick[k] = sum(1/(k:p))
broken.stick.perc = 100*broken.stick/sum(broken.stick)
points(1:n,broken.stick.perc, type="b", lty="dotted", cex=1.3,
       xlim=c(0,nmax)) # Fig. 4.5a
## Bootstrap eigenvalue method (e.g., see Jackson 1993 ).
nboot = 999
lambda.boot=matrix(rep(0,nboot*n),nrow=nboot, ncol=n, byrow=T)
lambda.perc.boot=matrix(rep(0,nboot*n),nrow=nboot, ncol=n, byrow=T)
for (iboot in 1:nboot) {
  index = sample(1:n, replace=TRUE)
  D.boot = D[index,index]
  # Do the PCO and get eigenvalues from the bootstrap data
  lambda.boot[iboot,1:n]= cmdscale(D.boot, k = p, eig = TRUE, add = TRUE, x.ret = FALSE)$eig
  lambda.perc.boot[iboot,1:n] = 100*lambda.boot[iboot,1:n]/sum(lambda.boot[iboot,1:n])
}
# Get the empirical 95% confidence interval on the bootstrap eigenvalues
low <- function(x) quantile(x,probs=0.025)
high <- function(x) quantile(x,probs=0.975)
lower = apply(lambda.perc.boot, MARGIN = 2, low) 
upper = apply(lambda.perc.boot, MARGIN = 2, high)
centre = apply(lambda.perc.boot, MARGIN = 2, mean) 
# Scree plot with the bootstrap results
plot(1:n, centre, type="b", xlab="PCO axis number",
     ylab="Percent variation explained", las=1, pch=19, ylim = c(0,50),
     main="b) Bootstrap eigenvector method", xlim=c(0,nmax)) # WP replaced "100*lambda/sum(lambda)" with "centre"
abline(0,0)
arrows(1:n,lower,1:n,upper, code=3, length=0.1, angle = 90, xlim=c(0,nmax)) # Fig. 4.5b

## Permutation method (e.g., McCune et al. 2002; Clarke et al. 2008) with
## variation explained by each axis as either:
##    1. a fraction of the total (holistically), or
##    2. a fraction of the variation remaining given prior axes (conditionally)
nperm = 999
lambda.perm=matrix(rep(0,nperm*n),nrow=nperm, ncol=n, byrow=T)
lambda.perc.perm1=matrix(rep(0,nperm*n),nrow=nperm, ncol=n, byrow=T)
lambda.perc.perm2=matrix(rep(0,nperm*n),nrow=nperm, ncol=n, byrow=T)

nvars = dim(Y)[2]   # no. of variables in original Y matrix
for (iperm in 1:nperm) {
  Y.perm = matrix(rep(0,n*nvars), nrow=n, ncol=nvars, byrow=T)
  for (k in 1:nvars) {
    index = sample(1:n, replace=FALSE)
    Y.perm[1:n,k] = Y[index,k] 
  }
  D.perm = as.matrix(vegdist(Y.perm,diag=TRUE,upper=FALSE))
  # Do the PCO and get eigenvalues from the permuted data
  lambda.perm[iperm,1:n]= cmdscale(D.perm, k = p, eig = TRUE, add = TRUE,
                                   x.ret = FALSE)$eig
  lambda.perc.perm1[iperm,1:n] = 
    100*lambda.perm[iperm,1:n]/sum(lambda.perm[iperm,1:n])
  for (k in 1:n) {
    lambda.perc.perm2[iperm,k] = 
      100*lambda.perm[iperm,k]/sum(lambda.perm[iperm,k:n])
  }
}
# Get the empirical 95% confidence interval on the permutation eigenvalues, in
# each case
lower.p1 = apply(lambda.perc.perm1, MARGIN = 2, low) 
upper.p1 = apply(lambda.perc.perm1, MARGIN = 2, high)
middle.p1 = apply(lambda.perc.perm1, MARGIN = 2, median)
lower.p2 = apply(lambda.perc.perm2, MARGIN = 2, low) 
upper.p2 = apply(lambda.perc.perm2, MARGIN = 2, high)
middle.p2 = apply(lambda.perc.perm2, MARGIN = 2, median)

# Scree plot with the permutation results, holistic approach
plot(1:n, 100*lambda/sum(lambda), type="b", xlab="PCO axis number",
     ylab="Percent variation explained", las=1, pch=19,
     ylim = c(0,max(110*lambda/sum(lambda))),xlim=c(0,nmax),
     main="c) Permutation method - holistic approach")
abline(0,0)
points(1:n, middle.p1, type="b", lty="dotted", cex=1.3,xlim=c(0,nmax))
arrows(1:n,lower.p1,1:n,upper.p1, code=3, length=0.1, angle = 90) # Fig. 4.5c

# Scree plot with the permutation results, conditional approach
real=rep(0,n)
for (k in 1:n) {real[k] = 100*lambda[k]/sum(lambda[k:n])}
plot(1:(n-2), real[1:(n-2)], type="b", xlab="PCO axis number",
     ylab="Percent variation explained", las=1, pch=19,
     ylim = c(0,max(120*lambda/sum(lambda))), xlim=c(0,nmax),
     main="d) Permutation method - conditional approach")
abline(0,0)
points(1:(n-2), middle.p2[1:(n-2)], type="b", lty="dotted", cex=1.3)
arrows(1:(n-2),lower.p2[1:(n-2)],1:(n-2),upper.p2[1:(n-2)], code=3, length=0.1,
       angle = 90) # Fig. 4d

#################################################
#*€*€*€*€* Figure 3 in paper #4 *€*€*€*€*#
tiff(file="paper #4-Figure 3.tif",width=6,height=6,units="in",pointsize = 12,bg = "transparent",res=800,compression="lzw") # Save the following graph as a tif image

par(mfrow=c(2,2),mar=c(4,4,2.5,2)+0.1,cex=0.6,cex.axis=0.8,las=1)

# Scree plot with expectations for eigenvalues under the broken stick model
plot(1:n, 100*lambda/sum(lambda), type="b", xlab="PCO axis number",
     ylab="Percent variation explained", pch=19, las=1, xlim=c(0,nmax))
abline(h=0)
broken.stick=rep(0,n)
for (k in 1:p) broken.stick[k] = sum(1/(k:p))
broken.stick.perc = 100*broken.stick/sum(broken.stick)
points(1:n,broken.stick.perc, type="b", lty="dotted", cex=1.3,
       xlim=c(0,nmax))  # Fig. 3a
mtext("a",3,-5,cex=1.8)

# Scree plot with the bootstrap results
plot(1:n, centre, type="b", xlab="PCO axis number",
     ylab="Percent variation explained", las=1, pch=19, ylim = c(0,max(upper)),
     xlim=c(0,nmax)) # WP replaced "100*lambda/sum(lambda)" with "centre"
abline(0,0)
arrows(1:n,lower,1:n,upper, code=3, length=0.1, angle = 90,
       xlim=c(0,nmax)) # Fig. 3b
mtext("b",3,-5,cex=1.8)

# Scree plot with the permutation results, holistic approach
plot(1:n, 100*lambda/sum(lambda), type="b", xlab="PCO axis number",
     ylab="Percent variation explained", las=1, pch=19,
     ylim = c(0,max(110*lambda/sum(lambda))),xlim=c(0,nmax))
abline(0,0)
points(1:n, middle.p1, type="b", lty="dotted", cex=1.3,xlim=c(0,nmax))
arrows(1:n,lower.p1,1:n,upper.p1, code=3, length=0.1, angle = 90) # Fig. 3c
mtext("c",3,-5,cex=1.8)

# Scree plot with the permutation results, conditional approach
real=rep(0,n)
for (k in 1:n) {real[k] = 100*lambda[k]/sum(lambda[k:n])}
plot(1:(n-2), real[1:(n-2)], type="b", xlab="PCO axis number",
     ylab="Percent variation explained", las=1, pch=19,
     ylim = c(0,max(110*lambda/sum(lambda))), xlim=c(0,nmax))
abline(0,0)
points(1:(n-2), middle.p2[1:(n-2)], type="b", lty="dotted", cex=1.3)
arrows(1:(n-2),lower.p2[1:(n-2)],1:(n-2),upper.p2[1:(n-2)], code=3, length=0.1,
       angle = 90) # Fig. 3d
mtext("d",3,-5,cex=1.8)

dev.off()

## Further exploratory analysis 
edit(myrenv)
bugenv<-cbind(myrenv,pmyrbug)# combine environmental data and PCO matrix
edit(bugenv)
dim(bugenv)
edit(pmyrbug)

################ displaying date labels on x-axis for Figure 5
edit(bugenv)
as.character(bugenv$date)
bugenv$date<-as.Date(as.character(bugenv$date), format="%d-%m-%Y")
mydates<-bugenv[bugenv$dist==0,"date"]
mydaterange=c(as.POSIXlt(min(mydates)),as.POSIXlt(max(mydates)))

#*€*€*€*€* discharge and creek flow rate graphs *€*€*€*€*#
####”€”€”€”€”€#discharge and creek flow rate graphs
tiff(file="discharge and creek flow rate graphs.tif",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mfrow=c(3,2), mar=c(0.1,0.1,0.1,0.1),cex=0.6,cex.axis=0.8,las=1)
par(mar=c(5,5,4,2),cex=0.9)

plot(mydates,bugenv[bugenv$dist==0,"dflowm"],type = "p",ylim=c(0,30),pch=19,cex=1.5,xlab="month",ylab="Discharge flow rate (ML/month)",main="Average monthly discharge flow rate plotted against time",lwd=3,lty=2,xaxt="n")
axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")

plot(mydates,myrenv[myrenv$dist==0,"dflow3m"],type = "p",ylim=c(0,100),pch=19,cex=1.5,xlab="month",ylab="Discharge flow rate (ML/month)",main="Average 3 monthly discharge flow rate plotted against time",lwd=3,lty=2,xaxt="n")
axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")

plot(mydates,bugenv[bugenv$dist==0,"rflow"],type = "p",ylim=c(0,70000),pch=19,cex=1.5,xlab="month",ylab="River flow rate (ML/month)",main="Average monthly river flow rate plotted against time",lwd=3,lty=2,xaxt="n",col="blue")
axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")

plot(mydates,bugenv[bugenv$dist==0,"rflow3"],type = "p",ylim=c(0,70000),pch=19,cex=1.5,xlab="month",ylab="River flow rate (ML/month)",main="Average 3 monthly river flow rate plotted against time",lwd=3,lty=2,xaxt="n",col="blue")
axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")

plot(mydates,myrenv[myrenv$dist==0,"weekflow"],type = "p",ylim=c(0,20000),pch=19,cex=1.5,xlab="month",ylab="River flow rate (ML/week)",main="Average weekly river flow rate plotted against time",lwd=3,lty=2,xaxt="n",col="blue")
axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")

plot(mydates,bugenv[bugenv$dist==0,"dayflow"],type = "p",ylim=c(0,10000),pch=19,cex=1.5,xlab="day",ylab="River flow rate (ML/day)",main="Daily river flow rate plotted against time",lwd=3,lty=2,xaxt="n",col="blue")
axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")
dev.off()

####”€”€”€”€”€#rainfall graphs

tiff(file="rainfall graphs.tif",width=12,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mfrow=c(3,1), mar=c(5,5,4,2),cex=1.2,cex.axis=0.8,las=1)

plot(mydates,bugenv[bugenv$dist==0,"rain1"],type = "p",ylim=c(0,50),pch=19,cex=1.5,xlab="month",ylab="Rainfall(mm)",main="Daily rainfall plotted against time",lwd=3,lty=2,xaxt="n",col="blue")
axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")

plot(mydates,bugenv[bugenv$dist==0,"rain2"],type = "p",ylim=c(0,5),pch=19,cex=1.5,xlab="month",ylab="Rainfall(mm)",main="Average monthly rainfall plotted against time",lwd=3,lty=2,xaxt="n",col="blue")
axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")

plot(mydates,bugenv[bugenv$dist==0,"rain3"],type = "p",ylim=c(0,5),pch=19,cex=1.5,xlab="month",ylab="Rainfall(mm)",main="Average 3 monthly rainfall plotted against time",lwd=3,lty=2,xaxt="n",col="blue")
axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")

dev.off()

####”€”€”€”€”€PCO1 plotted against spatial position ”€”€”€”€”€# 
# Spatiotemporal plots
xyplot(pco1~dist,groups=day,data=bugenv,type="b", auto.key=list(space="top", columns=4, title="days", cex.title=1, lines=TRUE, points=FALSE),  main="PCO1 plotted against spatial position", xlab = "Distance(km)", ylab = "PCO1",
       panel = function(...) {
         panel.abline(v = 10.8, lty = 2)
         panel.xyplot(...)
       }) #change response variable 

xyplot(pco2~dist,groups=day,data=bugenv,type="b", auto.key=list(space="top", columns=4, title="days", cex.title=1, lines=TRUE, points=FALSE),  main="PCO2 plotted against spatial position", xlab = "Distance(km)", ylab = "PCO2",
       panel = function(...) {
         panel.abline(v = 10.8, lty = 2)
         panel.xyplot(...)
       }) #change response variable

xyplot(pco3~dist,groups=day,data=bugenv,type="b", auto.key=list(space="top", columns=4, title="days", cex.title=1, lines=TRUE, points=FALSE),  main="PCO3 plotted against spatial position", xlab = "Distance(km)", ylab = "PCO3",
       panel = function(...) {
         panel.abline(v = 10.8, lty = 2)
         panel.xyplot(...)
       }) #change response variable

xyplot(pco4~dist,groups=day,data=bugenv,type="b", auto.key=list(space="top", columns=4, title="days", cex.title=1, lines=TRUE, points=FALSE),  main="PCO4 plotted against spatial position", xlab = "Distance(km)", ylab = "PCO4",
       panel = function(...) {
         panel.abline(v = 10.8, lty = 2)
         panel.xyplot(...)
       }) #change response variable

xyplot(pco5~dist,groups=day,data=bugenv,type="b", auto.key=list(space="top", columns=4, title="days", cex.title=1, lines=TRUE, points=FALSE),  main="PCO5 plotted against spatial position", xlab = "Distance(km)", ylab = "PCO5",
       panel = function(...) {
         panel.abline(v = 10.8, lty = 2)
         panel.xyplot(...)
       }) #change response variable

xyplot(pco6~dist,groups=day,data=bugenv,type="b", auto.key=list(space="top", columns=4, title="days", cex.title=1, lines=TRUE, points=FALSE),  main="PCO6 plotted against spatial position", xlab = "Distance(km)", ylab = "PCO6",
       panel = function(...) {
         panel.abline(v = 10.8, lty = 2)
         panel.xyplot(...)
       }) #change response variable

xyplot(pco7~dist,groups=day,data=bugenv,type="b", auto.key=list(space="top", columns=4, title="days", cex.title=1, lines=TRUE, points=FALSE),  main="PCO7 plotted against spatial position", xlab = "Distance(km)", ylab = "PCO7",
       panel = function(...) {
         panel.abline(v = 10.8, lty = 2)
         panel.xyplot(...)
       }) #change response variable

xyplot(pco8~dist,groups=day,data=bugenv,type="b", auto.key=list(space="top", columns=4, title="days", cex.title=1, lines=TRUE, points=FALSE),  main="PCO8 plotted against spatial position", xlab = "Distance(km)", ylab = "PCO8",
       panel = function(...) {
         panel.abline(v = 10.8, lty = 2)
         panel.xyplot(...)
       }) #change response variable

xyplot(pco1~day,groups=dist,data=bugenv,type="b", auto.key=list(space="top", columns=5, title="distance(km)", cex.title=1, lines=TRUE, points=FALSE),  main="PCO1 plotted against time", xlab = "days", ylab = "PCO1") #change response variable
xyplot(pco2~day,groups=dist,data=bugenv,type="b", auto.key=list(space="top", columns=5, title="distance(km)", cex.title=1, lines=TRUE, points=FALSE),  main="PCO2 plotted against time", xlab = "days", ylab = "PCO2") #change response variable
xyplot(pco3~day,groups=dist,data=bugenv,type="b", auto.key=list(space="top", columns=5, title="distance(km)", cex.title=1, lines=TRUE, points=FALSE),  main="PCO3 plotted against time", xlab = "days", ylab = "PCO3") #change response variable
xyplot(pco4~day,groups=dist,data=bugenv,type="b", auto.key=list(space="top", columns=5, title="distance(km)", cex.title=1, lines=TRUE, points=FALSE),  main="PCO4 plotted against time", xlab = "days", ylab = "PCO4") #change response variable
xyplot(pco5~day,groups=dist,data=bugenv,type="b", auto.key=list(space="top", columns=5, title="distance(km)", cex.title=1, lines=TRUE, points=FALSE),  main="PCO5 plotted against time", xlab = "days", ylab = "PCO5") #change response variable
xyplot(pco6~day,groups=dist,data=bugenv,type="b", auto.key=list(space="top", columns=5, title="distance(km)", cex.title=1, lines=TRUE, points=FALSE),  main="PCO6 plotted against time", xlab = "days", ylab = "PCO6") #change response variable
xyplot(pco7~day,groups=dist,data=bugenv,type="b", auto.key=list(space="top", columns=5, title="distance(km)", cex.title=1, lines=TRUE, points=FALSE),  main="PCO7 plotted against time", xlab = "days", ylab = "PCO7") #change response variable
dev.off()

####”€”€”€”€”€PCO1 plotted against spatial position ”€”€”€”€”€# 
#*€*€*€*€* Paper4-Figure4 *€*€*€*€*#

tiff(file="Paper4-Figure4.tif",width=12,height=10,units="in",pointsize = 12,bg ="transparent",res=1000,compression="lzw")
par(mfrow=c(3,2), mar=c(3,3,2,1.5),mai = c(0.5, 0.9, 0.3, 0.3),cex=1,cex.axis=0.8,las=1)

##PCO1 against distance
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco1"],type = "b",ylim=c(-.6,.6),pch=19,cex=1.5,xlab="",ylab="PCO1 score",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco1"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco1"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco1"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
#legend("topright",inset=c(0,0),legend=c("Feb", "May", "Sep", "Dec"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=4,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)
grid(26,col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

##PCO2 against distance
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco2"],type = "b",ylim=c(-.6,1),pch=19,cex=1.5,xlab="",ylab="PCO2 score",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco2"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco2"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco2"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
#legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##PCO3 against distance
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco3"],type = "b",ylim=c(-.6,1),pch=19,cex=1.5,xlab="",ylab="PCO3 score",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco3"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco3"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco3"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
#legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##PCO4 against distance
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco4"],type = "b",ylim=c(-.6,1),pch=19,cex=1.5,xlab="",ylab="PCO4 score",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco4"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco4"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco4"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
#legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##PCO5 against distance
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco5"],type = "b",ylim=c(-.6,1),pch=19,cex=1.5,xlab="",ylab="PCO5 score",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco5"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco5"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco5"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
#legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##PCO6 against distance
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco6"],type = "b",ylim=c(-.6,1),pch=19,cex=1.5,xlab="",ylab="PCO5 score",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco6"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco6"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco6"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
#legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

dev.off()

################ displaying date labels on x-axis for Paper4-Figure5
edit(bugenv)
as.character(bugenv$date)
bugenv$date<-as.Date(as.character(bugenv$date), format="%d-%m-%Y")
mydates<-bugenv[bugenv$dist==0,"date"]
mydaterange=c(as.POSIXlt(min(mydates)),as.POSIXlt(max(mydates)))

########”€”€”€”€”€PCOs plotted against time ”€”€”€”€”€  ##*€*€*€*€* Figure 5*€*€*€*€*#
##PCO1 against time
tiff(file="Paper4-Figure5.tif",width=12,height=10,units="in",pointsize = 12,bg ="transparent",res=1000,compression="lzw")
par(mfrow=c(3,2), mar=c(3,3,2,1.5),mai = c(0.5, 0.9, 0.3, 0.3),cex=1,cex.axis=0.8,las=1)

#tiff(file="pco1-time.tif",width=6,height=3.5,units="in",pointsize = 12,bg = "transparent",res=800,compression="lzw") # Save the following graph as a tif image
plot(mydates,bugenv[bugenv$dist==0,"pco1"],type = "b",ylim=c(-.8,0.8),pch=19,cex=1,xlab="",ylab="PCO1",main="",lwd=3,lty=2,cex.main=1,xaxt="n")
points(mydates,bugenv[bugenv$dist==8.7,"pco1"],type="b",col="lightseagreen",pch=15,lwd=3,cex=1.5,lty=3)
points(mydates,bugenv[bugenv$dist==10.4,"pco1"],type="b",col="blue",pch=8,cex=1,lwd=3,lty=4)
points(mydates,bugenv[bugenv$dist==10.77,"pco1"],type="b",col="green",pch=17,cex=1,lwd=3,lty=5)
points(mydates,bugenv[bugenv$dist==11.03,"pco1"],type="b",col="red",pch=18,cex=1.5,lwd=3,lty=6)
points(mydates,bugenv[bugenv$dist==11.53,"pco1"],type="b",col="gold2",pch=18,cex=1.5,lwd=3,lty=7)
points(mydates,bugenv[bugenv$dist==15.13,"pco1"],type="b",col="deep pink",pch=18,cex=1.5,lwd=3,lty=6)
points(mydates,bugenv[bugenv$dist==21.23,"pco1"],type="b",col="sienna2",pch=18,cex=1.5,lwd=3,lty=6)
labels<-axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")
#legend("bottomright",inset=c(0,0),legend=levels(as.factor(bugenv$dist)),lty=1,lwd=2,col=c("black","deep pink","blue","green","red","gold2","lightseagreen","sienna2"),ncol=4,horiz=FALSE,cex=0.6,title="distance(km)")
#dev.off()

##PCO2 against time
#tiff(file="pco2-time.tif",width=6,height=3.5,units="in",pointsize = 12,bg = "transparent",res=800,compression="lzw") # Save the following graph as a tif image
plot(mydates,bugenv[bugenv$dist==0,"pco2"],type = "b",ylim=c(-.8,.8),pch=19,cex=1,xlab="",ylab="PCO2",main="",lwd=3,lty=2,cex.main=1,xaxt="n")
points(mydates,bugenv[bugenv$dist==8.7,"pco2"],type="b",col="deep pink",pch=15,lwd=3,cex=1.5,lty=3)
points(mydates,bugenv[bugenv$dist==10.4,"pco2"],type="b",col="blue",pch=8,cex=1,lwd=3,lty=4)
points(mydates,bugenv[bugenv$dist==10.77,"pco2"],type="b",col="green",pch=17,cex=1,lwd=3,lty=5)
points(mydates,bugenv[bugenv$dist==11.03,"pco2"],type="b",col="red",pch=18,cex=1.5,lwd=3,lty=6)
points(mydates,bugenv[bugenv$dist==11.53,"pco2"],type="b",col="gold2",pch=18,cex=1.5,lwd=3,lty=7)
points(mydates,bugenv[bugenv$dist==15.13,"pco2"],type="b",col="lightseagreen",pch=18,cex=1.5,lwd=3,lty=6)
points(mydates,bugenv[bugenv$dist==21.23,"pco2"],type="b",col="sienna2",pch=18,cex=1.5,lwd=3,lty=6)
labels<-axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")
#legend("bottomright",inset=c(0,0),legend=levels(as.factor(bugenv$dist)),lty=1,lwd=2,col=c("black","deep pink","blue","green","red","gold2","lightseagreen","sienna2"),ncol=4,horiz=FALSE,cex=0.6,title="distance(km)")
#dev.off()

##PCO3 against time
#tiff(file="pco3-time.tif",width=6,height=3.5,units="in",pointsize = 12,bg = "transparent",res=800,compression="lzw") # Save the following graph as a tif image
plot(mydates,bugenv[bugenv$dist==0,"pco3"],type = "b",ylim=c(-.8,.8),pch=19,cex=1,xlab="",ylab="PCO3",main="",lwd=3,lty=2,cex.main=1,xaxt="n")
points(mydates,bugenv[bugenv$dist==8.7,"pco3"],type="b",col="deep pink",pch=15,lwd=3,cex=1.5,lty=3)
points(mydates,bugenv[bugenv$dist==10.4,"pco3"],type="b",col="blue",pch=8,cex=1,lwd=3,lty=4)
points(mydates,bugenv[bugenv$dist==10.77,"pco3"],type="b",col="green",pch=17,cex=1,lwd=3,lty=5)
points(mydates,bugenv[bugenv$dist==11.03,"pco3"],type="b",col="red",pch=18,cex=1.5,lwd=3,lty=6)
points(mydates,bugenv[bugenv$dist==11.53,"pco3"],type="b",col="gold2",pch=18,cex=1.5,lwd=3,lty=7)
points(mydates,bugenv[bugenv$dist==15.13,"pco3"],type="b",col="lightseagreen",pch=18,cex=1.5,lwd=3,lty=6)
points(mydates,bugenv[bugenv$dist==21.23,"pco3"],type="b",col="sienna2",pch=18,cex=1.5,lwd=3,lty=6)
labels<-axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")
#legend("bottomright",inset=c(0,0),legend=levels(as.factor(bugenv$dist)),lty=1,lwd=2,col=c("black","deep pink","blue","green","red","gold2","lightseagreen","sienna2"),ncol=4,horiz=FALSE,cex=0.6,title="distance(km)")
#dev.off()

##PCO4 against time
#tiff(file="pco4-time.tif",width=6,height=3.5,units="in",pointsize = 12,bg = "transparent",res=800,compression="lzw") # Save the following graph as a tif image
plot(mydates,bugenv[bugenv$dist==0,"pco4"],type = "b",ylim=c(-.8,.8),pch=19,cex=1,xlab="",ylab="PCO4",main="",lwd=3,lty=2,cex.main=1,xaxt="n")
points(mydates,bugenv[bugenv$dist==8.7,"pco4"],type="b",col="deep pink",pch=15,lwd=3,cex=1.5,lty=3)
points(mydates,bugenv[bugenv$dist==10.4,"pco4"],type="b",col="blue",pch=8,cex=1,lwd=3,lty=4)
points(mydates,bugenv[bugenv$dist==10.77,"pco4"],type="b",col="green",pch=17,cex=1,lwd=3,lty=5)
points(mydates,bugenv[bugenv$dist==11.03,"pco4"],type="b",col="red",pch=18,cex=1.5,lwd=3,lty=6)
points(mydates,bugenv[bugenv$dist==11.53,"pco4"],type="b",col="gold2",pch=18,cex=1.5,lwd=3,lty=7)
points(mydates,bugenv[bugenv$dist==15.13,"pco4"],type="b",col="lightseagreen",pch=18,cex=1.5,lwd=3,lty=6)
points(mydates,bugenv[bugenv$dist==21.23,"pco4"],type="b",col="sienna2",pch=18,cex=1.5,lwd=3,lty=6)
labels<-axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")
#legend("bottomright",inset=c(0,0),legend=levels(as.factor(bugenv$dist)),lty=1,lwd=2,col=c("black","deep pink","blue","green","red","gold2","lightseagreen","sienna2"),ncol=4,horiz=FALSE,cex=0.6,title="distance(km)")
#dev.off()

##PCO5 against time
#tiff(file="pco5-time.tif",width=6,height=3.5,units="in",pointsize = 12,bg = "transparent",res=800,compression="lzw") # Save the following graph as a tif image
plot(mydates,bugenv[bugenv$dist==0,"pco5"],type = "b",ylim=c(-.8,.8),pch=19,cex=1,xlab="",ylab="PCO5",main="",lwd=3,lty=2,cex.main=1,xaxt="n")
points(mydates,bugenv[bugenv$dist==8.7,"pco5"],type="b",col="deep pink",pch=15,lwd=3,cex=1.5,lty=3)
points(mydates,bugenv[bugenv$dist==10.4,"pco5"],type="b",col="blue",pch=8,cex=1,lwd=3,lty=4)
points(mydates,bugenv[bugenv$dist==10.77,"pco5"],type="b",col="green",pch=17,cex=1,lwd=3,lty=5)
points(mydates,bugenv[bugenv$dist==11.03,"pco5"],type="b",col="red",pch=18,cex=1.5,lwd=3,lty=6)
points(mydates,bugenv[bugenv$dist==11.53,"pco5"],type="b",col="gold2",pch=18,cex=1.5,lwd=3,lty=7)
points(mydates,bugenv[bugenv$dist==15.13,"pco5"],type="b",col="lightseagreen",pch=18,cex=1.5,lwd=3,lty=6)
points(mydates,bugenv[bugenv$dist==21.23,"pco5"],type="b",col="sienna2",pch=18,cex=1.5,lwd=3,lty=6)
labels<-axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")
#legend("bottomright",inset=c(0,0),legend=levels(as.factor(bugenv$dist)),lty=1,lwd=2,col=c("black","deep pink","blue","green","red","gold2","lightseagreen","sienna2"),ncol=4,horiz=FALSE,cex=0.6,title="distance(km)")
#dev.off()

##PCO6 against time
#tiff(file="pco6-time.tif",width=6,height=3.5,units="in",pointsize = 12,bg = "transparent",res=800,compression="lzw") # Save the following graph as a tif image
plot(mydates,bugenv[bugenv$dist==0,"pco6"],type = "b",ylim=c(-.8,.8),pch=19,cex=1,xlab="",ylab="PCO6",main="",lwd=3,lty=2,cex.main=1,xaxt="n")
points(mydates,bugenv[bugenv$dist==8.7,"pco6"],type="b",col="deep pink",pch=15,lwd=3,cex=1.5,lty=3)
points(mydates,bugenv[bugenv$dist==10.4,"pco6"],type="b",col="blue",pch=8,cex=1,lwd=3,lty=4)
points(mydates,bugenv[bugenv$dist==10.77,"pco6"],type="b",col="green",pch=17,cex=1,lwd=3,lty=5)
points(mydates,bugenv[bugenv$dist==11.03,"pco6"],type="b",col="red",pch=18,cex=1.5,lwd=3,lty=6)
points(mydates,bugenv[bugenv$dist==11.53,"pco6"],type="b",col="gold2",pch=18,cex=1.5,lwd=3,lty=7)
points(mydates,bugenv[bugenv$dist==15.13,"pco6"],type="b",col="lightseagreen",pch=18,cex=1.5,lwd=3,lty=6)
points(mydates,bugenv[bugenv$dist==21.23,"pco6"],type="b",col="sienna2",pch=18,cex=1.5,lwd=3,lty=6)
labels<-axis.Date(side=1,mydates,at=seq(mydaterange[1],mydaterange[2],by="month"),format="%b-%y")
#legend("bottomright",inset=c(0,0),legend=levels(as.factor(bugenv$dist)),lty=1,lwd=2,col=c("black","deep pink","blue","green","red","gold2","lightseagreen","sienna2"),ncol=4,horiz=FALSE,cex=0.6,title="distance(km)")
dev.off()


#####”€”€”€”€”€causal model 1 (Model building)”€”€”€”€”€##### Modelling macroinvertebrate community data as a function of space and time 
####”€”€”€”€”€ dbRDA 
#####”€”€”€”€”€ Simulations to generate effluent and time vectors and add them to the data frame
myrenv$eff<-ifelse(myrenv$dist<11.03,0,1)
myrenv$buffalo<-ifelse(myrenv$dist<10.4,0,1)
myrenv$creek<-ifelse(myrenv$dist<8.7,0,1)
myrenv$time<-as.factor(myrenv$time)
#sim.pco2<-myrenv$eff

edit(myrenv)
#####”€” to subset times after time 1 can use myrenv$time%in%c("2","3","4")
#####”€” "add=TRUE" in capscale function addsa constant to the distance to avoid negative values.
#myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(pi*day/365):eff+sin(2*pi*day/365):dist+cos(pi*day/365):dist+eff:time+I(dist^2):eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
#myr.cap1<-capscale(formula=myrbug.BC~I(dist^2):eff:time+sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+cos(2*pi*day/365):dist+eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
#myr.cap1<-capscale(formula=myrbug.BC~as.factor(eff)*time*dist,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
#myr.cap1<-capscale(formula=myrbug.BC~as.factor(eff)+time+dist+as.factor(eff):time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
#myr.cap1<-capscale(formula=myrbug.BC~dist+time+dist:time+eff+eff:time+eff:dist+eff:dist:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
#myr.cap1<-capscale(formula=myrbug.BC~dist+time+dist:time+eff+eff:time+eff:dist:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
#myr.cap1<-capscale(formula=myrbug.BC~dist+time+dist:time+eff+eff:time+eff:dist:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
#myr.cap1<-capscale(formula=myrbug.BC~dist+time+dist:time+eff:time+eff:dist:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
#myr.cap1<-capscale(formula=myrbug.BC~eff+time+dist+eff:time+eff:dist+time:dist+eff:dist:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit) # 
#myr.cap1<-capscale(formula=myrbug.BC~dist+time+dist:time+eff+eff:time+eff:dist:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit) 
#myr.cap1<-capscale(formula=myrbug.BC~I(cos(pi*day/365))+dist+I(dist^2)+time+dist:time+I(dist^2):time+I(dist^2),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
#myr.cap1<-capscale(formula=myrbug.BC~I(cos(2*pi*day/365)),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
#myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+eff:time:dist,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
#myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+eff:time:dist,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)

myr.cap1<-capscale(formula=myrbug.BC~time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~dist,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~I(dist^2),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~I(dist^3),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~I(dist^3):time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~I(dist^3):time:eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~I(dist^2):time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~I(dist^2):time:eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)

myr.cap1<-capscale(formula=myrbug.BC~eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~cos(2*pi*day/365),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+time:eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+time:eff:dist,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365)+time:eff:dist,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365)+sin(2*pi*day/365):eff+cos(2*pi*day/365):eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365)+sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365)+sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365)+sin(2*pi*day/365):eff:dist+cos(2*pi*day/365):eff:dist,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365)+sin(2*pi*day/365):eff+cos(2*pi*day/365):eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+sin(2*pi*day/365):eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+sin(2*pi*day/365):eff+sin(2*pi*day/365):dist,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~cos(2*pi*day/365)+cos(2*pi*day/365):eff+cos(2*pi*day/365):dist+I(dist^2):eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###

myr.cap1<-capscale(formula=myrbug.BC~dist+time+I(dist^2)+dist*time+I(time^2)+I(dist^3)+I(dist^2)*time+dist*I(time^2)+I(time^3),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~time+I(dist^2)+dist*time+I(time^2)+I(dist^3)+I(dist^2)*time+dist*I(time^2)+I(time^3),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~time+dist*time+I(time^2)+I(dist^3)+I(dist^2)*time+dist*I(time^2)+I(time^3),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~dist+time+I(dist^2)+dist*time+I(time^2)+I(dist^3)+I(dist^2)*time+dist*I(time^2)+I(time^3),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~time+I(dist^2)+dist*time+I(time^2)+I(dist^3)+I(dist^2)*time+dist*I(time^2)+I(time^3),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~time+dist*time+I(time^2)+I(dist^2)*time+dist*I(time^2)+I(time^3),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~time+I(time^2)+I(time^3),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)## all time significant terms

myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365)+time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/4)+cos(2*pi*day/4)+time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+cos(2*pi*day/365):dist+eff:time+eff:time:dist,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+cos(2*pi*day/365):dist+eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+eff:time+eff:time:dist,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)

myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365)+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+eff:time+eff:time:dist,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###sounds good #1
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365)+cos(2*pi*day/365):dist+eff:time+I(dist^2):eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###sounds good #2
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365)+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+I(dist^2):eff:time+eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###sounds good #3
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+cos(2*pi*day/365)+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+I(dist^2):eff:time+eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###sounds good #5
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+cos(2*pi*day/365)+sin(2*pi*day/365)+cos(2*pi*day/365):dist+sin(2*pi*day/365):dist+I(dist^2):eff:time+eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### 5-1
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+cos(2*pi*day/365)+sin(2*pi*day/365)+cos(2*pi*day/365):dist+sin(2*pi*day/365):dist+I(dist^2)+dist:eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### 5-2
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+cos(2*pi*day/365)+sin(2*pi*day/365)+cos(2*pi*day/365):dist+I(dist^2)+dist:eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### 5-3
myr.cap1<-capscale(formula=myrbug.BC~cos(2*pi*day/365):eff+cos(2*pi*day/365)+sin(2*pi*day/365)+cos(2*pi*day/365):dist+I(dist^2)+dist:eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### 5-4
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365)+sin(2*pi*day/365)+sin(2*pi*day/365):dist+I(dist^2)+dist:eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### 5-5
myr.cap1<-capscale(formula=myrbug.BC~cos(2*pi*day/365):eff+cos(2*pi*day/365)+sin(2*pi*day/365)+sin(2*pi*day/365):dist+I(dist^2)+dist:eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### 5-6

myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+I(dist^2):eff:time+eff:dist:time+time+dist+eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###sounds good #4
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+I(dist^2):eff:time+eff:dist:time+time+eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### 4-1
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+I(dist^2):eff:time+eff:dist:time+time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### 4-2
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+I(dist^2):eff:time+eff:dist:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### 4-3
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+cos(2*pi*day/365):dist+sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+I(dist^2):eff:time+eff:dist:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###  4-4
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):dist+sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+I(dist^2):eff:time+eff:dist:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### 4-5

myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+I(dist^2):eff:time+eff:dist:time+eff+I(time==2),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### the best so far #6
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+I(dist^2):eff:time+eff:dist:time+eff+I(time==2)+I(dist==8.7)+I(dist==10.4)+I(dist==10.77)+I(dist==11.03),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### the best so far #6
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### the best so far #6
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+sin(2*pi*day/365)+cos(2*pi*day/365)+eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### the best so far #6
myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+I(dist^2):eff:time+eff:dist:time+eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### 6-1  ## without sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist
myr.cap1<-capscale(formula=myrbug.BC~time+eff+time:eff+time:eff:dist+I(dist==8.7):time+I(dist==10.4):time+I(dist==10.77):time+I(dist==11.03):eff+I(dist==15.13):time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### the best ever
myr.cap1<-capscale(formula=myrbug.BC~time+eff+time:eff+time:eff:dist+I(dist==8.7):time+I(dist==10.4):time+I(dist==10.77):time+I(dist==11.03):eff+I(dist==15.13):time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~time+eff+time:eff+time:eff:dist+I(dist==8.7):time+I(dist==10.4):time+I(dist==10.77):time+I(dist==11.03):eff+I(dist==15.13):time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~time+time:eff+time:eff:dist+I(dist==8.7):time+I(dist==10.4):time+I(dist==10.77):time+I(dist==11.03):eff+I(dist==15.13):time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~time+time:eff+I(dist==8.7):time+I(dist==10.4):time+I(dist==10.77):time+I(dist==11.03):eff:time+I(dist==15.13):time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~time+time:eff+I(dist==8.7):time+I(dist==10.77):time+I(dist==11.03):eff:time+I(dist==15.13):time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~time+time:eff+I(dist==8.7):time+I(dist==10.4):time+I(dist==11.03):eff:time+I(dist==15.13):time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### very good
myr.cap1<-capscale(formula=myrbug.BC~time+I(dist==11.03):eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### significant in very good
myr.cap1<-capscale(formula=myrbug.BC~time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time+eff:dist+eff:dist:time+eff:I(dist^2):time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### good with buffalo terms
myr.cap1<-capscale(formula=myrbug.BC~time+buffalo:I(dist^2):time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~time+I(dist==11.03):eff:I(time==1)+I(dist==11.03):eff:I(time==2)+I(dist==11.03):eff:I(time==3),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~time+I(dist==11.03):eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~time+I(dist==11.03):eff:time+creek:time:I(dist==8.7)+buffalo:time:I(dist==10.4),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~time+I(dist==11.03):eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)###
myr.cap1<-capscale(formula=myrbug.BC~time+time:dist:eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)##

summary(myr.cap1) 
myr.anova1<-anova(myr.cap1,by="term",permutations = how(nperm=9999))
print(myr.anova1)  #*€*€*€*€* *€*€*€*€*#  (ANOVA table for dbRDA model with space and time as predictors)

myr.cap1b<-capscale(formula=myrbug.BC~time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
summary(myr.cap1b)
myr.anova1b<-anova(myr.cap1b,by="term")
print(myr.anova1b) #*€*€*€*€*€*€*€*€*# Spatiotemporal model without effluent terms
anova(myr.cap1b,myr.cap1,permutations = how(nperm=9999))

#*€*€*€*€* note that the capscale function doesn't give the fitted and residuals foR PCOs.
#*€*€*€*€* Instead, the function "fitted" and "residual" for capscale give the constrained and unconstrained parts of the ordination.
#*€*€*€*€* Calculate fitted and residual PCOs *€*€*€*€*#

b<-model.matrix(~-1+time+time:eff+I(dist==8.7):time+I(dist==10.4):time+I(dist==11.03):eff:time+I(dist==15.13):time,data=myrenv)
design<-scale(model.matrix(~-1+time+time:as.factor(eff)+I(dist==8.7):time+I(dist==10.4):time+I(dist==11.03):as.factor(eff):time+I(dist==15.13):time,data=myrenv),center=FALSE,scale=FALSE) 
edit(design)
edit(b)

b<-model.matrix(~-1+time+I(dist==11.03):eff:time,data=myrenv)
design<-scale(model.matrix(~-1+time+I(dist==11.03):eff:time,data=myrenv),center=FALSE,scale=FALSE) 
edit(design)
edit(b)

b<-model.matrix(~-1+time+I(dist==11.03):eff,data=myrenv)
design<-scale(model.matrix(~-1+time+I(dist==11.03):eff,data=myrenv),center=FALSE,scale=FALSE) 
edit(design)
edit(b)

b<-model.matrix(~-1+time+I(dist==11.03):eff:I(time==1)+I(dist==11.03):eff:I(time==2)+I(dist==11.03):eff:I(time==3),data=myrenv)
design<-scale(model.matrix(~-1+time+I(dist==11.03):eff:I(time==1)+I(dist==11.03):eff:I(time==2)+I(dist==11.03):eff:I(time==3),data=myrenv),center=FALSE,scale=FALSE) 
edit(design)
edit(b)

b<-model.matrix(~-1+time+I(dist==11.03):eff:I(time==2),data=myrenv)
design<-scale(model.matrix(~-1+time+I(dist==11.03):eff:I(time==2),data=myrenv),center=FALSE,scale=FALSE) 
edit(design)
edit(b)

b<-model.matrix(~-1+time,data=myrenv)
design<-scale(model.matrix(~-1+time,data=myrenv),center=FALSE,scale=FALSE) 
edit(design)
edit(b)

b<-model.matrix(~-1+time+I(dist==11.03):eff:time,data=myrenv)
design<-scale(model.matrix(~-1+time+I(dist==11.03):eff:time,data=myrenv),center=FALSE,scale=FALSE) 
edit(design)
edit(b)

b<-model.matrix(~-1+time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time+eff:dist+eff:dist:time+eff:I(dist^2):time,data=myrenv)
design<-scale(model.matrix(~-1+time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time+eff:dist+eff:dist:time+eff:I(dist^2):time,data=myrenv),center=FALSE,scale=FALSE) 
edit(design)
edit(b)

b<-model.matrix(~-1+time+eff+time:eff+time:eff:dist+I(dist==8.7):time+I(dist==10.4):time+I(dist==10.77):time+I(dist==11.03):eff+I(dist==15.13):time,data=myrenv)
design<-scale(model.matrix(~-1+time+eff+time:eff+time:eff:dist+I(dist==8.7):time+I(dist==10.4):time+I(dist==10.77):time+I(dist==11.03):eff+I(dist==15.13):time,data=myrenv),center=FALSE,scale=FALSE) 
edit(design)
edit(b)

b<-model.matrix(~-1+time:eff:dist,data=myrenv)
design<-scale(model.matrix(~-1+time:eff:dist,data=myrenv),center=FALSE,scale=FALSE) 
edit(design)
edit(b)

#*€*€*€*€*# model.matrix creates a design matrix
#*€*€*€*€*# scale: is a generic function whose default method centers or scales the columns of a matrix.

pco.beta<-qr.coef(qr(design),pmyrbug)
#edit(pco.beta)
pco.predict<-qr.fitted(qr(design),pmyrbug)
pco.resid<-pmyrbug-pco.predict # Compute PCO residuals
dim(pco.resid)
colnames(pco.resid)<-c(paste("res",sep="",1:p))
colnames(pco.predict)<-c(paste("pred",sep="",1:p))
envpcores<-cbind(myrenv,pco.resid)
envpcopred<-cbind(myrenv,pco.predict)

##compute AIC and BIC
sum(diag(var(pmyrbug)))*p
sum(diag(var(pco.predict)))*p
100*(sum(diag(var(pco.predict)))*p)/(sum(diag(var(pmyrbug)))*p)
SS.res<-sum(diag(var(pco.resid)))*p
SS.res
AIC<-n*log(SS.res/n)+2*dim(design)[2]
AIC# 
BIC<-n*log(SS.res/n)+log(n)*dim(design)[2]
BIC

#edit(pco.resid)
#edit(envpcopred)
alldata<-cbind(envpcopred,pmyrbug)#

####plot predicted and residual values vs. distance and time
##pred1
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco1"],type = "p",ylim=c(-.6,.6),pch=19,cex=1.5,xlab="Distance(km)",ylab="Pred1 score",main="Predicted values plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco1"],type="p",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco1"],type="p",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco1"],type="p",col="red",pch=17,cex=1.5,lwd=3,lty=2)

points(alldata[alldata$day==1,"dist"],alldata[alldata$day==1,"pred1"],type = "l",ylim=c(-.6,.6),pch=19,cex=1.5,xlab="Distance(km)",ylab="Pred1 score",main="Predicted values plotted against spatial position",lwd=3,lty=5)
points(alldata[alldata$day==92,"dist"],alldata[alldata$day==92,"pred1"],type="l",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(alldata[alldata$day==210,"dist"],alldata[alldata$day==210,"pred1"],type="l",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(alldata[alldata$day==283,"dist"],alldata[alldata$day==283,"pred1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##pred2
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco2"],type = "p",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO1 score",main="predicted values plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco2"],type="p",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco2"],type="p",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco2"],type="p",col="red",pch=17,cex=1.5,lwd=3,lty=2)

points(alldata[alldata$day==1,"dist"],alldata[alldata$day==1,"pred2"],type = "l",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO1 score",main="predicted values plotted against spatial position",lwd=3,lty=5)
points(alldata[alldata$day==92,"dist"],alldata[alldata$day==92,"pred2"],type="l",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(alldata[alldata$day==210,"dist"],alldata[alldata$day==210,"pred2"],type="l",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(alldata[alldata$day==283,"dist"],alldata[alldata$day==283,"pred2"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##res1
par(mar=c(5,5,4,2),cex=0.9)
plot(envpcores[envpcores$day==1,"dist"],envpcores[envpcores$day==1,"res1"],type = "l",ylim=c(-.6,0.6),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO1 score",main="Residuals plotted against spatial position",lwd=3,lty=5)
points(envpcores[envpcores$day==92,"dist"],envpcores[envpcores$day==92,"res1"],type="l",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(envpcores[envpcores$day==210,"dist"],envpcores[envpcores$day==210,"res1"],type="l",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(envpcores[envpcores$day==283,"dist"],envpcores[envpcores$day==283,"res1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##res2
par(mar=c(5,5,4,2),cex=0.9)
plot(envpcores[envpcores$day==1,"dist"],envpcores[envpcores$day==1,"res2"],type = "l",ylim=c(-.6,0.6),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO1 score",main="Residuals plotted against spatial position",lwd=3,lty=5)
points(envpcores[envpcores$day==92,"dist"],envpcores[envpcores$day==92,"res2"],type="l",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(envpcores[envpcores$day==210,"dist"],envpcores[envpcores$day==210,"res2"],type="l",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(envpcores[envpcores$day==283,"dist"],envpcores[envpcores$day==283,"res2"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##”€”€”€”€”€ PCO1, pco2 and pred1, pred2 ”€”€”€”€”€##
tiff(file="PCO1, pco2 and pred1, pred2.tif",width=16,height=12,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mfrow=c(2,2), mar=c(0.1,0.1,0.1,0.1),cex=0.6,cex.axis=0.8,las=1)

##PCO1 against distance
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco1"],type = "b",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO1 score",main="PCO1 plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco1"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco1"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco1"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##PCO2 against distance
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco2"],type = "b",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO2 score",main="PCO2 plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco2"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco2"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco2"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco1"],type = "p",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="Pred1 score",main="Predicted values plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco1"],type="p",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco1"],type="p",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco1"],type="p",col="red",pch=17,cex=1.5,lwd=3,lty=2)

points(alldata[alldata$day==1,"dist"],alldata[alldata$day==1,"pred1"],type = "l",ylim=c(-.6,.6),pch=19,cex=1.5,xlab="Distance(km)",ylab="Pred1 score",main="Predicted values (pred1) plotted against spatial position",lwd=3,lty=5)
points(alldata[alldata$day==92,"dist"],alldata[alldata$day==92,"pred1"],type="l",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(alldata[alldata$day==210,"dist"],alldata[alldata$day==210,"pred1"],type="l",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(alldata[alldata$day==283,"dist"],alldata[alldata$day==283,"pred1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##pred2
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco2"],type = "p",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO1 score",main="predicted values (pred1) plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco2"],type="p",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco2"],type="p",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco2"],type="p",col="red",pch=17,cex=1.5,lwd=3,lty=2)

points(alldata[alldata$day==1,"dist"],alldata[alldata$day==1,"pred2"],type = "l",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="Pred2 score",main="predicted values plotted against spatial position",lwd=3,lty=5)
points(alldata[alldata$day==92,"dist"],alldata[alldata$day==92,"pred2"],type="l",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(alldata[alldata$day==210,"dist"],alldata[alldata$day==210,"pred2"],type="l",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(alldata[alldata$day==283,"dist"],alldata[alldata$day==283,"pred2"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)
dev.off()


##”€”€”€”€”€ PCO1, pco2 and pred1, pred2 for formula= time”€”€”€”€”€##

b<-model.matrix(~-1+time,data=myrenv)
design<-scale(model.matrix(~-1+time,data=myrenv),center=FALSE,scale=FALSE) 
design<-scale(model.matrix(~-1+time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time+eff:dist+eff:dist:time+eff:I(dist^2):time,data=myrenv),center=FALSE,scale=FALSE) 

edit(design)
edit(b)

pco.beta<-qr.coef(qr(design),pmyrbug)
#edit(pco.beta)
pco.predict<-qr.fitted(qr(design),pmyrbug)
pco.resid<-pmyrbug-pco.predict # Compute PCO residuals
dim(pco.resid)
colnames(pco.resid)<-c(paste("res",sep="",1:p))
colnames(pco.predict)<-c(paste("pred",sep="",1:p))
envpcores<-cbind(myrenv,pco.resid)
envpcopred<-cbind(myrenv,pco.predict)

#edit(pco.resid)
#edit(envpcopred)
alldata<-cbind(envpcopred,pmyrbug)#

tiff(file="Temporal variation of PCOs.tif",width=16,height=12,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mfrow=c(2,2), mar=c(0.1,0.1,0.1,0.1),cex=0.6,cex.axis=0.8,las=1)

##PCO1 against distance
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco1"],type = "b",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO1 score",main="PCO1 plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco1"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco1"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco1"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##PCO2 against distance
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco2"],type = "b",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO2 score",main="PCO2 plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco2"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco2"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco2"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##pred1
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco1"],type = "p",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="Pred1 score",main="Predicted values plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco1"],type="p",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco1"],type="p",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco1"],type="p",col="red",pch=17,cex=1.5,lwd=3,lty=2)

points(alldata[alldata$day==1,"dist"],alldata[alldata$day==1,"pred1"],type = "l",ylim=c(-.6,.6),pch=19,cex=1.5,xlab="Distance(km)",ylab="Pred1 score",main="Predicted values (pred1) plotted against spatial position",lwd=3,lty=5)
points(alldata[alldata$day==92,"dist"],alldata[alldata$day==92,"pred1"],type="l",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(alldata[alldata$day==210,"dist"],alldata[alldata$day==210,"pred1"],type="l",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(alldata[alldata$day==283,"dist"],alldata[alldata$day==283,"pred1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##pred2
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco2"],type = "p",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO1 score",main="predicted values (pred1) plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco2"],type="p",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco2"],type="p",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco2"],type="p",col="red",pch=17,cex=1.5,lwd=3,lty=2)

points(alldata[alldata$day==1,"dist"],alldata[alldata$day==1,"pred2"],type = "l",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="Pred2 score",main="predicted values plotted against spatial position",lwd=3,lty=5)
points(alldata[alldata$day==92,"dist"],alldata[alldata$day==92,"pred2"],type="l",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(alldata[alldata$day==210,"dist"],alldata[alldata$day==210,"pred2"],type="l",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(alldata[alldata$day==283,"dist"],alldata[alldata$day==283,"pred2"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)
dev.off()

##”€”€”€”€”€ PCO1, pco2 and pred1, pred2 for formular time+I(dist==11.03):eff”€”€”€”€”€##

b<-model.matrix(~-1+time+I(dist==11.03):eff:time,data=myrenv)
design<-scale(model.matrix(~-1+time+I(dist==11.03):eff:time,data=myrenv),center=FALSE,scale=FALSE) 
edit(design)
edit(b)

pco.beta<-qr.coef(qr(design),pmyrbug)
#edit(pco.beta)
pco.predict<-qr.fitted(qr(design),pmyrbug)
pco.resid<-pmyrbug-pco.predict # Compute PCO residuals
dim(pco.resid)
colnames(pco.resid)<-c(paste("res",sep="",1:p))
colnames(pco.predict)<-c(paste("pred",sep="",1:p))
envpcores<-cbind(myrenv,pco.resid)
envpcopred<-cbind(myrenv,pco.predict)

#edit(pco.resid)
#edit(envpcopred)
alldata<-cbind(envpcopred,pmyrbug)#

tiff(file="spatiotemporal variations of PCO1, pco2 and pred1, pred2",width=16,height=12,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mfrow=c(2,2), mar=c(0.1,0.1,0.1,0.1),cex=0.6,cex.axis=0.8,las=1)

##PCO1 against distance
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco1"],type = "b",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO1 score",main="PCO1 plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco1"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco1"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco1"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##PCO2 against distance
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco2"],type = "b",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO2 score",main="PCO2 plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco2"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco2"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco2"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##pred1
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco1"],type = "p",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="Pred1 score",main="Predicted values plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco1"],type="p",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco1"],type="p",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco1"],type="p",col="red",pch=17,cex=1.5,lwd=3,lty=2)

points(alldata[alldata$day==1,"dist"],alldata[alldata$day==1,"pred1"],type = "l",ylim=c(-.6,.6),pch=19,cex=1.5,xlab="Distance(km)",ylab="Pred1 score",main="Predicted values (pred1) plotted against spatial position",lwd=3,lty=5)
points(alldata[alldata$day==92,"dist"],alldata[alldata$day==92,"pred1"],type="l",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(alldata[alldata$day==210,"dist"],alldata[alldata$day==210,"pred1"],type="l",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(alldata[alldata$day==283,"dist"],alldata[alldata$day==283,"pred1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

##pred2
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco2"],type = "p",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO1 score",main="predicted values (pred1) plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco2"],type="p",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco2"],type="p",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco2"],type="p",col="red",pch=17,cex=1.5,lwd=3,lty=2)

points(alldata[alldata$day==1,"dist"],alldata[alldata$day==1,"pred2"],type = "l",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="Pred2 score",main="predicted values plotted against spatial position",lwd=3,lty=5)
points(alldata[alldata$day==92,"dist"],alldata[alldata$day==92,"pred2"],type="l",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(alldata[alldata$day==210,"dist"],alldata[alldata$day==210,"pred2"],type="l",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(alldata[alldata$day==283,"dist"],alldata[alldata$day==283,"pred2"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)
dev.off()

##”€”€”€”€”€ Plot predictions
max(myrenv$dist)

#Generate predictions
xygrid<-expand.grid(seq(1,4,1),seq(0,21.23,0.1))
#xygrid<-expand.grid(seq(1,4,1))
#colnames(xygrid)<-c("time")
colnames(xygrid)<-c("time","dist")
edit(xygrid)

#days are 1,92,210,283
for (i in 1:dim(xygrid)[1]) {
  if (xygrid$time[i]==1) xygrid$day[i]<-1
  if (xygrid$time[i]==2) xygrid$day[i]<-92
  if (xygrid$time[i]==3) xygrid$day[i]<-210
  if (xygrid$time[i]==4) xygrid$day[i]<-283
}

#xygrid$eff<-as.factor(ifelse(xygrid$dist<4,0,1))
xygrid$eff<-ifelse(xygrid$dist<11.03,0,1)
xygrid$time<-as.factor(xygrid$time)
colnames(xygrid)<-c("time","dist","day","eff")
colnames(xygrid)<-c("time","dist")
colnames(xygrid)<-c("time")
#xygrid$time<-as.factor(xygrid$time)

edit(xygrid) #use xygrid as data for design
#design2<-scale(model.matrix(~-1+sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+I(dist^2):eff:time+eff:dist:time+eff,data=xygrid),center=FALSE,scale=FALSE)
#design2<-scale(model.matrix(~-1+dist+time+dist:time+eff+eff:time+eff:dist:time,data=xygrid),center=FALSE,scale=FALSE)#
#design2<-scale(model.matrix(~-1+as.factor(eff)+time+dist+as.factor(eff):time,data=xygrid),center=FALSE,scale=FALSE)  ## construct centred desin matrix
#design2<-scale(model.matrix(~-1+dist+time+dist:time+as.factor(eff)+as.factor(eff):time+as.factor(eff):dist:time,data=xygrid),center=FALSE,scale=FALSE) 
#design2<-scale(model.matrix(~-1+sin(2*pi*day/365)+cos(2*pi*day/365)+cos(2*pi*day/365):dist+eff:time+eff:dist:time,data=xygrid),center=TRUE,scale=FALSE)
#design2<-scale(model.matrix(~-1+sin(2*pi*day/365)+cos(2*pi*day/365)+time,data=xygrid),center=TRUE,scale=FALSE)
#design2<-scale(model.matrix(~-1+sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+eff:time:dist,data=xygrid),center=TRUE,scale=FALSE)
#design2<-scale(model.matrix(~-1+sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+I(dist^2):eff:time+eff:dist:time+eff,data=xygrid),center=TRUE,scale=FALSE)
design2<-scale(model.matrix(~-1+time+I(dist==11.03):eff:time,data=xygrid),center=FALSE,scale=FALSE)
design2<-scale(model.matrix(~-1+time+time:eff+I(dist==8.7):time+I(dist==10.4):time+I(dist==11.03):eff:time+I(dist==15.13):time,data=xygrid),center=FALSE,scale=FALSE)
design2<-scale(model.matrix(~-1+time+I(dist==11.03):eff:time,data=xygrid),center=FALSE,scale=FALSE)
design2<-scale(model.matrix(~-1+time+time:as.factor(eff)+I(dist==8.7):time+I(dist==10.4):time+I(dist==11.03):as.factor(eff):time+I(dist==15.13):time,data=xygrid),center=FALSE,scale=FALSE) 
design2<-scale(model.matrix(~-1+as.factor(time),data=xygrid),center=FALSE,scale=FALSE)
design2<-scale(model.matrix(~-1+time+I(dist==11.03):as.factor:time,data=xygrid),center=FALSE,scale=FALSE)


edit(design2)
dim(design2)
dim(pco.beta)
edit(pco.beta)
pco.predict2<-as.matrix(design2)%*%pco.beta # Compute fitted PCOs
edit(pco.predict2)
colnames(pco.predict2)<-c(paste("pred",sep="",1:p))
dim(xygrid)
dim(pco.predict2)
pco.predict2<-cbind(xygrid,pco.predict2)
edit(pco.predict2)

#”€”€”€”€”#### Pco1 linear model
#”€”€”€”€”#### Pco1 as a function of time, distance, and effluent

tempo<-lm(pco1~time+time:eff+I(dist==8.7):time+I(dist==10.4):time+I(dist==11.03):eff:time+I(dist==15.13):time,data=alldata)
summary(tempo)
anova(tempo)

tempo<-lm(pco1~time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time+eff:dist+eff:dist:time+eff:I(dist^2):time,data=alldata)
summary(tempo)
anova(tempo)

tempo<-lm(pco1~time+time:eff+I(dist==8.7):time+I(dist==10.4):time+I(dist==11.03):eff:time+I(dist==15.13):time,data=alldata)
tempo<-lm(pco1~time+I(dist==11.03):eff:time,data=alldata)
summary(tempo)
anova(tempo)

tempo<-lm(pco2~time+I(dist==11.03):eff:time,data=alldata)
summary(tempo)
anova(tempo)

tempo<-lm(pco1~time,data=alldata)
summary(tempo)

#”€”€”€”€”#### Pco2 as a function of time, distance, and effluent

tempo<-lm(pco2~time+time:eff+I(dist==8.7):time+I(dist==10.4):time+I(dist==11.03):eff:time+I(dist==15.13):time,data=alldata)
summary(tempo)
anova(tempo)

tempo<-lm(pco2~time+I(dist==11.03):eff:time,data=alldata)
summary(tempo)
anova(tempo)

tempo<-lm(pco2~time+I(dist==11.03):eff:time,data=alldata)
summary(tempo)
anova(tempo)

tempo<-lm(pco2~time,data=alldata)
summary(tempo)

#”€”€”€”€”€##pco plots with spatiotemporal model predictions###  #*€*€*€*€* 

tiff(file="pco plots with spatiotemporal model predictions.tif",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mfrow=c(3,2), mar=c(4.5,4.5,2,0.5),cex=1.5,cex.axis=0.7,las=1,cex.main=0.8)

num<-1
#fig.name<-"myrpco1.tif"
pco.no<-paste("pco",num,sep="")
PCO.NO<-paste("PCO",num," (",pco.varpercent[num],"% of total variation)",sep="")
pred.no<-paste("pred",num,sep="")
#tiff(file=fig.name,width=7,height=5,units="in",pointsize = 12,bg = "transparent",res=300,compression="lzw")
#par(mar=c(5,5,4,2),cex=0.9)
plot(alldata$dist[alldata$time==1],alldata[alldata$time==1,pco.no],type="p",pch=19,col="black",xlab="",ylab=PCO.NO,ylim=c(-.6,1),main="a) PCO1 with spatiotemporal model predictions",cex=1,lwd=3,lty=5)
points(alldata$dist[alldata$time==1],alldata[alldata$time==2,pco.no],col="blue",pch=8,lwd=3,cex=1,lty=4)
points(alldata$dist[alldata$time==1],alldata[alldata$time==3,pco.no],col="green3",pch=15, cex=1,lwd=3,lty=3)
points(alldata$dist[alldata$time==1],alldata[alldata$time==4,pco.no],col="red",pch=17,cex=1,lwd=3,lty=2)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==1,pred.no],type="l",lty=5,col="black",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==2,pred.no],type="l",lty=4,col="blue",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==3,pred.no],type="l",lty=3,col="green3",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==4,pred.no],type="l",lty=2,col="red",lwd=3)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),
       ncol=2,horiz=FALSE,cex=0.6,title="months")

num<-2
#fig.name<-"myrpco1.tif"
pco.no<-paste("pco",num,sep="")
PCO.NO<-paste("PCO",num," (",pco.varpercent[num],"% of total variation)",sep="")
pred.no<-paste("pred",num,sep="")
#tiff(file=fig.name,width=7,height=5,units="in",pointsize = 12,bg = "transparent",res=300,compression="lzw")
#par(mar=c(5,5,4,2),cex=0.9)
plot(alldata$dist[alldata$time==1],alldata[alldata$time==1,pco.no],type="p",pch=19,col="black",xlab="",ylab=PCO.NO,ylim=c(-.6,1),main="b) PCO2 with spatiotemporal model predictions",cex=1,lwd=3,lty=5)
points(alldata$dist[alldata$time==1],alldata[alldata$time==2,pco.no],col="blue",pch=8,lwd=3,cex=1,lty=4)
points(alldata$dist[alldata$time==1],alldata[alldata$time==3,pco.no],col="green3",pch=15, cex=1,lwd=3,lty=3)
points(alldata$dist[alldata$time==1],alldata[alldata$time==4,pco.no],col="red",pch=17,cex=1,lwd=3,lty=2)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==1,pred.no],type="l",lty=5,col="black",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==2,pred.no],type="l",lty=4,col="blue",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==3,pred.no],type="l",lty=3,col="green3",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==4,pred.no],type="l",lty=2,col="red",lwd=3)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),
       ncol=2,horiz=FALSE,cex=0.6,title="months")

num<-3
#fig.name<-"myrpco1.tif"
pco.no<-paste("pco",num,sep="")
PCO.NO<-paste("PCO",num," (",pco.varpercent[num],"% of total variation)",sep="")
pred.no<-paste("pred",num,sep="")
#tiff(file=fig.name,width=7,height=5,units="in",pointsize = 12,bg = "transparent",res=300,compression="lzw")
#par(mar=c(5,5,4,2),cex=0.9)
plot(alldata$dist[alldata$time==1],alldata[alldata$time==1,pco.no],type="p",pch=19,col="black",xlab="",ylab=PCO.NO,ylim=c(-.6,1),main="c) PCO3 with spatiotemporal model predictions",cex=1,lwd=3,lty=5)
points(alldata$dist[alldata$time==1],alldata[alldata$time==2,pco.no],col="blue",pch=8,lwd=3,cex=1,lty=4)
points(alldata$dist[alldata$time==1],alldata[alldata$time==3,pco.no],col="green3",pch=15, cex=1,lwd=3,lty=3)
points(alldata$dist[alldata$time==1],alldata[alldata$time==4,pco.no],col="red",pch=17,cex=1,lwd=3,lty=2)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==1,pred.no],type="l",lty=5,col="black",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==2,pred.no],type="l",lty=4,col="blue",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==3,pred.no],type="l",lty=3,col="green3",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==4,pred.no],type="l",lty=2,col="red",lwd=3)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),
       ncol=2,horiz=FALSE,cex=0.6,title="months")

num<-4
#fig.name<-"myrpco1.tif"
pco.no<-paste("pco",num,sep="")
PCO.NO<-paste("PCO",num," (",pco.varpercent[num],"% of total variation)",sep="")
pred.no<-paste("pred",num,sep="")
#tiff(file=fig.name,width=7,height=5,units="in",pointsize = 12,bg = "transparent",res=300,compression="lzw")
#par(mar=c(5,5,4,2),cex=0.9)
plot(alldata$dist[alldata$time==1],alldata[alldata$time==1,pco.no],type="p",pch=19,col="black",xlab="",ylab=PCO.NO,ylim=c(-.6,1),main="d) PCO4 with spatiotemporal model predictions",cex=1,lwd=3,lty=5)
points(alldata$dist[alldata$time==1],alldata[alldata$time==2,pco.no],col="blue",pch=8,lwd=3,cex=1,lty=4)
points(alldata$dist[alldata$time==1],alldata[alldata$time==3,pco.no],col="green3",pch=15, cex=1,lwd=3,lty=3)
points(alldata$dist[alldata$time==1],alldata[alldata$time==4,pco.no],col="red",pch=17,cex=1,lwd=3,lty=2)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==1,pred.no],type="l",lty=5,col="black",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==2,pred.no],type="l",lty=4,col="blue",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==3,pred.no],type="l",lty=3,col="green3",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==4,pred.no],type="l",lty=2,col="red",lwd=3)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),
       ncol=2,horiz=FALSE,cex=0.6,title="months")

num<-5
#fig.name<-"myrpco1.tif"
pco.no<-paste("pco",num,sep="")
PCO.NO<-paste("PCO",num," (",pco.varpercent[num],"% of total variation)",sep="")
pred.no<-paste("pred",num,sep="")
#tiff(file=fig.name,width=7,height=5,units="in",pointsize = 12,bg = "transparent",res=300,compression="lzw")
#par(mar=c(5,5,4,2),cex=0.9)
plot(alldata$dist[alldata$time==1],alldata[alldata$time==1,pco.no],type="p",pch=19,col="black",xlab="",ylab=PCO.NO,ylim=c(-.6,1),main="e) PCO5 with spatiotemporal model predictions",cex=1,lwd=3,lty=5)
points(alldata$dist[alldata$time==1],alldata[alldata$time==2,pco.no],col="blue",pch=8,lwd=3,cex=1,lty=4)
points(alldata$dist[alldata$time==1],alldata[alldata$time==3,pco.no],col="green3",pch=15, cex=1,lwd=3,lty=3)
points(alldata$dist[alldata$time==1],alldata[alldata$time==4,pco.no],col="red",pch=17,cex=1,lwd=3,lty=2)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==1,pred.no],type="l",lty=5,col="black",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==2,pred.no],type="l",lty=4,col="blue",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==3,pred.no],type="l",lty=3,col="green3",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==4,pred.no],type="l",lty=2,col="red",lwd=3)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),
       ncol=2,horiz=FALSE,cex=0.6,title="months")


num<-6
#fig.name<-"myrpco1.tif"
pco.no<-paste("pco",num,sep="")
PCO.NO<-paste("PCO",num," (",pco.varpercent[num],"% of total variation)",sep="")
pred.no<-paste("pred",num,sep="")
#tiff(file=fig.name,width=7,height=5,units="in",pointsize = 12,bg = "transparent",res=300,compression="lzw")
#par(mar=c(5,5,4,2),cex=0.9)
plot(alldata$dist[alldata$time==1],alldata[alldata$time==1,pco.no],type="p",pch=19,col="black",xlab="",ylab=PCO.NO,ylim=c(-.6,1),main="f) PCO6 with spatiotemporal model predictions",cex=1,lwd=3,lty=5)
points(alldata$dist[alldata$time==1],alldata[alldata$time==2,pco.no],col="blue",pch=8,lwd=3,cex=1,lty=4)
points(alldata$dist[alldata$time==1],alldata[alldata$time==3,pco.no],col="green3",pch=15, cex=1,lwd=3,lty=3)
points(alldata$dist[alldata$time==1],alldata[alldata$time==4,pco.no],col="red",pch=17,cex=1,lwd=3,lty=2)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==1,pred.no],type="l",lty=5,col="black",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==2,pred.no],type="l",lty=4,col="blue",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==3,pred.no],type="l",lty=3,col="green3",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==4,pred.no],type="l",lty=2,col="red",lwd=3)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),
       ncol=2,horiz=FALSE,cex=0.6,title="months")

dev.off()


#”€”€”€”€”#### Pco1 nonlinear model
#”€”€”€”€”#### 
#”€”€”€”€”#### 

### Before using dbRDA for modlelling bugs as a function of environmental variables, we check all the possible realtionships using correlation and scatterplots
##””##””##””##dbRDA for environmental variables ##””##””##””##

myr.cap1<-capscale(formula=myrbug.BC~sin(2*pi*day/365):eff+cos(2*pi*day/365):eff+sin(2*pi*day/365):dist+cos(2*pi*day/365):dist+sin(2*pi*day/365)+cos(2*pi*day/365)+eff:time+I(dist^2):eff:time+eff:dist:time+eff,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
summary(myr.cap1)
myr.anova1<-anova(myr.cap1,by="term")
print(myr.anova1)

###”€”€”## before dbRDA , first any possible relationship between environmental variables and PCOs is examined using correlation and scatter plots
##”€”€”##catter plot and more plots
###”€”€”##Scatterplot Matrices/first two PCOs and environmental variables #*€*€*€*€*  *€*€*€*€*#
tiff(file="scatterplot1.tif",width=20,height=14,units="in",pointsize = 12,bg = "transparent",res=1200,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~alk+temp+log(cond)+do+log(toc)+log(cfpom)+log(chla)+ph2+turb+sed+vel+log(tp)+no3+pco1+pco2,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

#########first two PCOs and environmental variables (log transformed)
tiff(file="scatterplot2.tif",width=20,height=14,units="in",pointsize = 12,bg = "transparent",res=1200,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~log1p(alk)+log1p(temp)+log1p(cond)+log1p(do)+log1p(toc)+log1p(cfpom)+log1p(chla)+ph2+log1p(turb)+log1p(sed)+log1p(vel)+log1p(tp)+log1p(no3)+pco1+pco2,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

################# more exploratory analysis 
tiff(file="first two PCOs, pH, salinity, and DO.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~ph1+sali+do+pco1+pco2,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

#################first two predicted PCOs , salinity, ph and DO
tiff(file="first two predicted PCOs, salinity, ph and DO.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{usr <- par("usr"); on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
# correlation coefficient
r <- cor(x, y)
method=c("spearman")
txt <- format(c(r, 0.123456789), digits = digits)[1]
txt <- paste("r= ", txt, sep = "")
text(0.5, 0.8, txt)
# p-value calculation
p <- cor.test(x, y)$p.value
txt2 <- format(c(p, 0.123456789), digits = digits)[1]
txt2 <- paste("p= ", txt2, sep = "")
if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
text(0.5, 0.3, txt2)}
pairs(~sali+do+ph1+ph2+pred1+pred2,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

#################velocity and other factors
tiff(file="velocity and other factors1.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{usr <- par("usr"); on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
# correlation coefficient
r <- cor(x, y)
method=c("spearman")
txt <- format(c(r, 0.123456789), digits = digits)[1]
txt <- paste("r= ", txt, sep = "")
text(0.5, 0.8, txt)
# p-value calculation
p <- cor.test(x, y)$p.value
txt2 <- format(c(p, 0.123456789), digits = digits)[1]
txt2 <- paste("p= ", txt2, sep = "")
if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
text(0.5, 0.3, txt2)}
pairs(~vel+depth+width+turb+chla+sed+ph2+dflowm+dflow3m+rflow+rflow3+weekflow+dayflow,data=alldata,upper.panel=panel.cor,pch=20)
dev.off()

######### pH and other factors
tiff(file="pH and other factors.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~ph1+ph2+chla+alk+eff+dflowm+dflow3m+rflow+rflow3+weekflow+dayflow,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

########### water temperature and other factors
tiff(file="water temperature and other factors.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~temp+airtemp+eff+canop+veg30m+veg60m+veg90m+rain1+rain2+rain3+dflowm+dflow3m+rflow+rflow3+weekflow+dayflow,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

######### pH and other factors
tiff(file="pH and other factors.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~ph1+ph2+chla+alk+eff+dflowm+dflow3m+rflow+rflow3+weekflow+dayflow,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

######### Chl a and other factors
tiff(file="Chl a and other factors.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~chla+eff+canop+no3+tp+turb+airtemp+vel+temp+solar+dflowm+dflow3m+rflow+rflow3+weekflow+dayflow,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

##########CPOM/FPOM and other factors
tiff(file="CPOM and other factors.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~cfpom+dayflow+weekflow+eff+canop+veg30m+veg60m+veg90m+toc+rain1,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

##########Alkalinity and other factors
tiff(file="Alkalinity and other factors.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~alk+dflow+dalk+dcond+cond+dtemp+temp,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

##########Nitrate and other factors
tiff(file="Nitrate and other factors.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~no3+rain1+rain2+rain3+eff+dflowm+dflow3m+rflow+rflow3+weekflow+dayflow,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

##########
tiff(file="Phosphate and other factors.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~log(tp)+eff+alk+ph2+dflowm+dflow3m+rain1+rain2+rain3,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

########salinity and other factors
tiff(file="salinity and other factors.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~sali+tp+no3+dflowm+dflow3m+rain1+rain2+rain3+eff,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

########Turbidity and other factors
tiff(file="Turbidity and other factors.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~turb+vel+eff+dflowm+dflow3m+rain1+rain2+rain3,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

########sediment and other factors
tiff(file="Turbidity and other factors",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~sed+vel+eff+dflowm+dflow3m+rflow+rflow3+weekflow+dayflow+rain1+rain2+rain3,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

###########Scatterplot Matrices/first two PCOs and environmental variables
tiff(file="first two PCOs and environmental variables.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~pco1+pco2+temp+cfpom+chla+ph2+turb+sed+vel+alk+cond+no3+tp+tn+dflowm+dflow3m+rflow+rflow3+weekflow+dayflow+rain1+rain2+rain3,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

###########
tiff(file="first two PCOs ,important variables",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~log(vel)+log(sali)+temp+log(chla)+log(do)+pco1+pco2,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

###########
tiff(file="first two PCOs , nutrients.tiff",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~alk+vel+sed+ph2+ph1+turb+sali+do+toc+cfpom+temp+no3+tp+tn+chla+cond+pco1+pco2,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
dev.off()

##########Checking correlation between variables
edit(alldata)
plot(alldata$temp,alldata$pco1)
cor.test(alldata$temp,alldata$pco1)
plot(log(alldata$temp),alldata$pco1)
cor.test(log(alldata$temp),alldata$pco1)

plot(alldata$cond,alldata$pco1)
cor.test(alldata$cond,alldata$pco1)
plot(log10(alldata$cond),alldata$pco1)
cor.test(log10(alldata$cond),alldata$pco1)

plot(alldata$alk,alldata$pco1)
cor.test(alldata$alk,alldata$pco1)
plot(log(alldata$alk),alldata$pco1)
cor.test(log(alldata$alk),alldata$pco1)

plot(alldata$sed,alldata$pco1)
cor.test(alldata$sed,alldata$pco1)
plot(alldata$sed[alldata$sed>40],alldata$pco1[alldata$sed>40])
cor.test(alldata$sed[alldata$sed>40],alldata$pco1[alldata$sed>40])
plot(log(alldata$sed[alldata$sed>40]),alldata$pco1[alldata$sed>40])
cor.test(log(alldata$sed[alldata$sed>40]),alldata$pco1[alldata$sed>40])

plot(alldata$toc,alldata$pco1)
cor.test(alldata$toc,alldata$pco1)
plot(log(alldata$toc),alldata$pco1)
cor.test(log(alldata$toc),alldata$pco1)

plot(alldata$sali,alldata$pco1)
cor.test(alldata$sali,alldata$pco1)

plot(alldata$cfpom,alldata$pco1)
cor.test(alldata$cfpom,alldata$pco1)
plot(log(alldata$cfpom),alldata$pco1)
cor.test(log(alldata$cfpom),alldata$pco1)

plot(alldata$turb,alldata$pco1)
cor.test(alldata$turb,alldata$pco1)
plot(log(alldata$turb),alldata$pco1)
cor.test(log(alldata$turb),alldata$pco1)

plot(alldata$no3,alldata$pco1)
cor.test(alldata$no3,alldata$pco1)
plot(log(alldata$no3),alldata$pco1)
cor.test(log(alldata$no3),alldata$pco1)
plot(log(alldata$no3[alldata$no3<1]),alldata$pco1[alldata$no3<1])
cor.test(log(alldata$no3[alldata$no3<1]),alldata$pco1[alldata$no3<1])

plot(alldata$do,alldata$pco1)
cor.test(alldata$do,alldata$pco1)
plot(sqrt(alldata$do),alldata$pco1)
cor.test(sqrt(alldata$do),alldata$pco1)

plot(alldata$tp,alldata$pco1)
cor.test(alldata$tp,alldata$pco1)
plot(log(alldata$tp),alldata$pco1)
cor.test(log(alldata$tp),alldata$pco1)
plot(sqrt(alldata$tp),alldata$pco1)
cor.test(sqrt(alldata$tp),alldata$pco1)

plot(alldata$chla,alldata$pco1)
cor.test(alldata$chla,alldata$pco1)
plot(log(alldata$chla),alldata$pco1)
cor.test(log(alldata$chla),alldata$pco1)

plot(alldata$cond,alldata$pco1)
cor.test(alldata$cond,alldata$pco1)
plot(log(alldata$cond),alldata$pco1)
cor.test(log(alldata$cond),alldata$pco1)

plot(alldata$ph2,alldata$pco1)
cor.test(alldata$ph2,alldata$pco1)

plot(alldata$do,alldata$pco1)
cor.test(alldata$do,alldata$pco1)
plot(log(alldata$do),alldata$pco1)
cor.test(log(alldata$do),alldata$pco1)

plot(alldata$pred2,alldata$pco2)
cor.test(alldata$pred1,alldata$pco1)

plot(alldata$alk,alldata$pco1)
cor.test(alldata$alk,alldata$pco1)
plot(log(alldata$alk),alldata$pco1)
cor.test(log(alldata$alk),alldata$pco1)

plot(alldata$turb,alldata$toc)
cor.test(alldata$turb,alldata$toc)

plot(alldata$dflow3m,alldata$dayflow)
cor.test(alldata$dflow3m,alldata$dayflow)

plot(alldata$sali,alldata$pco1)
cor.test(alldata$sali,alldata$pco1)

plot(alldata$nh3,alldata$pco1)
cor.test(alldata$nh3,alldata$pco1)

plot(alldata$cfpom,alldata$pco1)
cor.test(alldata$cfpom,alldata$pco1)
plot(log(alldata$cfpom),alldata$pco1)
cor.test(log(alldata$cfpom),alldata$pco1)

###”€”€”## Testing the variables before dbRDA --PCO1

###”€”€”## scatterplot for pco1 
#######
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(pco1~ph2+do+cfpom+nh3+turb+temp+sed+cond+toc+tp+tn+alk,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
edit(alldata)

###”€”€”## pco1 as a function of environmental variables
library(mass)
install.packages("mass")
tempo<-lm(pco1~temp,data=alldata) # important
summary(tempo)
tempo<-lm(pco1~cond,data=alldata) # important
summary(tempo)
tempo<-lm(pco1~temp+cond+temp:cond,data=alldata) # important
summary(tempo)
tempo<-lm(pco1~log(no3),data=alldata) # important
summary(tempo)
tempo<-lm(pco1~no3+temp+cond+temp:cond+temp:no3,data=alldata) # important
summary(tempo)
tempo<-lm(pco1~chla,data=alldata)
tempo<-lm(pco1~do,data=alldata) # important
tempo<-lm(pco1~temp+do+do:temp,data=alldata)
tempo<-lm(pco1~do+temp+cond+temp:cond+temp:do,data=alldata) 
tempo<-lm(pco1~temp+cond+temp:cond+do+do:temp,data=alldata)
tempo<-lm(pco1~cfpom,data=alldata)
tempo<-lm(pco1~cfpom+temp+temp:cfpom,data=alldata) # important
tempo<-lm(pco1~temp+cond+temp:cond+cfpom+cfpom:temp,data=alldata)
tempo<-lm(pco1~turb,data=alldata)
tempo<-lm(pco1~temp+turb+turb:temp,data=alldata) # important
summary(tempo)
tempo<-lm(pco1~temp+cond+temp:cond+turb+turb:temp,data=alldata) # important
tempo<-lm(pco1~vel,data=alldata)
tempo<-lm(pco1~temp+vel+vel:temp,data=alldata)
tempo<-lm(pco1~temp+cond+temp:cond+vel+vel:temp,data=alldata)
tempo<-lm(pco1~nh3,data=alldata) # important
tempo<-lm(pco1~temp+nh3+nh3:temp,data=alldata) # important
tempo<-lm(pco1~temp+cond+temp:cond+nh3+nh3:temp,data=alldata)
tempo<-lm(pco1~toc,data=alldata)
tempo<-lm(pco1~temp+toc+toc:temp,data=alldata) # important
tempo<-lm(pco1~temp+cond+temp:cond+toc+toc:temp,data=alldata)
tempo<-lm(pco1~ph2,data=alldata) # important
tempo<-lm(pco1~temp+ph2+ph2:temp,data=alldata) # important
tempo<-lm(pco1~temp+cond+temp:cond+ph2+ph2:temp,data=alldata) # important
summary(tempo)
tempo<-lm(pco1~sed,data=alldata) # important
tempo<-lm(pco1~temp+sed+sed:temp,data=alldata)
tempo<-lm(pco1~tp,data=alldata) # important
tempo<-lm(pco1~temp+tp+tp:temp,data=alldata) # important
tempo<-lm(pco1~tn,data=alldata) # important
tempo<-lm(pco1~temp+tn+tn:temp,data=alldata) # important
tempo<-lm(pco1~canop,data=alldata) # 
tempo<-lm(pco1~log(sed),data=alldata) # 
tempo<-lm(pco1~alk,data=alldata) # important
tempo<-lm(pco1~temp+alk+alk:temp,data=alldata) # important
summary(tempo)

## final models
tempo<-lm(pco1~temp,data=alldata)## Adjusted R-squared:  0.7246
tempo<-lm(pco1~temp+cond+temp:cond,data=alldata)##temp:cond---> Adjusted R-squared:  0.7595
tempo<-lm(pco1~temp+toc+temp:toc,data=alldata)##temp:toc---> Adjusted R-squared:  0.8663

tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do+do:temp+chla+temp+temp:cfpom+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+vel+sed,data=alldata)## without canop
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do+do:temp+chla+temp+temp:cfpom+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+vel,data=alldata)## without canop+sed
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do+do:temp+chla+temp+temp:cfpom+ph2+ph2:temp+tp:temp+tp+tn:temp+tn,data=alldata)## without canop+sed+vel

tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do+do:temp+chla+temp+temp:cfpom+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+alk+alk:time,data=alldata)## without canop+sed+vel
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do:temp+chla+temp+temp:cfpom+ph2+ph2:temp+tp:temp+tp+tn:temp+tn,data=alldata)## without canop+sed+vel+do
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+chla+temp+temp:cfpom+ph2+ph2:temp+tp:temp+tp+tn:temp+tn,data=alldata)## without canop+sed+vel+do+temp:do
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+temp+temp:cfpom+ph2+ph2:temp+tp:temp+tp+tn:temp+tn,data=alldata)## without canop+sed+vel+do+temp:do+chla
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+temp+ph2+ph2:temp+tp:temp+tp+tn:temp+tn,data=alldata)## without canop+sed+vel+do+temp:do+chla+temp:cfpom
tempo<-lm(pco1~temp+cond+temp:cond+nh3+no3+no3:temp+temp+ph2+ph2:temp+tp:temp+tp+tn:temp+tn,data=alldata)## without canop+sed+vel+do+temp:do+chla+temp:cfpom
tempo<-lm(pco1~temp+cond+temp:cond+nh3+no3+no3:temp+temp+ph2+ph2:temp+tp:temp+tp+tn:temp+tn,data=alldata)## without canop+sed+vel+do+temp:do+chla+temp:cfpom

tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do+do:temp+chla+temp+temp:cfpom+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+vel+canop+sed+turb+alk+alk:temp,data=alldata)##
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do+do:temp+chla+temp+temp:cfpom+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+vel+canop+turb+alk+alk:temp,data=alldata)## sed+
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do+do:temp+chla+temp+temp:cfpom+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+canop+turb+alk+alk:temp,data=alldata)## sed+vel+
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do+do:temp+chla+temp+temp:cfpom+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+turb+alk+alk:temp,data=alldata)## sed+vel+canop+
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do+do:temp+chla+temp+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+turb+alk+alk:temp,data=alldata)## sed+vel+canop+temp:cfpom+
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do+do:temp+temp+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+turb+alk+alk:temp+toc+toc:temp,data=alldata)## sed+vel+canop+temp:cfpom+chla+
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do+do:temp+temp+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+turb+alk+toc+toc:temp,data=alldata)## sed+vel+canop+temp:cfpom+chla+alk:temp+
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do+do:temp+temp+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+alk,data=alldata)## sed+vel+canop+temp:cfpom+chla+alk:temp+toc:temp+toc+turb+
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+do+temp+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+alk,data=alldata)## sed+vel+canop+temp:cfpom+chla+alk:temp+toc:temp+toc+turb+do:temp+
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+temp+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+alk,data=alldata)## sed+vel+canop+temp:cfpom+chla+alk:temp+toc:temp+toc+turb+do:temp+do+ ### final model
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+alk,data=alldata)## sed+vel+canop+temp:cfpom+chla+alk:temp+toc:temp+toc+turb+do:temp+do+ ### final model

summary(tempo)
anova(tempo)
anova(tempo,tempo1)
avPlots(tempo)
plot(tempo)
tempo1<-cbind(cbind(fitted(tempo),residuals(tempo)),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
#edit(tempo1)
tempo1[,"2"]
tempo2<-lm(tempo1[,"2"]~pred1,data=tempo1)
summary(tempo2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco1"],type = "p",ylim=c(-.6,.6),pch=19,cex=0.8,xlab="Distance(km)",ylab="Pred1 score",main="Predicted values plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco1"],type="p",col="blue",pch=8,lwd=3,cex=0.8,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco1"],type="p",col="green3",pch=15,cex=0.8,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco1"],type="p",col="red",pch=17,cex=0.8,lwd=3,lty=2)

points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type = "l",ylim=c(-.6,0.6),pch=19,col="black",cex=1.5,xlab="Distance(km)",ylab="Pred1",main="Prediction with environmental variables",lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1.5,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=2,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

xyplot(fitted(tempo)~dist, groups=day,data=tempo1,type="l",auto.key=TRUE)
xyplot(residuals(tempo)~dist, groups=day,data=tempo1,type="l",auto.key=TRUE)
tempo3<-lm(tempo1[,2]~tempo1$pred1)
summary(tempo3)

#*€*€*€*€* Paper4.Figure6 in thesis *€*€*€*€*#
tiff(file="Paper4.Figure6.tif",width=10,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mfrow=c(3,1), mar=c(4.5,4.5,2.5,3),cex=1.5,cex.axis=0.9,las=1,cex.main=1,cex.lab=0.8)

## pco plot against distance
##PCO1 against distance
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco1"],type = "b",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO1 score",main="PCO1 plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco1"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco1"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco1"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

### predicted pcos
num<-1
#fig.name<-"myrpco1.tif"
pco.no<-paste("pco",num,sep="")
PCO.NO<-paste("PCO",num," (",pco.varpercent[num],"% of total variation)",sep="")
pred.no<-paste("pred",num,sep="")
#tiff(file=fig.name,width=7,height=5,units="in",pointsize = 12,bg = "transparent",res=300,compression="lzw")
#par(mar=c(5,5,4,2),cex=0.9)
plot(alldata$dist[alldata$time==1],alldata[alldata$time==1,pco.no],type="p",pch=19,col="black",xlab="",ylab=PCO.NO,ylim=c(-.6,1),main="b) PCO1 with spatiotemporal model predictions",cex=1.5,lwd=3,lty=5)
points(alldata$dist[alldata$time==1],alldata[alldata$time==2,pco.no],col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(alldata$dist[alldata$time==1],alldata[alldata$time==3,pco.no],col="green3",pch=15, cex=1.5,lwd=3,lty=3)
points(alldata$dist[alldata$time==1],alldata[alldata$time==4,pco.no],col="red",pch=17,cex=1.5,lwd=3,lty=2)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==1,pred.no],type="l",lty=5,col="black",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==2,pred.no],type="l",lty=4,col="blue",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==3,pred.no],type="l",lty=3,col="green3",lwd=3)
lines(xygrid$dist[xygrid$time==1],pco.predict2[pco.predict2$time==4,pred.no],type="l",lty=2,col="red",lwd=3)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),
       ncol=2,horiz=FALSE,cex=0.6,title="months")


## pco plot for fitted values
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+temp+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+alk,data=alldata)## sed+vel+canop+temp:cfpom+chla+alk:temp+toc:temp+toc+turb+do:temp+do+
tempo1<-cbind(cbind(fitted(tempo),residuals(tempo)),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
plot(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type = "l",ylim=c(-.6,1),pch=19,col="black",xlab="Distance(km)",ylab="PCO1",main="c) Prediction with environmental variables",lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.4,lwd=3,lty=2)
points(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco1"],type = "p",ylim=c(-.6,1),pch=19,xlab="",ylab="PCO1",main="a) PCO1 plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco1"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco1"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco1"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)

legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)
dev.off()


##############dbRDA for pco1~env ## to check which variables explain time:space (spatiotemporal variation)
myr.cap1<-capscale(formula=myrbug.BC~time+I(dist==11.03):eff:time,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)
summary(myr.cap1)
myr.anova1<-anova(myr.cap1,by="term")
print(myr.anova1)

edit(myrbug)
check.datasets(myrbug,myrenv)
edit(myrenv)

myrbug.BC<-vegdist(sqrt(myrbug)) 
myr.cap2<-capscale(formula=myrbug.BC~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+temp+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+alk,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### 
myr.cap2<-capscale(formula=myrbug.BC~temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+temp+ph2+ph2:temp+alk,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### (without tp:temp+tp+tn:temp+tn)###
myr.cap2<-capscale(formula=myrbug.BC~temp+cond+temp:cond+nh3+temp:nh3+temp+ph2+ph2:temp+alk,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### (without tp:temp+tp+tn:temp+tn) and (no3+no3:temp)###
myr.cap2<-capscale(formula=myrbug.BC~temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+alk,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### (all significant ones)###

anova(myr.cap2)
summary(myr.cap2)
myr.anova2<-anova(myr.cap2,by="term",permutations=how(nperm=9999))
print(myr.anova2)## Paper 4-Table 3

myr.cap2b<-capscale(formula=myrbug.BC~temp+nh3+temp+ph2+ph2:temp+alk,data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)### (all significant ones)###
summary(myr.cap2b)
myr.anova2b<-anova(myr.cap2b,by="term")
print(myr.anova2b) #*€*€*€*€*€*€*€*€*# Spatiotemporal model without effluent terms
anova(myr.cap2b,myr.cap2,permutations = how(nperm=9999))



myr.cap3<-capscale(formula=myrbug.BC~time+Condition(temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+temp+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+alk),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)## 
myr.cap3<-capscale(formula=myrbug.BC~time+time:eff+I(dist==8.7):time+I(dist==10.4):time+I(dist==11.03):eff:time+I(dist==15.13):time+Condition(temp+cond+temp:cond+nh3+temp:nh3+no3+no3:temp+temp+ph2+ph2:temp+tp:temp+tp+tn:temp+tn+alk),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)##
myr.cap3<-capscale(formula=myrbug.BC~time+time:eff+I(dist==8.7):time+I(dist==10.4):time+I(dist==11.03):eff:time+I(dist==15.13):time+Condition(temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+alk),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)##
myr.cap3<-capscale(formula=myrbug.BC~time+I(dist==11.03):eff:time+Condition(temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+alk),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)##
myr.cap3<-capscale(formula=myrbug.BC~time+Condition(temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+alk),data=myrenv,comm=myrbug,add=TRUE,na.action=na.omit)##


summary(myr.cap3)
myr.anova3<-anova(myr.cap3,by="term",permutations=how(nperm=9999)) # No evidence of spatiotemporal patterns
print(myr.anova3)###



##”€”€”##Testing the variables before dbRDA --PCO2
tempo<-lm(pco2~temp,data=alldata) # important
tempo<-lm(pco2~cond,data=alldata)
tempo<-lm(pco2~temp+cond+temp:cond,data=alldata)
tempo<-lm(pco2~log(chla),data=alldata)
tempo<-lm(pco2~temp+log(chla)+log(chla):temp,data=alldata)
tempo<-lm(pco2~do,data=alldata)
tempo<-lm(pco2~do+temp+do:temp,data=alldata) # important
tempo<-lm(pco2~log(cfpom),data=alldata)
tempo<-lm(pco2~log(cfpom)+temp+temp:log(cfpom),data=alldata)
tempo<-lm(pco2~temp+cond+temp:cond+do+do:temp,data=alldata)
tempo<-lm(pco2~turb,data=alldata)
tempo<-lm(pco2~temp+turb+turb:temp,data=alldata) # important
tempo<-lm(pco2~temp+cond+temp:cond+turb+turb:temp,data=alldata)
tempo<-lm(pco2~vel,data=alldata)
tempo<-lm(pco2~temp+vel+vel:temp,data=alldata)
tempo<-lm(pco2~temp+cond+temp:cond+vel+vel:temp,data=alldata)
tempo<-lm(pco2~nh3,data=alldata)
tempo<-lm(pco2~temp+nh3+nh3:temp,data=alldata) # important
tempo<-lm(pco2~temp+cond+temp:cond+nh3+nh3:temp,data=alldata)
tempo<-lm(pco2~toc,data=alldata)
tempo<-lm(pco2~temp+toc+toc:temp,data=alldata) # important
tempo<-lm(pco2~temp+cond+temp:cond+toc+toc:temp,data=alldata)
tempo<-lm(pco2~ph2,data=alldata)
tempo<-lm(pco2~temp+ph2+ph2:temp,data=alldata) # important
tempo<-lm(pco2~temp+cond+temp:cond+ph2+ph2:temp,data=alldata)
tempo<-lm(pco2~temp+sed+sed:temp,data=alldata)
summary(tempo)

## final models for pco2
tempo<-lm(pco2~toc,data=alldata)
tempo<-lm(pco2~turb,data=alldata)
tempo<-lm(pco2~vel,data=alldata)
tempo<-lm(pco2~ph2,data=alldata)
tempo<-lm(pco2~temp+ph2+temp:ph2,data=alldata) # important
tempo<-lm(pco2~temp+nh3+temp:nh3,data=alldata) # important
tempo<-lm(pco2~temp+log(chla)+temp:log(chla),data=alldata)

summary(tempo)

tempo2<-cbind(cbind(fitted(tempo),residuals(tempo)),alldata)
attributes(tempo2)
names(tempo2[,1:2])<-c("fitted","residual")
edit(tempo2)
tempo2[,"2"]
tempo3<-lm(tempo2[,"2"]~pred2,data=tempo2)
summary(tempo3)

par(mar=c(5,5,4,2),cex=0.9)
plot(tempo2[tempo2$day==1,"dist"],tempo2[tempo2$day==1,"2"],type = "b",ylim=c(-0.6,0.6),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO2",main="prediction with other variables interaction",col="black",lwd=3,lty=4)
points(tempo2[tempo2$day==92,"dist"],tempo2[tempo2$day==92,"2"],type="b",col="blue",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo2[tempo2$day==210,"dist"],tempo2[tempo2$day==210,"2"],type="b",col="green3",pch=15,cex=1.5,lwd=3)
points(tempo2[tempo2$day==283,"dist"],tempo2[tempo2$day==283,"2"],type="b",col="red",pch=17,cex=2,lwd=3,lty=5)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.7,title="months")
abline(v=10.9,lty=2)

xyplot(fitted(tempo)~dist, groups=day,data=tempo2,type="l",auto.key=TRUE)
xyplot(residuals(tempo)~dist, groups=day,data=tempo2,type="l",auto.key=TRUE)
tempo3<-lm(tempo2[,2]~tempo2$pred2)
summary(tempo3)

## scatterplot for pco2 
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(pco2~ph2+do+cfpom+nh3+turb+temp+sed+cond+toc+tp+tn+cfpom+vel+no3,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)
edit(alldata)

###”€”€”###Testing the variables before dbRDA --PCO3
edit(alldata)
tempo<-lm(pco3~log(temp),data=alldata)
tempo<-lm(pco3~cond,data=alldata)
tempo<-lm(pco3~log(temp)+cond+log(temp):cond,data=alldata)
tempo<-lm(pco3~log(chla),data=alldata)
tempo<-lm(pco3~temp+log(chla)+log(chla):temp,data=alldata)
tempo<-lm(pco3~temp+cond+temp:cond+log(chla)+temp:log(chla),data=alldata)
tempo<-lm(pco3~do,data=alldata)
tempo<-lm(pco3~temp+do+do:temp,data=alldata)
tempo<-lm(pco3~temp+cond+temp:cond+do+do:temp,data=alldata)
tempo<-lm(pco3~cfpom,data=alldata)
tempo<-lm(pco3~cfpom+temp+temp:cfpom,data=alldata)
tempo<-lm(pco3~temp+cond+temp:cond+cfpom+cfpom:temp,data=alldata)
tempo<-lm(pco3~temp+cond+temp:cond,data=alldata)
tempo<-lm(pco3~turb,data=alldata)
tempo<-lm(pco3~temp+turb+turb:temp,data=alldata)
tempo<-lm(pco3~temp+cond+temp:cond+turb+turb:temp,data=alldata)
tempo<-lm(pco3~vel,data=alldata)
tempo<-lm(pco3~temp+vel+vel:temp,data=alldata)
tempo<-lm(pco3~temp+cond+temp:cond+vel+vel:temp,data=alldata)
tempo<-lm(pco3~nh3,data=alldata)
tempo<-lm(pco3~temp+nh3+nh3:temp,data=alldata)
tempo<-lm(pco3~temp+cond+temp:cond+nh3+nh3:temp,data=alldata)
tempo<-lm(pco3~toc,data=alldata)
tempo<-lm(pco3~temp+toc+toc:temp,data=alldata)
tempo<-lm(pco3~temp+cond+temp:cond+toc+toc:temp,data=alldata)
tempo<-lm(pco3~ph2,data=alldata)
tempo<-lm(pco3~temp+ph2+ph2:temp,data=alldata)
tempo<-lm(pco3~temp+cond+temp:cond+ph2+ph2:temp,data=alldata)
summary(tempo)

## final models for pco3
tempo<-lm(pco3~temp+toc+temp:toc,data=alldata)##
tempo<-lm(pco3~do,data=alldata)##
tempo<-lm(pco3~log(chla),data=alldata)##
tempo<-lm(pco3~nh3,data=alldata)##
tempo<-lm(pco3~vel,data=alldata)##
tempo<-lm(pco3~log(temp),data=alldata)## 
tempo<-lm(pco3~temp+temp:toc+toc+do+turb+temp:turb,data=alldata)## 
tempo<-lm(pco3~temp+temp:toc+toc+do+turb+temp:turb+log(chla),data=alldata)##
tempo<-lm(pco3~temp+temp:toc+toc+do+turb+temp:turb+log(chla)+nh3+vel+log(temp),data=alldata)##  
tempo<-lm(pco3~temp+temp:toc+toc+turb+temp:turb+log(chla)+nh3+vel+log(temp),data=alldata)## 
tempo<-lm(pco3~temp+temp:toc+toc+turb+temp:turb+log(chla)+vel+log(temp),data=alldata)##

summary(tempo)

tempo3<-cbind(cbind(fitted(tempo),residuals(tempo)),alldata)
attributes(tempo3)
names(tempo3[,1:2])<-c("fitted","residual")
edit(tempo3)
tempo3[,"2"]
tempo4<-lm(tempo3[,"2"]~pred3,data=tempo3)
summary(tempo4)

par(mar=c(5,5,4,2),cex=0.9)
plot(tempo3[tempo3$day==1,"dist"],tempo3[tempo3$day==1,"1"],type = "b",ylim=c(-0.6,0.6),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO3",main="prediction with other variables interaction",col="black",lwd=3,lty=4)
points(tempo3[tempo3$day==92,"dist"],tempo3[tempo3$day==92,"1"],type="b",col="blue",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo3[tempo3$day==210,"dist"],tempo3[tempo3$day==210,"1"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(tempo3[tempo3$day==283,"dist"],tempo3[tempo3$day==283,"1"],type="b",col="red",pch=17,cex=1.5,lwd=3)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=4,horiz=FALSE,cex=0.7,title="months")
abline(v=10.9,lty=2)

xyplot(fitted(tempo)~dist, groups=day,data=tempo3,type="l",auto.key=TRUE)
xyplot(residuals(tempo)~dist, groups=day,data=tempo3,type="l",auto.key=TRUE)
tempo4<-lm(tempo3[,2]~tempo3$pred3)
summary(tempo4)

plot(alldata$nh3~alldata$pco3)
cor.test(alldata$nh3,alldata$pco3)
plot(log(alldata$temp),alldata$pco3)
cor.test(log(alldata$temp),alldata$pco3)

##Testing the variables before dbRDA --PCO4
edit(alldata)
summary(tempo)
tempo<-lm(pco4~temp,data=alldata)
tempo<-lm(pco4~cond,data=alldata)
tempo<-lm(pco4~log(temp)+cond+log(temp):cond,data=alldata)
tempo<-lm(pco4~log(chla),data=alldata)
tempo<-lm(pco4~temp+log(chla)+log(chla):temp,data=alldata)
tempo<-lm(pco4~temp+cond+temp:cond+log(chla)+temp:log(chla),data=alldata)
tempo<-lm(pco4~do,data=alldata)
tempo<-lm(pco4~temp+do+do:temp,data=alldata)
tempo<-lm(pco4~temp+cond+temp:cond+do+do:temp,data=alldata)
tempo<-lm(pco4~cfpom,data=alldata)
tempo<-lm(pco4~cfpom+temp+temp:cfpom,data=alldata)
tempo<-lm(pco4~temp+cond+temp:cond+cfpom+cfpom:temp,data=alldata)
tempo<-lm(pco4~cod,data=alldata)
tempo<-lm(pco4~cod+temp+temp:cod,data=alldata)
tempo<-lm(pco4~temp+cond+temp:cond+cod+cod:temp,data=alldata)
tempo<-lm(pco4~turb,data=alldata)
tempo<-lm(pco4~temp+turb+turb:temp,data=alldata)
tempo<-lm(pco4~temp+cond+temp:cond+turb+turb:temp,data=alldata)
tempo<-lm(pco4~vel,data=alldata)
tempo<-lm(pco4~temp+vel+vel:temp,data=alldata)
tempo<-lm(pco4~temp+cond+temp:cond+vel+vel:temp,data=alldata)
tempo<-lm(pco4~nh3,data=alldata)
tempo<-lm(pco4~temp+nh3+nh3:temp,data=alldata)
tempo<-lm(pco4~temp+cond+temp:cond+nh3+nh3:temp,data=alldata)
tempo<-lm(pco4~toc,data=alldata)
tempo<-lm(pco4~temp+toc+toc:temp,data=alldata)
tempo<-lm(pco4~temp+cond+temp:cond+toc+toc:temp,data=alldata)
tempo<-lm(pco4~ph2,data=alldata)
tempo<-lm(pco4~temp+ph2+ph2:temp,data=alldata)
tempo<-lm(pco4~temp+cond+temp:cond+ph2+ph2:temp,data=alldata)

###”€”€”# final models for pco4
tempo<-lm(pco4~temp+cfpom+temp:cfpom,data=alldata) ## 
tempo<-lm(pco4~temp+toc+temp:toc,data=alldata)## 
tempo<-lm(pco4~do,data=alldata)##
tempo<-lm(pco4~turb,data=alldata)##
tempo<-lm(pco4~temp+cod+temp:cod,data=alldata)## 
tempo<-lm(pco4~cfpom,data=alldata)##

tempo<-lm(pco4~temp+toc+temp:toc+cfpom+temp:cfpom,data=alldata)## 
tempo<-lm(pco4~temp+toc+temp:toc+cod+temp:cod+cfpom+temp:cfpom,data=alldata)## 
tempo<-lm(pco4~temp+toc+temp:toc+cod+temp:cod+cfpom+temp:cfpom+do+turb,data=alldata)##
tempo<-lm(pco4~temp+toc+temp:toc+cod+temp:cod+cfpom+temp:cfpom+turb,data=alldata)## 

summary(tempo)
tempo4<-cbind(cbind(fitted(tempo),residuals(tempo)),alldata[complete.cases(alldata[,c("temp","toc","cod","cfpom","turb")]),])
attributes(tempo4)
names(tempo4[,1:2])<-c("fitted","residual")
edit(tempo4)
tempo4[,"2"]
tempo5<-lm(tempo4[,"2"]~pred4,data=tempo4)
summary(tempo5)

par(mar=c(5,5,4,2),cex=0.9)
plot(tempo4[tempo4$day==1,"dist"],tempo4[tempo4$day==1,"1"],type = "b",ylim=c(-0.6,0.6),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO4",main="prediction with other variables interaction",col="gold2",lwd=3,lty=4)
points(tempo4[tempo4$day==92,"dist"],tempo4[tempo4$day==92,"1"],type="b",col="blue",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo4[tempo4$day==210,"dist"],tempo4[tempo4$day==210,"1"],type="b",col="green3",pch=15,cex=1.5,lwd=3)
points(tempo4[tempo4$day==283,"dist"],tempo4[tempo4$day==283,"1"],type="b",col="red",pch=17,cex=2,lwd=3,lty=5)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=4,horiz=FALSE,cex=0.7,title="months")
abline(v=10.9,lty=2)

xyplot(fitted(tempo)~dist, groups=day,data=tempo4,type="l",auto.key=TRUE)
xyplot(residuals(tempo)~dist, groups=day,data=tempo4,type="l",auto.key=TRUE)
tempo5<-lm(tempo4[,2]~tempo4$pred4)
summary(tempo5)

plot(alldata$turb~alldata$pco4)
cor.test(alldata$turb,alldata$pco4)
plot(log(alldata$turb),alldata$pco4)
cor.test(log(alldata$turb),alldata$pco4)

###”€”€”#Testing the variables before dbRDA --PCO5
edit(alldata)
tempo<-lm(pco5~temp,data=alldata)
tempo<-lm(pco5~cond,data=alldata)
tempo<-lm(pco5~temp+cond+log(temp):cond,data=alldata)
tempo<-lm(pco5~log(chla),data=alldata)
tempo<-lm(pco5~temp+log(chla)+log(chla):temp,data=alldata)
tempo<-lm(pco5~temp+cond+temp:cond+log(chla)+temp:log(chla),data=alldata)
tempo<-lm(pco5~do,data=alldata)
tempo<-lm(pco5~temp+do+do:temp,data=alldata)
tempo<-lm(pco5~temp+cond+temp:cond+do+do:temp,data=alldata)
tempo<-lm(pco5~cfpom,data=alldata)
tempo<-lm(pco5~cfpom+temp+temp:cfpom,data=alldata)
tempo<-lm(pco5~temp+cond+temp:cond+cfpom+cfpom:temp,data=alldata)
tempo<-lm(pco5~cod,data=alldata)
tempo<-lm(pco5~cod+temp+temp:cod,data=alldata)
tempo<-lm(pco5~temp+cond+temp:cond+cod+cod:temp,data=alldata)
tempo<-lm(pco5~turb,data=alldata)
tempo<-lm(pco5~temp+turb+turb:temp,data=alldata)
tempo<-lm(pco5~temp+cond+temp:cond+turb+turb:temp,data=alldata)
tempo<-lm(pco5~vel,data=alldata)
tempo<-lm(pco5~temp+vel+vel:temp,data=alldata)
tempo<-lm(pco5~temp+cond+temp:cond+vel+vel:temp,data=alldata)
tempo<-lm(pco5~nh3,data=alldata)
tempo<-lm(pco5~temp+nh3+nh3:temp,data=alldata)
tempo<-lm(pco5~temp+cond+temp:cond+nh3+nh3:temp,data=alldata)
tempo<-lm(pco5~toc,data=alldata)
tempo<-lm(pco5~temp+toc+toc:temp,data=alldata)
tempo<-lm(pco5~temp+cond+temp:cond+toc+toc:temp,data=alldata)
tempo<-lm(pco5~ph2,data=alldata)
tempo<-lm(pco5~temp+ph2+ph2:temp,data=alldata)
tempo<-lm(pco5~temp+cond+temp:cond+ph2+ph2:temp,data=alldata)
tempo<-lm(pco5~sed,data=alldata)
summary(tempo)

## final models for pco5
tempo<-lm(pco5~log(vel),data=alldata)## 
tempo<-lm(pco5~ph2,data=alldata)##
tempo<-lm(pco5~cod,data=alldata)##
tempo<-lm(pco5~ph2+cod,data=alldata)##

summary(tempo)
tempo5<-cbind(cbind(fitted(tempo),residuals(tempo)),alldata)#[complete.cases(alldata[,c("temp","cond")]),])
attributes(tempo5)
names(tempo5[,1:2])<-c("fitted","residual")
edit(tempo5)
tempo5[,"2"]
tempo6<-lm(tempo5[,"2"]~pred5,data=tempo5)
summary(tempo6)

par(mar=c(5,5,4,2),cex=0.9)
plot(tempo5[tempo5$day==1,"dist"],tempo5[tempo5$day==1,"1"],type = "b",ylim=c(-0.6,0.6),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO5",main="prediction with other variables interaction",col="gold2",lwd=3,lty=4)
points(tempo5[tempo5$day==92,"dist"],tempo5[tempo5$day==92,"1"],type="b",col="blue",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo5[tempo5$day==210,"dist"],tempo5[tempo5$day==210,"1"],type="b",col="green3",pch=15,cex=1.5,lwd=3)
points(tempo5[tempo5$day==283,"dist"],tempo5[tempo5$day==283,"1"],type="b",col="red",pch=17,cex=2,lwd=3,lty=5)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=4,horiz=FALSE,cex=0.7,title="months")
abline(v=10.9,lty=2)


xyplot(fitted(tempo)~dist, groups=day,data=tempo5,type="l",auto.key=TRUE)
xyplot(residuals(tempo)~dist, groups=day,data=tempo5,type="l",auto.key=TRUE)
tempo6<-lm(tempo5[,2]~tempo5$pred5)
summary(tempo6)

plot(alldata$ph2~alldata$pco5)
cor.test(alldata$ph2,alldata$pco5)
plot(log(alldata$chla),alldata$pco5)
cor.test(log(alldata$chla),alldata$pco5)

###”€”€”###Testing the variables before dbRDA --PCO6
edit(alldata)
summary(tempo)
tempo<-lm(pco6~temp,data=alldata)
tempo<-lm(pco6~cond,data=alldata)
tempo<-lm(pco6~temp+cond+log(temp):cond,data=alldata)
tempo<-lm(pco6~log(chla),data=alldata)
tempo<-lm(pco6~temp+log(chla)+log(chla):temp,data=alldata)
tempo<-lm(pco6~temp+cond+temp:cond+log(chla)+temp:log(chla),data=alldata)
tempo<-lm(pco6~do,data=alldata)
tempo<-lm(pco6~temp+do+do:temp,data=alldata)
tempo<-lm(pco6~temp+cond+temp:cond+do+do:temp,data=alldata)
tempo<-lm(pco6~cfpom,data=alldata)
tempo<-lm(pco6~cfpom+temp+temp:cfpom,data=alldata)
tempo<-lm(pco6~temp+cond+temp:cond+cfpom+cfpom:temp,data=alldata)
tempo<-lm(pco6~cod,data=alldata)
tempo<-lm(pco6~cod+temp+temp:cod,data=alldata)
tempo<-lm(pco6~temp+cond+temp:cond+cod+cod:temp,data=alldata)
tempo<-lm(pco6~turb,data=alldata)
tempo<-lm(pco6~temp+turb+turb:temp,data=alldata)
tempo<-lm(pco6~temp+cond+temp:cond+turb+turb:temp,data=alldata)
tempo<-lm(pco6~vel,data=alldata)
tempo<-lm(pco6~temp+vel+vel:temp,data=alldata)
tempo<-lm(pco6~temp+cond+temp:cond+vel+vel:temp,data=alldata)
tempo<-lm(pco6~nh3,data=alldata)
tempo<-lm(pco6~temp+nh3+nh3:temp,data=alldata)
tempo<-lm(pco6~temp+cond+temp:cond+nh3+nh3:temp,data=alldata)
tempo<-lm(pco6~toc,data=alldata)
tempo<-lm(pco6~temp+toc+toc:temp,data=alldata)
tempo<-lm(pco6~temp+cond+temp:cond+toc+toc:temp,data=alldata)
tempo<-lm(pco6~ph2,data=alldata)
tempo<-lm(pco6~temp+ph2+ph2:temp,data=alldata)
tempo<-lm(pco6~temp+cond+temp:cond+ph2+ph2:temp,data=alldata)
tempo<-lm(pco6~sed,data=alldata)

## final models for pco6
tempo<-lm(pco6~do+do:temp+temp,data=alldata)##	Adjusted R-squared:  0.1395 
summary(tempo)
tempo6<-cbind(cbind(fitted(tempo),residuals(tempo)),alldata[complete.cases(alldata[,c("do")]),])
attributes(tempo6)
names(tempo6[,1:2])<-c("fitted","residual")
edit(tempo6)
tempo6[,"2"]
tempo7<-lm(tempo6[,"2"]~pred6,data=tempo6)
summary(tempo7)

par(mar=c(5,5,4,2),cex=0.9)
plot(tempo6[tempo6$day==1,"dist"],tempo6[tempo6$day==1,"1"],type = "b",ylim=c(-0.6,0.6),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO6",main="prediction with other variables interaction",col="gold2",lwd=3,lty=4)
points(tempo6[tempo6$day==92,"dist"],tempo6[tempo6$day==92,"1"],type="b",col="blue",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo6[tempo6$day==210,"dist"],tempo6[tempo6$day==210,"1"],type="b",col="green3",pch=15,cex=1.5,lwd=3)
points(tempo6[tempo6$day==283,"dist"],tempo6[tempo6$day==283,"1"],type="b",col="red",pch=17,cex=2,lwd=3,lty=5)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=4,horiz=FALSE,cex=0.7,title="months")
abline(v=10.9,lty=2)

plot(alldata$do~alldata$pco6)
cor.test(alldata$do,alldata$pco6)
plot(log(alldata$do),alldata$pco6)
cor.test(log(alldata$do),alldata$pco6)

###”€”€”######”€”€”######”€”€”######”€”€”###
###”€”€”€”€”€Shannon index
shannon<-diversity(myrbug, index = "shannon", MARGIN = 1, base = exp(1))
shanmyrbug<-cbind(myrenv,shannon)
plot(myrenv$dist[1:8],shannon[1:8],xlab = "sites", ylab = "Shannon index",type="b",pch=19,ylim = c(0,3.5),col="black",lwd=3,lty=1)
points(myrenv$dist[9:16],shannon[9:16],xlab = "sites", ylab = "Shannon index",type="b",pch=8,col="blue",lwd=3,cex=1.2,lty=2)
points(myrenv$dist[17:24],shannon[17:24],xlab = "sites", ylab = "Shannon index",type="b",pch=15,col="green3",cex=1.2,lwd=3,lty=3)
points(myrenv$dist[25:32],shannon[25:32],xlab = "sites", ylab = "Shannon index",type="b",col="red",pch=17,cex=1.2,lwd=3,lty=4)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,8,15,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)
edit(shanmyrbug)

##Save shannon index into a CSV file
write.csv(shanmyrbug)
write.csv(shanmyrbug, file = "Shannon.csv")

###”€”€”###PCOs and Diversity indices
#PCO1
plot(pmyrbug[,"pco1"],shanmyrbug[,"shannon"])
plot(pmyrbug[,"pco1"],diversity[,"signal"])
plot(pmyrbug[,"pco1"],diversity[,"taxarich"])
plot(pmyrbug[,"pco1"],diversity[,"abundance"])
plot(pmyrbug[,"pco1"],diversity[,"eptnum"])
plot(pmyrbug[,"pco1"],diversity[,"eptrich"])

pmyrdiver<-cbind(diversity,pmyrbug)
pmyrdiver1<-cbind(shanmyrbug,pmyrdiver)
model1 <- lm(pmyrdiver1$pco1 ~ pmyrdiver1$shannon)
summary(model1)
anova(model1)
plot(pmyrdiver1$pco1 ~ pmyrdiver1$shannon)
abline(model1)

model1 <- lm(pmyrdiver1$pco1 ~ pmyrdiver1$signal)
summary(model1)
plot(pmyrdiver1$pco1 ~ pmyrdiver1$signal)
abline(model1)

model1 <- lm(pmyrdiver1$pco1 ~ pmyrdiver1$abundance)
summary(model1)
plot(pmyrdiver1$pco1 ~ pmyrdiver1$abundance)
abline(model1)

plot(pmyrbug[,"pco1"],myrbug[,"oligo"])  
model1 <- lm(pmyrdiver1$pco1 ~ myrbug$oligo)
summary(model1)
plot(pmyrdiver$pco1 ~ myrbug$oligo)
abline(model1)

plot(pmyrbug[,"pco1"],myrbug[,"simulidae"])  
model1 <- lm(pmyrdiver1$pco1 ~ myrbug$simulidae)
summary(model1)
plot(pmyrdiver1$pco1 ~ myrbug$simulidae)
abline(model1)

plot(pwangbug[,"pco1"],wangbug[,"chironominae"])  
model1 <- lm(pwangdiver$pco1 ~ wangbug$chironominae)
summary(model1)
plot(pwangdiver$pco1 ~ wangbug$chironominae)
abline(model1)

#pco2
plot(pmyrdiver1[,"pco2"],pmyrdiver1[,"shannon"])
plot(pmyrdiver1[,"pco2"],pmyrdiver1[,"signal"])
plot(pmyrdiver1[,"pco2"],pmyrdiver1[,"taxarich"])
plot(pmyrdiver1[,"pco2"],pmyrdiver1[,"abundance"])
plot(pmyrdiver1[,"pco2"],pmyrdiver1[,"eptnum"])
plot(pmyrdiver1[,"pco2"],pmyrdiver1[,"eptrich"])

model1 <- lm(pmyrdiver1$pco2 ~ pmyrdiver1$shannon) #Checks on aggregate process and heat maps
summary(model1)
plot(pmyrdiver1$pco2 ~ pmyrdiver1$shannon)
abline(model1)

model1 <- lm(pmyrdiver1$pco2 ~ pmyrdiver1$signal) #Checks on aggregate process and heat maps
summary(model1)
plot(pmyrdiver1$pco2 ~ pmyrdiver1$signal)
abline(model1)

model1 <- lm(pmyrdiver1$pco2 ~ pmyrdiver1$taxarich) #Checks on aggregate process and heat maps
summary(model1)
plot(pmyrdiver1$pco2 ~ pmyrdiver1$taxarich)
abline(model1)

model1 <- lm(pmyrdiver1$pco2 ~ pmyrdiver1$abundance) #Checks on aggregate process and heat maps
summary(model1)
plot(pmyrdiver1$pco2 ~ pmyrdiver1$abundance)
abline(model1)

model1 <- lm(pmyrdiver1$pco2 ~ pmyrdiver1$eptnum) #Checks on aggregate process and heat maps
summary(model1)
plot(pmyrdiver1$pco2 ~ pmyrdiver1$eptnum)
abline(model1)

model1 <- lm(pmyrdiver1$pco2 ~ pmyrdiver1$eptrich) #Checks on aggregate process and heat maps
summary(model1)
plot(pmyrdiver1$pco2 ~ pmyrdiver1$eptrich)
abline(model1)

plot(pmyrbug[,"pco2"],myrbug[,"gripopterygidae"]) #Checks on aggregate process and heat maps
model1 <- lm(pmyrdiver1$pco2 ~ myrbug$gripopterygidae)
summary(model1)
plot(pmyrdiver1$pco2 ~ myrbug$gripopterygidae)
abline(model1)

plot(pmyrbug[,"pco2"],myrbug[,"oligo"]) #Checks on aggregate process and heat maps
model1 <- lm(pmyrdiver1$pco2 ~ myrbug$oligo)
summary(model1)
plot(pmyrdiver$pco2~ myrbug$oligo)
abline(model1)

plot(pmyrbug[,"pco2"],myrbug[,"simulidae"])  # Checks on aggregate process and heat maps
model1 <- lm(pmyrdiver$pco2 ~ myrbug$simulidae)
summary(model1)
plot(pmyrdiver$pco2 ~ myrbug$simulidae)
abline(model1)

plot(pmyrbug[,"pco2"],myrbug[,"chironominae"])  # Checks on aggregate process and heat maps
model1 <- lm(pmyrdiver$pco2 ~ myrbug$chironominae)
summary(model1)
plot(pmyrdiver$pco2 ~ myrbug$chironominae)
abline(model1)

#PCO3
plot(pmyrbug[,"pco3"],pmyrdiver1[,"shannon"])
plot(pmyrbug[,"pco3"],pmyrdiver1[,"signal"])
plot(pmyrbug[,"pco3"],pmyrdiver1[,"taxarich"])
plot(pmyrbug[,"pco3"],pmyrdiver1[,"abundance"])
plot(pmyrbug[,"pco3"],pmyrdiver1[,"eptnum"])
plot(pmyrbug[,"pco3"],pmyrdiver1[,"eptrich"])

model1 <- lm(pmyrdiver1$pco3 ~ pmyrdiver1$shannon)
summary(model1)
plot(pmyrdiver1$pco3 ~ pmyrdiver1$shannon)
abline(model1)

model1 <- lm(pmyrdiver1$pco3 ~ pmyrdiver1$signal)
summary(model1)
plot(pmyrdiver1$pco3 ~ pmyrdiver1$signal)
abline(model1)

model1 <- lm(pmyrdiver1$pco3 ~ pmyrdiver1$taxarich)
summary(model1)
plot(pmyrdiver1$pco3 ~ pmyrdiver1$taxarich)
abline(model1)

model1 <- lm(pmyrdiver1$pco3 ~ pmyrdiver1$abundance)
summary(model1)
plot(pmyrdiver1$pco3 ~ pmyrdiver1$abundance)
abline(model1)

model1 <- lm(pmyrdiver1$pco3 ~ pmyrdiver1$eptnum)
summary(model1)
plot(pmyrdiver1$pco3 ~ pmyrdiver1$eptnum)
abline(model1)

model1 <- lm( pmyrdiver1$pco3 ~  pmyrdiver1$eptrich)
summary(model1)
plot( pmyrdiver1$pco3 ~  pmyrdiver1$eptrich)
abline(model1)

#PCO4
plot(pmyrbug[,"pco4"],pmyrdiver1[,"shannon"])
plot(pmyrbug[,"pco4"],pmyrdiver1[,"signal"])
plot(pmyrbug[,"pco4"],pmyrdiver1[,"taxarich"])
plot(pmyrbug[,"pco4"],pmyrdiver1[,"abundance"])
plot(pmyrbug[,"pco4"],pmyrdiver1[,"eptnum"])
plot(pmyrbug[,"pco4"],pmyrdiver1[,"eptrich"])

model1 <- lm(pmyrdiver1$pco4 ~ pmyrdiver1$shannon)
summary(model1)
plot(pmyrdiver1$pco4 ~ pmyrdiver1$shannon)
abline(model1)

model1 <- lm(pmyrdiver1$pco4 ~ pmyrdiver1$signal)
summary(model1)
plot(pmyrdiver1$pco4 ~ pmyrdiver1$signal)
abline(model1)

model1 <- lm(pmyrdiver1$pco4 ~ pmyrdiver1$taxarich)
summary(model1)
plot(pmyrdiver1$pco4 ~ pmyrdiver1$taxarich)
abline(model1)

model1 <- lm(pmyrdiver1$pco4 ~ pmyrdiver1$abundance)
summary(model1)
plot(pmyrdiver1$pco4 ~ pmyrdiver1$abundance)
abline(model1)

model1 <- lm(pmyrdiver1$pco4 ~ pmyrdiver1$eptnum)
summary(model1)
plot(pmyrdiver1$pco4 ~ pmyrdiver1$eptnum)
abline(model1)

model1 <- lm( pmyrdiver1$pco4 ~  pmyrdiver1$eptrich)
summary(model1)
plot( pmyrdiver1$pco4 ~  pmyrdiver1$eptrich)
abline(model1)

#PCO5
plot(pmyrbug[,"pco5"],pmyrdiver1[,"shannon"])
plot(pmyrbug[,"pco5"],pmyrdiver1[,"signal"])
plot(pmyrbug[,"pco5"],pmyrdiver1[,"taxarich"])
plot(pmyrbug[,"pco5"],pmyrdiver1[,"abundance"])
plot(pmyrbug[,"pco5"],pmyrdiver1[,"eptnum"])
plot(pmyrbug[,"pco5"],pmyrdiver1[,"eptrich"])

model1 <- lm(pmyrdiver1$pco5 ~ pmyrdiver1$shannon)
summary(model1)
plot(pmyrdiver1$pco5 ~ pmyrdiver1$shannon)
abline(model1)

model1 <- lm(pmyrdiver1$pco5 ~ pmyrdiver1$signal)
summary(model1)
plot(pmyrdiver1$pco5 ~ pmyrdiver1$signal)
abline(model1)

model1 <- lm(pmyrdiver1$pco5 ~ pmyrdiver1$taxarich)
summary(model1)
plot(pmyrdiver1$pco5 ~ pmyrdiver1$taxarich)
abline(model1)

model1 <- lm(pmyrdiver1$pco5 ~ pmyrdiver1$abundance)
summary(model1)
plot(pmyrdiver1$pco5 ~ pmyrdiver1$abundance)
abline(model1)

model1 <- lm(pmyrdiver1$pco5 ~ pmyrdiver1$eptnum)
summary(model1)
plot(pmyrdiver1$pco5 ~ pmyrdiver1$eptnum)
abline(model1)

model1 <- lm( pmyrdiver1$pco5 ~  pmyrdiver1$eptrich)
summary(model1)
plot( pmyrdiver1$pco5 ~  pmyrdiver1$eptrich)
abline(model1)

#PCO6
plot(pmyrbug[,"pco6"],pmyrdiver1[,"shannon"])
plot(pmyrbug[,"pco6"],pmyrdiver1[,"signal"])
plot(pmyrbug[,"pco6"],pmyrdiver1[,"taxarich"])
plot(pmyrbug[,"pco6"],pmyrdiver1[,"abundance"])
plot(pmyrbug[,"pco6"],pmyrdiver1[,"eptnum"])
plot(pmyrbug[,"pco6"],pmyrdiver1[,"eptrich"])

model1 <- lm(pmyrdiver1$pco6 ~ pmyrdiver1$shannon)
summary(model1)
plot(pmyrdiver1$pco6 ~ pmyrdiver1$shannon)
abline(model1)

model1 <- lm(pmyrdiver1$pco6 ~ pmyrdiver1$signal)
summary(model1)
plot(pmyrdiver1$pco6 ~ pmyrdiver1$signal)
abline(model1)

model1 <- lm(pmyrdiver1$pco6 ~ pmyrdiver1$taxarich)
summary(model1)
plot(pmyrdiver1$pco6 ~ pmyrdiver1$taxarich)
abline(model1)

model1 <- lm(pmyrdiver1$pco6 ~ pmyrdiver1$abundance)
summary(model1)
plot(pmyrdiver1$pco6 ~ pmyrdiver1$abundance)
abline(model1)

model1 <- lm(pmyrdiver1$pco6 ~ pmyrdiver1$eptnum)
summary(model1)
plot(pmyrdiver1$pco6 ~ pmyrdiver1$eptnum)
abline(model1)

model1 <- lm( pmyrdiver1$pco6 ~  pmyrdiver1$eptrich)
summary(model1)
plot( pmyrdiver1$pco6 ~  pmyrdiver1$eptrich)
abline(model1)


#######macroinvertebrates against distance
bugandenv<-cbind(myrenv,myrbug)
plot(bugandenv$Oligo,bugandenv$cond)
cor.test(bugandenv$Oligo,bugandenv$cond)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Baetidae"],type = "b",ylim=c(0,800),pch=19,cex=1.5,xlab="Distance(km)",ylab="baetidae",main="Baetidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Baetidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Baetidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Baetidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Caenidae"],type = "b",ylim=c(0,1100),pch=19,cex=1.5,xlab="Distance(km)",ylab="caenidae",main="Caenidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Caenidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Caenidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Caenidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Calamoceratidae"],type = "b",ylim=c(0,500),pch=19,cex=1.5,xlab="Distance(km)",ylab="calamoceratidae",main="Calamoceratidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Calamoceratidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Calamoceratidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Calamoceratidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Ceratopogonidae"],type = "b",ylim=c(0,20),pch=19,cex=1.5,xlab="Distance(km)",ylab="ceratopogonidae",main="Ceratopogonidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Ceratopogonidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Ceratopogonidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Ceratopogonidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Chironominae"],type = "b",ylim=c(0,200),pch=19,cex=1.5,xlab="Distance(km)",ylab="chironominae",main="Chironominae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Chironominae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Chironominae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Chironominae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Coloburiscidae"],type = "b",ylim=c(0,300),pch=19,cex=1.5,xlab="Distance(km)",ylab="coloburiscidae",main="Coloburiscidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Coloburiscidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Coloburiscidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Coloburiscidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Conoesucidae"],type = "b",ylim=c(0,50),pch=19,cex=1.5,xlab="Distance(km)",ylab="conoesucidae",main="Conoesucidaee plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Conoesucidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Conoesucidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Conoesucidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Corydalidae"],type = "b",ylim=c(0,50),pch=19,cex=1.5,xlab="Distance(km)",ylab="corydalidae",main="Corydalidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Corydalidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Corydalidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Corydalidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Crambidae"],type = "b",ylim=c(0,30),pch=19,cex=1.5,xlab="Distance(km)",ylab="crambidae",main="Crambidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Crambidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Crambidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Crambidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Ecnomidae"],type = "b",ylim=c(0,110),pch=19,cex=1.5,xlab="Distance(km)",ylab="ecnomidae",main="Ecnomidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Ecnomidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Ecnomidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Ecnomidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Elmidae"],type = "b",ylim=c(0,110),pch=19,cex=1.5,xlab="Distance(km)",ylab="elmidae",main="Elmidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Elmidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Elmidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Elmidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Empididae"],type = "b",ylim=c(0,200),pch=19,cex=1.5,xlab="Distance(km)",ylab="empididae",main="Empididae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Empididae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Empididae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Empididae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Glossosomatidae"],type = "b",ylim=c(0,80),pch=19,cex=1.5,xlab="Distance(km)",ylab="glossosomatidae",main="Glossosomatidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Glossosomatidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Glossosomatidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Glossosomatidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Hydroptilidae"],type = "b",ylim=c(0,1000),pch=19,cex=1.5,xlab="Distance(km)",ylab="hydroptilidae",main="Hydroptilidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Hydroptilidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Hydroptilidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Hydroptilidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Leptoceridae"],type = "b",ylim=c(0,800),pch=19,cex=1.5,xlab="Distance(km)",ylab="leptoceridae",main="Leptoceridae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Leptoceridae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Leptoceridae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Leptoceridae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Gomphidae"],type = "b",ylim=c(0,20),pch=19,cex=1.5,xlab="Distance(km)",ylab="gomphidae",main="Gomphidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Gomphidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Gomphidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Gomphidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Gripopterygidae"],type = "b",ylim=c(0,20),pch=19,cex=1.5,xlab="Distance(km)",ylab="gripopterygidae",main="Gripopterygidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Gripopterygidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Gripopterygidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Gripopterygidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)

par(mar=c(5,5,4,2),cex=0.9)
plot(bugandenv[bugandenv$day==1,"dist"],bugandenv[bugandenv$day==1,"Helicopsychidae"],type = "b",ylim=c(0,40),pch=19,cex=1.5,xlab="Distance(km)",ylab="helicopsychidae",main="Helicopsychidae plotted against spatial position",lwd=3,lty=5)
points(bugandenv[bugandenv$day==92,"dist"],bugandenv[bugandenv$day==92,"Helicopsychidae"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugandenv[bugandenv$day==210,"dist"],bugandenv[bugandenv$day==210,"Helicopsychidae"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugandenv[bugandenv$day==283,"dist"],bugandenv[bugandenv$day==283,"Helicopsychidae"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.8,title="months")
abline(v=10.9,lty=2)


######### heat map
sptree<-hclust(vegdist(t((myrbug)^0.25), "raup"), "average")

tiff(file="heatpco1_ungrouped.tif",width=5,height=5,units="in",pointsize = 12,bg = "transparent",res=800,compression="lzw")
tabasco((myrbug)^0.25,use=pmyrbug[,"pco1"],sp.ind=sptree,main="Macroinvertebrate taxa against PCO1 scores",cex.main=0.2,cexRow=0.3,cexCol=0.5)
dev.off()

cl <- function(x) quantile(x,seq(0,1,0.1))
apply(pmyrbug[,c("pco1","pco2","pco3","pco4","pco5","pco6")],2,cl)

breaks1<-cut(pmyrbug[,"pco1"],breaks=quantile(pmyrbug[,"pco1"],seq(0,1,0.1)))
myrbug.agg1<-aggregate(myrbug,by=list(breaks1),mean)
dim(myrbug.agg1)
attributes(myrbug.agg1)
myrbug.agg1$Group

breaks2<-cut(pmyrbug[,"pco2"],breaks=quantile(pmyrbug[,"pco2"],seq(0,1,0.1)))
myrbug.agg2<-aggregate(myrbug,by=list(breaks2),mean)
myrbug.agg2$Group

breaks3<-cut(pmyrbug[,"pco3"],breaks=quantile(pmyrbug[,"pco3"],seq(0,1,0.1)))
myrbug.agg3<-aggregate(myrbug,by=list(breaks3),mean)

breaks4<-cut(pmyrbug[,"pco4"],breaks=quantile(pmyrbug[,"pco4"],seq(0,1,0.1)))
myrbug.agg4<-aggregate(myrbug,by=list(breaks4),mean)

breaks5<-cut(pmyrbug[,"pco5"],breaks=quantile(pmyrbug[,"pco5"],seq(0,1,0.1)))
myrbug.agg5<-aggregate(myrbug,by=list(breaks5),mean)

breaks6<-cut(pmyrbug[,"pco6"],breaks=quantile(pmyrbug[,"pco6"],seq(0,1,0.1)))
myrbug.agg6<-aggregate(myrbug,by=list(breaks6),mean)

#*€*€*€*€* heat map *€*€*€*€*#
tiff(file="heatpco1.tif",width=5,height=5,units="in",pointsize = 12,bg = "transparent",res=1200,compression="lzw")
tabasco((as.data.frame(myrbug.agg1[,-1]))^0.25,use=as.numeric(rownames(myrbug.agg1)),sp.ind=sptree,labCol=myrbug.agg1$Group.1,main="Macroinvertebrate taxa against PCO1 scores",cex.main=0.2,cexRow=0.6,cexCol=0.5)
dev.off()

tiff(file="heatpco2.tif",width=5,height=5,units="in",pointsize = 12,bg = "transparent",res=1200,compression="lzw")
tabasco((as.data.frame(myrbug.agg2[,-1]))^0.25,use=as.numeric(rownames(myrbug.agg2)),sp.ind=sptree,labCol=myrbug.agg2$Group.1,main="PCO2",cex.main=0.2,cexRow=0.6,cexCol=0.5)
dev.off()

tiff(file="heatpco3.tif",width=5,height=5,units="in",pointsize = 12,bg = "transparent",res=1200,compression="lzw")
tabasco((as.data.frame(myrbug.agg3[,-1]))^0.25,use=as.numeric(rownames(myrbug.agg3)),sp.ind=sptree,labCol=myrbug.agg3$Group.1,main="PCO3",cex.main=0.2,cexRow=0.6,cexCol=0.5)
dev.off()

tiff(file="heatpco4.tif",width=5,height=5,units="in",pointsize = 12,bg = "transparent",res=1200,compression="lzw")
tabasco((as.data.frame(myrbug.agg4[,-1]))^0.25,use=as.numeric(rownames(myrbug.agg4)),sp.ind=sptree,labCol=myrbug.agg4$Group.1,main="PCO4",cex.main=0.2,cexRow=0.6,cexCol=0.5)
dev.off()

tiff(file="heatpco5.tif",width=5,height=5,units="in",pointsize = 12,bg = "transparent",res=1200,compression="lzw")
tabasco((as.data.frame(myrbug.agg5[,-1]))^0.25,use=as.numeric(rownames(myrbug.agg5)),sp.ind=sptree,labCol=myrbug.agg5$Group.1,main="PCO5",cex.main=0.2,cexRow=0.6,cexCol=0.5)
dev.off()

tiff(file="heatpco6.tif",width=5,height=5,units="in",pointsize = 12,bg = "transparent",res=1200,compression="lzw")
tabasco((as.data.frame(myrbug.agg6[,-1]))^0.25,use=as.numeric(rownames(myrbug.agg6)),sp.ind=sptree,labCol=myrbug.agg6$Group.1,main="PCO5",cex.main=0.2,cexRow=0.6,cexCol=0.5)
dev.off()

temp1<-cbind(breaks1,myrbug[,"Baetidae"])
temp1<-temp1[order(breaks1),]
colnames(temp1)<-c("a","b")
edit(temp1)
temp2<-aggregate(temp1[,"b"],by=list(temp1[,"a"]),mean)
(myrbug.agg1[,"Baetidae"])^0.25

#*€*€*€*€* paper#4-figure7 *€*€*€*€*#
maxbug<- apply(myrbug, 2, max)
head(maxbug)
n1 <- names(which(maxbug < 4))
n1
myrbug1 <- myrbug[, -which(names(myrbug) %in% n1)]
#edit(wangbug1)
dim(myrbug1)
dim(myrbug)
sptree<-hclust(vegdist(t((myrbug1)^0.25), "raup"), "average")

cl <- function(x) quantile(x,seq(0,1,0.1))
apply(pmyrbug[,c("pco1","pco2","pco3","pco4","pco5","pco6")],2,cl)

breaks1<-cut(pmyrbug[,"pco1"],breaks=quantile(pmyrbug[,"pco1"],seq(0,1,0.1)))
myrbug.agg1<-aggregate(myrbug1,by=list(breaks1),mean)
dim(myrbug.agg1)


tiff(file="paper#4-figure7.tif",width=5,height=5,units="in",pointsize = 12,bg = "transparent",res=1200,compression="lzw")
tabasco((as.data.frame(myrbug.agg1[,-1]))^0.25,use=as.numeric(rownames(myrbug.agg1)),sp.ind=sptree,labCol=myrbug.agg1$Group.1,main="",cex.main=0.2,cexRow=0.6,cexCol=0.5)
dev.off()


####Calculate fitted and residual for environmental variables
alldata<-cbind(envpcopred,pwangbug)#
edit(alldata)
tempo<-lm(pco1~temp+cond+temp:cond,data=alldata)
tempo<-lm(pco1~tn+op+alk+zn+chla+temp+cond+temp:cond,data=alldata)
tempo<-lm(pco1~tn+op+zn+temp+cond+temp:cond,data=alldata)
summary(tempo)
anova(tempo)
plot(tempo)
fitted(tempo)
edit(tempo1)
tempo1<-cbind(cbind(fitted(tempo),residuals(tempo)),alldata[complete.cases(alldata[,c("temp","cond")]),])
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")

par(mar=c(5,5,4,2),cex=0.9)
plot(tempo1[tempo1$day==126,"dist"],tempo1[tempo1$day==126,"1"],type = "b",ylim=c(-0.6,0.6),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO1",main="prediction with other variables and Temperature*Conductivity interaction",col="gold2",lwd=3,lty=4)
points(tempo1[tempo1$day==260,"dist"],tempo1[tempo1$day==260,"1"],type="b",col="blue",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==336,"dist"],tempo1[tempo1$day==336,"1"],type="b",col="green3",pch=17,cex=1.5,lwd=3)
points(tempo1[tempo1$day==518,"dist"],tempo1[tempo1$day==518,"1"],type="b",col="red",pch=18,cex=2,lwd=3,lty=5)
legend("topright",inset=c(0,0),legend=c("April 2014", "Aug 2014", "Nov 2014","May 2015"),lty=1,lwd=2,col=c("black","blue","green3","red"),ncol=4,horiz=FALSE,cex=0.7,title="months")
abline(v=4,lty=2)

xyplot(fitted(tempo)~dist, groups=day,data=tempo1,type="l",auto.key=TRUE)
xyplot(residuals(tempo)~dist, groups=day,data=tempo1,type="l",auto.key=TRUE)
tempo3<-lm(tempo1[,2]~tempo1$pred1)
summary(tempo3)

#*€*€*€*€* paper#4-figure8*€*€*€*€*#
edit(pmyrdiver1)

tiff(file="paper#4-figure8.tif",width=14,height=14,units="in",pointsize = 13,bg ="transparent",res=1000,compression="lzw")
par(mar=c(0.2,0.2,0.2,0.2),cex=0.6,cex.axis=1.2,las=1)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, cex=2, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  method=c("spearman")
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, cex=2, txt2)
}
pairs(~myrbug$Oligochaeta+sqrt(shannon)+sqrt(signal)+sqrt(taxarich)+sqrt(abundance)+sqrt(eptnum)+sqrt(eptrich)+pco1,data=pmyrdiver1,upper.panel=panel.cor,pch=20)
dev.off()

pmyrdiver1$pco1[pmyrdiver1$taxarich>2.5]

####Calculate fitted and residual for environmental variables
alldata<-cbind(envpcopred,pmyrbug)#
edit(alldata)
tempo<-lm(pco1~temp+cond+temp:cond+nh3+temp+ph2+ph2:temp,data=alldata)
summary(tempo)
anova(tempo)
plot(tempo)
fitted(tempo)
tempo1<-cbind(cbind(fitted(tempo),residuals(tempo)),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")

par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"pco1"],type = "b",ylim=c(-.6,1),pch=19,cex=1.5,xlab="Distance(km)",ylab="PCO1 score",main="PCO1 plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"pco1"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"pco1"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"pco1"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

xyplot(fitted(tempo)~dist, groups=day,data=tempo1,type="l",auto.key=TRUE)
xyplot(residuals(tempo)~dist, groups=day,data=tempo1,type="l",auto.key=TRUE)
tempo3<-lm(tempo1[,2]~tempo1$pred1)
summary(tempo3)

###”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€
###”€”€”€”€”land use 

alldata<-cbind(envpcopred,pmyrbug)# combine environmental and landuse data,PCO matrix and predicted pco matrix
edit(alldata)
dim(alldata)
names(alldata)
edit(myrenv)
names(myrenv)
landuse1<-myrenv[c(11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,35,37,38,43,45,47,48,53,54,56,57,62,64,66,67,72,73,75,76,81,82,84,90,91,93,94,99,100,102,103,108,109,111,112,117,118,120,121,126,127,129,130,135,136,138,139,144,177)]# land use(grazing, forest, creational and treatment)
landuse2<-log1p(landuse1)
edit(landuse2)
names(landuse2)
dim(landuse2)
landnut <- lm(ph2 ~ graz+ind+fore+rec+road+tra+un+res+wat+treat+graz1000+ind1000+for1000+rec1000+road1000+tra1000+un1000+res1000+wat1000+treat1000+graz500+ind500+for500+rec500+road500+tra500+un500+res500+wat500+treat500+graz300+ind300+for300+rec300+road300+tra300+un300+res300+wat300+treat300+graz100+ind100+for100+rec100+road100+tra100+un100+res100+wat100+treat100+graz60+ind60+for60+rec60+road60+tra60+un60+res60+wat60+treat60, data=myrenv,na.action=na.exclude)
landnutstep <- step(landnut, direction='forward')
summary (landnutstep, corr = TRUE )

landnut <- lm(log1p(no3) ~ eff+log1p(rec300)+log1p(treat100)+log1p(graz100)+log1p(treat60w), data=landuse1,na.action=na.exclude)
landnutstep <- step(landnut, direction='forward')
summary (landnut, corr = TRUE )
landnut <- lm(log1p(nh3) ~ eff+log1p(rec300)+log1p(treat100)+log1p(graz100)+log1p(treat60w), data=landuse1,na.action=na.exclude)
landnutstep <- step(landnut, direction='forward')
summary (landnutstep, corr = TRUE )
landnut <- lm(log1p(nh3) ~ eff+log1p(rec300)+log1p(treat100)+log1p(graz100)+log1p(treat60w), data=landuse1,na.action=na.exclude)
landnutstep <- step(landnut, direction='forward')
summary (landnutstep, corr = TRUE )
landnut <- lm(log1p(op) ~ eff+log1p(rec300)+log1p(treat100)+log1p(graz100)+log1p(treat60w), data=landuse1,na.action=na.exclude)
landnut <- step(landnut, direction='forward')
summary (landnut, corr = TRUE )
landnut <- lm(log1p(tp) ~ eff+log1p(rec300)+log1p(treat100)+log1p(graz100)+log1p(treat60w), data=landuse1,na.action=na.exclude)
landnut <- step(landnut, direction='forward')
summary (landnut, corr = TRUE )
landnut <- lm(log1p(tn) ~ eff+log1p(rec300)+log1p(treat100)+log1p(graz100)+log1p(treat60w), data=landuse1,na.action=na.exclude)
landnut <- step(landnut, direction='forward')
summary (landnut, corr = TRUE )
landnut <- lm(log1p(sali) ~ eff+log1p(rec300)+log1p(treat100)+log1p(graz100)+log1p(treat60w), data=landuse1,na.action=na.exclude)
landnut <- step(landnut, direction='forward')
summary (landnut, corr = TRUE )
landnut <- lm(log1p(cond) ~ eff+log1p(rec300)+log1p(treat100)+log1p(graz100)+log1p(treat60w), data=landuse1,na.action=na.exclude)
landnut <- step(landnut, direction='forward')
summary (landnut, corr = TRUE )
landnut <- lm(log1p(turb) ~ eff+log1p(rec300)+log1p(treat100)+log1p(graz100)+log1p(treat60w), data=landuse1,na.action=na.exclude)
landnut <- step(landnut, direction='forward')
summary (landnut, corr = TRUE )
landnut <- lm(sqrt(cod) ~ eff+log1p(rec300)+log1p(treat100)+log1p(treat100w)+log1p(graz100)+log1p(treat60w)+log1p(treat60), data=landuse1,na.action=na.exclude)
landnut <- step(landnut, direction='forward')
summary (landnut, corr = TRUE )
landnut <- lm(log1p(toc) ~ eff+log1p(rec300)+log1p(treat100)+log1p(treat100w)+log1p(graz100)+log1p(treat60w)+log1p(treat60), data=landuse1,na.action=na.exclude)
landnut <- step(landnut, direction='forward')
summary (landnut, corr = TRUE )
landnut <- lm(log1p(zn) ~ eff+log1p(res1000)+log1p(treat1000)+log1p(treat500)+log1p(road1000)+log1p(treat1000)+log1p(ind), data=wangenv,na.action=na.exclude)
landnut <- step(landnut, direction='forward')
summary (landnut, corr = TRUE )

###”€”€”€”€”€land use scatterplot Matrices
######land use and ph
tiff(file="landuse variables and environmental variables",width=14,height=14,units="in",pointsize = 12,bg ="transparent",res=800,compression="lzw")
par(mar=c(0.2,0.2,0.2,0.2),cex=0.6,cex.axis=0.8,las=1)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  method=c("spearman")
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}
pairs(~ ph2+graz60+ind60+road60+tra60+un60+res60,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(ph2 ~ graz100+ind100+for100+road100+tra100+un100+res100,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(ph2 ~ graz300+ind300+for300+road300+tra300+un300+res300,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(ph2 ~ graz500+ind500+for500+road500+tra500+un500+res500,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(ph2 ~ graz1000+ind1000+for1000+rec1000+road1000+tra1000+un1000+res1000,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(ph2 ~ graz+ind+fore+rec+road+tra+un+res+wat+treat,data=myrenv,upper.panel=panel.cor,pch=20)

######land use and cond
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  method=c("spearman")
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}
pairs(~ log(cond)+graz60+ind60+road60+tra60+un60+res60,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(log(cond) ~ graz100+ind100+for100+road100+tra100+un100+res100,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(log(cond) ~ graz300+ind300+for300+road300+tra300+un300+res300,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(log(cond) ~ graz500+ind500+for500+road500+tra500+un500+res500,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(log(cond)~ graz1000+ind1000+for1000+rec1000+road1000+tra1000+un1000+res1000,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(log(cond)~ graz+ind+fore+rec+road+tra+un+res+wat+treat,data=myrenv,upper.panel=panel.cor,pch=20)

######land use and temp
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  method=c("spearman")
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}
pairs(~ temp+graz60+ind60+road60+tra60+un60+res60,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(temp ~ graz100+ind100+for100+road100+tra100+un100+res100,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(temp ~ graz300+ind300+for300+road300+tra300+un300+res300,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(temp ~ graz500+ind500+for500+road500+tra500+un500+res500,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(temp ~ graz1000+ind1000+for1000+rec1000+road1000+tra1000+un1000+res1000,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(temp ~ graz+ind+fore+rec+road+tra+un+res+wat+treat,data=myrenv,upper.panel=panel.cor,pch=20)

######land use and alkalinity
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  method=c("spearman")
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}
pairs(~ log(alk)+graz60+ind60+road60+tra60+un60+res60,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(log(alk) ~ graz100+ind100+for100+road100+tra100+un100+res100,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(log(alk)~ graz300+ind300+for300+road300+tra300+un300+res300,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(log(alk) ~ graz500+ind500+for500+road500+tra500+un500+res500,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(log(alk) ~ graz1000+ind1000+for1000+rec1000+road1000+tra1000+un1000+res1000,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(log(alk) ~ graz+ind+fore+rec+road+tra+un+res+wat+treat,data=myrenv,upper.panel=panel.cor,pch=20)

######land use and ammonia
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  method=c("spearman")
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}
pairs(~ log(nh3)+graz60+ind60+road60+tra60+un60+res60,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(nh3 ~ graz100+ind100+for100+road100+tra100+un100+res100,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(nh3 ~ graz300+ind300+for300+road300+tra300+un300+res300,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(nh3 ~ graz500+ind500+for500+road500+tra500+un500+res500,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(nh3 ~ graz1000+ind1000+for1000+rec1000+road1000+tra1000+un1000+res1000,data=myrenv,upper.panel=panel.cor,pch=20)
pairs(nh3 ~ graz+ind+fore+rec+road+tra+un+res+wat+treat,data=myrenv,upper.panel=panel.cor,pch=20)



#”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€
#”€”€”€”€”€ Temperature 

#Temperature plotted against spatial position
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"temp"],type = "b",ylim=c(0,30),pch=19,cex=1.5,xlab="Distance(km)",ylab="Temperature (°C)",main="Temperature plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"temp"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"temp"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"temp"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

# Multiple regression analysis for Temperature vs time, distance and eff to check spatiotemporal variation of Temperature
# observed Temperature plots with spatiotemporal model prediction
tempo<-lm(temp~time,data=alldata) ## yes  ## with Adjusted R-squared:  0.9955
tempo<-lm(temp~eff,data=alldata)  ## no
tempo<-lm(temp~eff:time,data=alldata) ## yes ## with Adjusted R-squared:  0.4142 
tempo<-lm(temp~buffalo,data=alldata)  ## yes ##Adjusted R-squared:  0.6914
tempo<-lm(temp~buffalo:time,data=alldata) ## yes ## Adjusted R-squared:  0.6914
tempo<-lm(temp~eff:dist,data=alldata)    ## no
tempo<-lm(temp~time+buffalo:time,data=alldata)  ## yes ## Adjusted R-squared:  0.9959

anova(tempo)
summary(tempo)
plot(tempo)
avPlots(tempo)

tempo<-lm(temp~eff:dist:time,data=alldata)
tempo<-lm(temp~time+buffalo:time,data=alldata) ##
tempo<-lm(temp~time+buffalo:time+eff:dist:time,data=alldata) ## 
tempo<-lm(temp~time+buffalo:time+eff:dist:time+eff:I(dist^2):time,data=alldata)
tempo<-lm(temp~time+buffalo:time+dist:buffalo:time,data=alldata) ##
tempo<-lm(temp~time+buffalo:time:dist+creek:I(dist^2):time,data=alldata)

tempo<-lm(temp~veg60m:time:dist+time+time:dist+buffalo:time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time,data=alldata)
tempo<-lm(temp~time,data=alldata)
tempo<-lm(temp~creek,data=alldata)
tempo<-lm(temp~buffalo,data=alldata)
tempo<-lm(temp~dist,data=alldata)
tempo<-lm(temp~creek:time,data=alldata)
tempo<-lm(temp~buffalo:time,data=alldata)
tempo<-lm(temp~time+dist:time,data=alldata)
tempo<-lm(temp~buffalo:time:I(dist==10.4)+creek:time,data=alldata)
tempo<-lm(temp~time+creek:time+buffalo:time+buffalo:I(dist==10.4):time+buffalo:I(dist^2):time,data=alldata)
tempo<-lm(temp~time+time:dist+buffalo:time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time,data=alldata)
tempo<-lm(temp~time+time:dist+buffalo:time+buffalo:dist+buffalo:dist:time+creek:time,data=alldata)
tempo<-lm(temp~time+creek:time+time:dist+buffalo:time+buffalo:dist,data=alldata)
tempo1<-lm(temp~time+dist+buffalo:dist:time+creek:time,data=alldata)
tempo<-lm(temp~time+buffalo:time+creek:time,data=alldata)#final model ##*€*€*€*€* Paper#4-appendix 1-Table 2 *€*€*€*€*##

# final model ##*€*€*€*€* Paper#4-appendix 1-Table 2 *€*€*€*€*##

tempo<-lm(temp~time+buffalo:time+creek:time,data=alldata)#final model 

anova(tempo)   ##*€*€*€*€* Paper#4-appendix 1-Table 2*€*€*€*€*##
summary(tempo) ##*€*€*€*€* Paper#4-appendix 1-Table 3*€*€*€*€*##
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"temp"],type="p",pch=19,col="black",xlab="Distance (km)",ylab="Temperature (°C)",main="Temperature with spatiotemporal prediction",ylim=c(10,25),cex=1,lwd=3,lty=1)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"temp"],col="blue",pch=15,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"temp"],col="green3",pch=8,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"temp"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

###residual plot 
plot(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"2"],type = "b",ylim=c(0,5),pch=19,col="black",cex=1.5,xlab="Distance(km)",ylab="Temperature",main="residuals",lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"2"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"2"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"2"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
legend("topleft",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

plot(tempo) ## checking the diagnostic plots to check the multiple regresstion assumptions
qqnorm(tempo$residuals)
qqline(tempo$residuals)
hist(tempo$residuals)

###*€*€*€*€*# check the scatterplot for Temperature and water quality variables
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~temp+eff+dayflow+dist+veg30m+veg60m+veg90m,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

edit(alldata)
plot(alldata$temp,alldata$temp)
cor.test(alldata$temp,alldata$temp)

##*€*€*## check the scatterplot for Temperature and vegetation
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~log(temp)+temp+vel+depth+width+cfpom+sed+canop,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

##*€*€*## check the scatterplot for Temperature and rainfall
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~log(temp)+temp+rain1+rain2+rain3+dflowm+dflow3m+dayflow+weekflow+rflow+rflow3,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

#Temperature regression with other variables
tempo<-lm(temp~depth,data=alldata)  ## significant with Adjusted R-squared:  0.2638 
tempo<-lm(temp~rain2,data=alldata)  ## significant with Adjusted R-squared:  0.7396
tempo<-lm(temp~rain3,data=alldata)  ## significant with Adjusted R-squared:  0.9589
tempo<-lm(temp~dflowm,data=alldata) ## significant with Adjusted R-squared:  0.1443 
tempo<-lm(temp~dflow3m,data=alldata) ## significant with Adjusted R-squared:  0.8357
tempo<-lm(temp~weekflow,data=alldata) ## significant with Adjusted R-squared:  0.7766 
tempo<-lm(temp~dayflow,data=alldata)  ## significant with Adjusted R-squared:  0.4722
tempo<-lm(temp~rflow,data=alldata)    ## significant with Adjusted R-squared:  0.6195
tempo<-lm(temp~rflow3,data=alldata)   ## significant with Adjusted R-squared:  0.315
tempo<-lm(temp~airtemp,data=alldata)  ## yes ## Adjusted R-squared:  0.8428 
tempo<-lm(temp~solar,data=alldata)    ## yes ## Adjusted R-squared:  0.09937
tempo<-lm(temp~airtemp+solar+airtemp:solar,data=alldata) ## yes ## Adjusted R-squared:  0.9955 
tempo<-lm(temp~dayflow,data=alldata)  ## yes ## Adjusted R-squared:  0.4722 
tempo<-lm(temp~depth,data=alldata)    ## yes ## Adjusted R-squared:  0.2638  

summary(tempo)

# Multiple regression analysis for Temperature and variables affecting it to check which variables explain spatiotemporal variation in Temperature
#### Linear modelling for Temperature
tempo<-lm(temp~time+buffalo:time+creek:time,data=alldata)# final spatiotemporal model

tempo<-lm(temp~rain2+airtemp+dflow3m+weekflow+buffalo:dist:time+creek:time,data=alldata)
tempo<-lm(temp~rain2+airtemp+dflowm+buffalo:dist:time+creek:time+rflow,data=alldata) 
tempo<-lm(temp~rain2+airtemp+dflowm+buffalo:dist:time+creek:time,data=alldata)
tempo<-lm(temp~solar+airtemp+rain2+buffalo:dist:time+creek:time,data=alldata)
tempo<-lm(temp~solar+rain2+buffalo:dist:time+creek:time,data=alldata)

tempo<-lm(temp~airtemp+solar:airtemp+rain2+creek:time,data=alldata)

tempo1<-lm(temp~time+dist+buffalo:dist:time+creek:time,data=alldata)
tempo<-lm(temp~airtemp+rflow+rain2+buffalo:time+creek:time,data=alldata)
tempo<-lm(temp~airtemp+dflow3m+rflow+buffalo:time+creek:time,data=alldata)
tempo<-lm(temp~buffalo:time+creek:time,data=alldata)
tempo<-lm(temp~airtemp+buffalo:time+creek:time,data=alldata)
tempo<-lm(temp~weekflow+airtemp+dflow3m+buffalo:time+creek:time,data=alldata)
tempo<-lm(temp~airtemp+dflow3m+buffalo:time+creek:time,data=alldata)
tempo<-lm(temp~rain2+airtemp+dflowm+buffalo:dist:time+creek:time,data=alldata)
tempo<-lm(temp~rain2+airtemp+dflowm+buffalo:time+creek:time,data=alldata)# final model with environmental variables

#anova(tempo,tempo1) 

anova(tempo)   ##*€*€*€*€* Paper#4-appendix 1-Table 4 *€*€*€*€*##
summary(tempo) ##*€*€*€*€* Paper#4-appendix 1-Table 5 *€*€*€*€*##
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
#tempo1<-cbind(cbind(exp(fitted(tempo)),exp(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
#observed Temperature plots with spatiotemporal model predictions (including other variables)
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"temp"],type="p",pch=19,col="black",xlab="Distance (km)",ylab="Temperature (°C)",main="Temperature prediction with environmental variable",ylim=c(10,25),cex=1,lwd=3,lty=1)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"temp"],col="blue",pch=15,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"temp"],col="green3",pch=8,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"temp"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)

legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=4,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

options( scipen = 1 )
anova(tempo)
anova(tempo,tempo1)
summary(tempo)
avPlots(tempo)
plot(tempo) ## checking the diagnostic plots to check the multiple regresstion assumptions
qqnorm(tempo$residuals)
qqline(tempo$residuals)
hist(tempo$residuals)
tempo.check<-lm(residuals(tempo)~time+dist:dist+buffalo:dist:time+creek:time,data=alldata)
summary(tempo.check)
anova(tempo.check) # No clear evidence of spatiotemporal patters in residuals 

##*€*€*€*€* Temperature plot Paper#4-Figure9 *€*€*€*€*#

tiff(file="Paper#4-Figure9-Temperature.tif",width=6,height=8,units="in",pointsize = 12,bg ="transparent",res=1000,compression="lzw")
par(mfrow=c(3,1), mar=c(3,3,2,1.5),mai = c(0.5, 0.9, 0.3, 0.3),cex=1,cex.axis=0.8,las=1)

#Temperature plotted against spatial position
#par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"temp"],type = "b",ylim=c(10,25),pch=19,cex=0.8,xlab="",ylab="Temperature (°C)",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"temp"],type="b",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"temp"],type="b",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"temp"],type="b",col="red",pch=17,cex=1.2,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,2),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=4,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)


#Temperature with spatiotemporal model predictions
tempo<-lm(temp~time+buffalo:time+creek:time,data=alldata)# final spatiotemporal model
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
#observed Temperature plots with spatiotemporal model predictions
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"temp"],type = "p",ylim=c(10,25),pch=19,cex=0.8,xlab="",ylab="Temperature (°C)",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"temp"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"temp"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"temp"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.2,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.2,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=4,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

#observed Temperature plots with spatiotemporal model predictions (including other variables)
tempo<-lm(temp~rain2+airtemp+dflowm+buffalo:time+creek:time,data=alldata)# final model with environmental variables

anova(tempo)
summary(tempo)
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]

plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"temp"],type = "p",ylim=c(10,25),pch=19,cex=0.8,xlab="",ylab="Temperature (°C)",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"temp"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"temp"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"temp"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.2,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.2,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=4,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

dev.off()

#”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€
#”€”€”€”€”€ Conductivity 

#Conductivity plotted against spatial position
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"cond"],type = "b",ylim=c(20,60),pch=19,cex=1.5,xlab="Distance(km)",ylab="Conductivity (mg/l)",main="Conductivity plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"cond"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"cond"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"cond"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

# Multiple regression analysis for Conductivity vs time, distance and eff to check spatiotemporal variation of Conductivity
# observed Conductivity plots with spatiotemporal model prediction
tempo<-lm(cond~time,data=alldata) ## Adjusted R-squared:  0.8786
tempo<-lm(cond~eff,data=alldata)  ## no
tempo<-lm(cond~eff:time,data=alldata) ## Adjusted R-squared:  0.4228 
tempo<-lm(cond~buffalo,data=alldata)  ## no
tempo<-lm(cond~buffalo:time,data=alldata) ## Adjusted R-squared:  0.6365
tempo<-lm(cond~eff:dist,data=alldata)    ## no
tempo<-lm(cond~time+buffalo:time,data=alldata)  ## no

anova(tempo)
summary(tempo)
plot(tempo)
avPlots(tempo)

tempo<-lm(cond~eff:dist:time,data=alldata)  ## Adjusted R-squared:  0.3946
tempo<-lm(cond~time+buffalo:time,data=alldata) ##  no
tempo<-lm(cond~time+buffalo:time+eff:dist:time,data=alldata) ## 
tempo<-lm(cond~time+buffalo:time+eff:dist:time+eff:I(dist^2):time,data=alldata)
tempo<-lm(cond~time+buffalo:time+dist:buffalo:time,data=alldata) ##
tempo<-lm(cond~time+buffalo:time:dist+creek:I(dist^2):time,data=alldata)

tempo<-lm(cond~time,data=alldata)
tempo<-lm(cond~creek,data=alldata)
tempo<-lm(cond~buffalo,data=alldata)
tempo<-lm(cond~dist,data=alldata)
tempo<-lm(cond~creek:time,data=alldata) ## Adjusted R-squared:  0.8338 
tempo<-lm(cond~buffalo:time,data=alldata)
tempo<-lm(cond~time+dist:time,data=alldata)
tempo<-lm(cond~time+creek:time+time:dist:eff+buffalo:time+buffalo:dist,data=alldata)
tempo<-lm(cond~time+creek:time+time:dist+buffalo:time:dist,data=alldata)
tempo1<-lm(cond~time+time:dist+buffalo:time+creek:time,data=alldata)

anova(tempo,tempo1)
summary(tempo)
plot(tempo)
avPlots(tempo)

##*€*€*€*€* Paper#4-appendix 1-Table 6 *€*€*€*€*##
tempo<-lm(cond~time+dist+time:dist+buffalo:time+creek:time,data=alldata)# final model ##*€*€*€*€* Paper#4-appendix 1-Table 6 *€*€*€*€*##
anova(tempo)   ##*€*€*€*€* Paper#4-appendix 1-Table 6*€*€*€*€*##
summary(tempo) ##*€*€*€*€* Paper#4-appendix 1-Table 7*€*€*€*€*##
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"cond"],type="p",pch=19,col="black",xlab="Distance (km)",ylab="Conductivity (mg/l)",main="Conductivity with spatiotemporal prediction",ylim=c(20,60),cex=1,lwd=3,lty=1)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"cond"],col="blue",pch=15,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"cond"],col="green3",pch=8,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"cond"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

###residual plot 
plot(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"2"],type = "b",ylim=c(-3,60),pch=19,col="black",cex=1.5,xlab="Distance(km)",ylab="Conductivity",main="residuals",lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"2"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"2"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"2"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
legend("topleft",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

plot(tempo) ## checking the diagnostic plots to check the multiple regresstion assumptions
qqnorm(tempo$residuals)
qqline(tempo$residuals)
hist(tempo$residuals)

###*€*€*€*€*# check the scatterplot for Conductivity and water quality variables
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~cond+eff+dayflow+dist+veg30m+veg60m+veg90m,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

##*€*€*## check the scatterplot for Conductivity and vegetation
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~log(cond)+cond+no3+tp+alk+vel+depth+width+cfpom+sed+canop+buffalo,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

##*€*€*## check the scatterplot for Conductivity and rainfall
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~log(cond)+cond+rain1+rain2+rain3+dflowm+dflow3m+dayflow+weekflow+rflow+rflow3,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

#Conductivity regression with other variables
tempo<-lm(cond~temp,data=alldata)  ## significant with Adjusted R-squared:  0.7882
tempo<-lm(cond~depth,data=alldata)  ## significant with Adjusted R-squared:  0.162
tempo<-lm(cond~rain2,data=alldata)  ## significant with Adjusted R-squared:  0.6701
tempo<-lm(cond~rain3,data=alldata)  ## significant with Adjusted R-squared:  0.8503 
tempo<-lm(cond~dflowm,data=alldata) ## no 
tempo<-lm(cond~dflow3m,data=alldata) ## significant with Adjusted R-squared:  0.5633
tempo<-lm(cond~weekflow,data=alldata) ## significant with Adjusted R-squared:   0.46 
tempo<-lm(cond~dayflow,data=alldata)  ## significant with Adjusted R-squared:  0.6708
tempo<-lm(cond~rflow,data=alldata)    ## significant with Adjusted R-squared:  0.2659
tempo<-lm(cond~rflow3,data=alldata)   ## no 
tempo<-lm(cond~dayflow,data=alldata)  ##  ## significant with Adjusted R-squared:  0.6708

anova(tempo)
summary(tempo)

##########Checking correlation
plot(alldata$tp,alldata$cond)
cor.test(alldata$tp,alldata$cond)
plot(alldata$tn,alldata$cond)
cor.test(alldata$tn,alldata$cond)
plot(alldata$alk,alldata$cond)
cor.test(alldata$alk,alldata$cond)
plot(alldata$rain1,alldata$cond)
cor.test(alldata$rain1,alldata$cond)
plot(alldata$rain2,alldata$cond)
cor.test(alldata$rain2,alldata$cond)
plot(alldata$rain3,alldata$cond)
cor.test(alldata$rain3,alldata$cond)
plot(alldata$ph2,alldata$cond)
cor.test(alldata$ph2,alldata$cond)
plot(alldata$temp,alldata$cond)
cor.test(alldata$temp,alldata$cond)

plot(alldata$tp,alldata$cond)
cor.test(alldata$tp,alldata$cond)
plot(log(alldata$tp),alldata$cond)
cor.test(log(alldata$tp),alldata$cond)

plot(alldata$tn,alldata$cond)
cor.test(alldata$tn,alldata$cond)
plot(log(alldata$tn),alldata$cond)
cor.test(log(alldata$tn),alldata$cond)

plot(alldata$temp,alldata$cond)
cor.test(alldata$temp,alldata$cond)
plot(log(alldata$temp),alldata$cond)
cor.test(log(alldata$temp),alldata$cond)

plot(alldata$alk,alldata$cond)
cor.test(alldata$alk,alldata$cond)
plot(log(alldata$alk),alldata$cond)
cor.test(log(alldata$alk),alldata$cond)

plot(alldata$rain2,alldata$cond)
cor.test(alldata$rain2,alldata$cond)
plot(log(alldata$rain2),alldata$cond)
cor.test(log(alldata$rain2),alldata$cond)

plot(alldata$rain3,alldata$cond)
cor.test(alldata$rain3,alldata$cond)
plot(log(alldata$rain3),alldata$cond)
cor.test(log(alldata$rain3),alldata$cond)

plot(alldata$rflow3,alldata$cond)
cor.test(alldata$rflow3,alldata$cond)
plot(log(alldata$rflow3),alldata$cond)
cor.test(log(alldata$rflow3),alldata$cond)

# Multiple regression analysis for Conductivity and variables affecting it to check which variables explain spatiotemporal variation in Conductivity
#### Linear modelling for Conductivity
tempo<-lm(cond~time+dist+time:dist+buffalo:time+creek:time,data=alldata)# final spatiotemporal model

tempo<-lm(cond~rain1+rain2+rain3+temp+alk+ph2+vel+tp+tn+rflow+dflow3m+dayflow+weekflow+creek:time,data=alldata) # 
tempo<-lm(cond~rain2+rain3+temp+alk+dayflow+creek:time,data=alldata)
tempo<-lm(cond~rain3+temp+alk+dayflow+creek:time+buffalo,data=alldata)
tempo<-lm(cond~rain3+temp+alk+dayflow+creek:time+buffalo:time,data=alldata)
tempo<-lm(cond~rain3+temp+alk+creek:time+buffalo:time,data=alldata)
tempo<-lm(cond~rain3:time+temp+alk+creek:time+buffalo:time,data=alldata)
tempo<-lm(cond~rain3+temp+alk+ph2+creek:time+buffalo:time,data=alldata)
tempo<-lm(cond~rain1+rain2+rain3+temp+alk+ph2+tp+tn+rflow+dayflow+weekflow+creek:time,data=alldata) 
tempo<-lm(cond~alk,data=alldata)
tempo<-lm(cond~rain1+rain2+rain3,data=alldata)
tempo<-lm(cond~rain2+dflowm+dayflow+alk+creek:alk:time+buffalo:time,data=alldata)
tempo<-lm(cond~rain3+dflowm+weekflow+alk+creek:alk:time+buffalo:time,data=alldata)
tempo<-lm(cond~rain3+dflowm+alk+creek:alk:time+buffalo:time,data=alldata)
tempo<-lm(cond~rain2+dflowm+alk+creek:alk:time+buffalo:time+dayflow:dist:time+temp,data=alldata) 
tempo<-lm(cond~dflowm+alk+creek:alk:time+buffalo:time+dayflow:dist:time+temp,data=alldata)
tempo<-lm(cond~dflowm+creek:alk:time+buffalo:time+dayflow:dist:time+temp,data=alldata) ### 
tempo<-lm(cond~dflowm+creek:alk:time+buffalo:time+temp,data=alldata) ### 
anova(tempo,tempo1) 

tempo<-lm(cond~time+temp+dflowm+alk+creek:time+buffalo:time,data=alldata) ### 
tempo<-lm(cond~time+temp+creek:time+buffalo:time,data=alldata) ### 
tempo<-lm(cond~dflowm+creek:alk:time+buffalo:time+dayflow:dist:time+temp,data=alldata) 
tempo<-lm(cond~dayflow:dist:time+alk:creek:time,data=alldata) 
tempo<-lm(cond~dayflow+time+dist:time+buffalo:time+creek:time,data=alldata) 
tempo<-lm(cond~time+dist:time+buffalo:time+creek:time,data=alldata) 

tempo<-lm(cond~temp+rain2+alk+dayflow+creek:time+buffalo:time,data=alldata) # 
tempo<-lm(cond~temp+rain2+alk+dayflow,data=alldata) # 
tempo<-lm(cond~temp+rain2+creek:time+buffalo:time,data=alldata) # 
tempo<-lm(cond~temp+rain1+rain2+creek:time+buffalo:time,data=alldata) # final model


anova(tempo)   ##*€*€*€*€* Paper#4-appendix 1-Table 8 *€*€*€*€*##
options("scipen"=10, "digits"=2)
summary(tempo) ##*€*€*€*€* Paper#4-appendix 1-Table 9 *€*€*€*€*##
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
#tempo1<-cbind(cbind(exp(fitted(tempo)),exp(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
#observed Conductivity plots with spatiotemporal model predictions (including other variables)
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"cond"],type="p",pch=19,col="black",xlab="Distance (km)",ylab="Conductivity (mg/l)",main="Conductivity prediction with environmental variable",ylim=c(10,60),cex=1,lwd=3,lty=1)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"cond"],col="blue",pch=15,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"cond"],col="green3",pch=8,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"cond"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)

#legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

xyplot(cond~time,groups=dist,data=alldata,type="b",pch=c(1:6),auto.key=list(columns=4))
xyplot(cond~dist,groups=time,data=alldata,type="b",auto.key=list(columns=5))
xyplot(cond~no,groups=time,data=alldata,type="b",auto.key=list(columns=5))

options( scipen = 1 )
anova(tempo)
anova(tempo,tempo1)
summary(tempo)
avPlots(tempo)
plot(tempo) ## checking the diagnostic plots to check the multiple regresstion assumptions
qqnorm(tempo$residuals)
qqline(tempo$residuals)
hist(tempo$residuals)
tempo.check<-lm(residuals(tempo)~time+dist+time:dist+buffalo:time+creek:time,data=alldata)
summary(tempo.check)
anova(tempo.check) # No clear evidence of spatiotemporal patters in residuals 

##*€*€*€*€* Conductivity plot € Paper#4-Figure10 *€*€*€*€*#

tiff(file="Paper#4-Figure10-Conductivity.tif",width=6,height=8,units="in",pointsize = 12,bg ="transparent",res=1000,compression="lzw")
par(mfrow=c(3,1), mar=c(3,3,2,1.5),mai = c(0.5, 0.9, 0.3, 0.3),cex=1,cex.axis=0.8,las=1)

#par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"cond"],type = "b",ylim=c(10,60),pch=19,cex=0.8,xlab="",ylab="Conductivity (mg/l)",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"cond"],type="b",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"cond"],type="b",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"cond"],type="b",col="red",pch=17,cex=1.2,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

#Conductivity with spatiotemporal model predictions
tempo<-lm(cond~time+dist+dist:time+buffalo:time+creek:time,data=alldata)# final spatiotemporal model
anova(tempo)
summary(tempo)
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
#observed Conductivity plots with spatiotemporal model predictions
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"cond"],type = "p",ylim=c(10,60),pch=19,cex=0.8,xlab="",ylab="Conductivity (mg/l)",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"cond"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"cond"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"cond"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.2,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.2,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

#observed Conductivity plots with spatiotemporal model predictions (including other variables)
tempo<-lm(cond~temp+rain1+rain2+creek:time+buffalo:time,data=alldata) # final model # final model with environmental variables
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]

plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"cond"],type = "p",ylim=c(10,60),pch=19,cex=0.8,xlab="",ylab="Conductivity (mg/l)",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"cond"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"cond"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"cond"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.2,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.2,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

dev.off()

#”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€
#”€”€”€”€”€ ammonia (nh3) 

#ammonia plotted against spatial position
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"nh3"],type = "b",ylim=c(0.05,0.15),pch=19,cex=0.8,xlab="Distance(km)",ylab="Ammonia (mg/L)",main="NH3 plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"nh3"],type="b",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"nh3"],type="b",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"nh3"],type="b",col="red",pch=17,cex=1.2,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

# Multiple regression analysis for ammonia vs time, distance and eff to check spatiotemporal variation of ammonia
# observed ammonia plots with spatiotemporal model prediction
tempo<-lm(log(nh3)~time+eff:time+eff:dist+eff:dist:time+eff:I(dist^2):time,data=alldata) # final model ##*€*€*€*€* 
tempo<-lm(log(nh3)~time+buffalo:time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time,data=alldata)
tempo<-lm(log(nh3)~time+buffalo:time+buffalo:dist:time,data=alldata) ## without "buffalo:dist" and "+"buffalo:I(dist^2):time"
tempo<-lm(log(nh3)~time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time,data=alldata) ## without "buffalo:time"
tempo<-lm(log(nh3)~time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time+eff:dist+eff:dist:time+eff:I(dist^2):time,data=alldata) ## without "buffalo:time"
tempo<-lm(log(nh3)~time+time3:I(dist^2):eff+buffalo:I(dist^2):time,data=alldata) ## without "buffalo:time"
tempo<-lm(log(nh3)~time+buffalo:I(dist^2):time,data=alldata)

tempo<-lm(nh3~time+buffalo:time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time,data=alldata)
tempo<-lm(nh3~time+buffalo:time+buffalo:dist:time,data=alldata) ## without "buffalo:dist" and "+"buffalo:I(dist^2):time"
tempo<-lm(nh3~time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time,data=alldata) ## without "buffalo:time"
tempo<-lm(nh3~time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time+eff:dist+eff:dist:time+eff:I(dist^2):time,data=alldata) ## without "buffalo:time"
tempo<-lm(nh3~time+time:I(dist^2):eff+buffalo:I(dist^2):time,data=alldata) ## without "buffalo:time"

tempo<-lm(nh3~time+buffalo:time+buffalo:dist:time,data=alldata) # 
tempo<-lm(nh3~buffalo:(time==3)+buffalo:dist:time+creek:(time==1),data=alldata)  
tempo1<-lm(nh3~time+buffalo:dist:time,data=alldata)
tempo<-lm(nh3~buffalo:(time==3)+creek:(time==1),data=alldata)
tempo<-lm(nh3~buffalo:(time==3)+creek:time,data=alldata) # final model ##*€*€*€*€*

anova(tempo)   ##*€*€*€*€* Paper#4-appendix-Table 10
summary(tempo) ##*€*€*€*€* Paper#4-appendix-Table 11
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
#observed nh3 plots with spatiotemporal model predictions
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"nh3"],type="p",pch=19,col="black",xlab="Distance (km)",ylab="Ammonia (mg/L)",main="Ammonia with spatiotemporal prediction",ylim=c(0.05,0.15),cex=1,lwd=3,lty=1)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"nh3"],col="blue",pch=15,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"nh3"],col="green3",pch=8,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"nh3"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
legend("topleft",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

###residual plot 
plot(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"2"],type = "b",ylim=c(0,0.2),pch=19,col="black",cex=1.5,xlab="Distance(km)",ylab="Ammonia",main="residuals",lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"2"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"2"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"2"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
legend("topleft",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

plot(tempo) ## checking the diagnostic plots to check the multiple regresstion assumptions
qqnorm(tempo$residuals)
qqline(tempo$residuals)
hist(tempo$residuals)

###*€*€*€*€*# check the scatterplot for nh3 and water quality variables
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~log(nh3)+nh3+ph2+log(alk)+eff+dayflow+dist+log(temp)+log(toc),data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

edit(alldata)
plot(alldata$nh3,alldata$temp)
cor.test(alldata$nh3,alldata$temp)

##*€*€*## check the scatterplot for nh3 and vegetation
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~log(nh3)+nh3+vel+depth+width+cfpom+sed+canop,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

##*€*€*## check the scatterplot for nh3 and rainfall
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~log(nh3)+nh3+veg30m+veg60m+veg90m+rain1+rain2+rain3+dflowm+dflow3m+dayflow+weekflow+rflow+rflow3,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

#ammonia regression with other variables
tempo<-lm(ph2~eff,data=alldata)##
tempo<-lm(nh3~dayflow,data=alldata)## 
tempo<-lm(nh3~dist,data=alldata)##
tempo<-lm(nh3~temp,data=alldata) ## significant with Adjusted R-squared:  0.2788
tempo<-lm(nh3~alk,data=alldata) ## significant with Adjusted R-squared:  0.1916 
tempo<-lm(nh3~ph2,data=alldata) ## significant with Adjusted R-squared:  0.5943 
tempo<-lm(nh3~depth,data=alldata) ## significant with Adjusted R-squared:0.1325 
tempo<-lm(nh3~rain2,data=alldata) ## significant with Adjusted R-squared:  0.3787
tempo<-lm(nh3~rain3,data=alldata) ## significant with Adjusted R-squared:  0.2399
tempo<-lm(nh3~dflow3m,data=alldata) ## significant with Adjusted R-squared:  0.1153
tempo<-lm(nh3~weekflow,data=alldata) ## significant with Adjusted R-squared:  0.3773
tempo<-lm(nh3~rflow,data=alldata) ## significant with Adjusted R-squared:  0.4064
tempo<-lm(nh3~rflow3,data=alldata) ## significant with Adjusted R-squared:  0.4064

summary(tempo)

# Multiple regression analysis for nh3 and variables affecting it to check which variables explain spatiotemporal variation in nh3
#### Linear modelling for nh3
tempo<-lm(nh3~buffalo:(time==3)+creek:time,data=alldata) 

tempo<-lm(nh3~temp:time+alk+ph2:time+depth+rain2:buffalo+rain3:buffalo+dflow3m:buffalo+weekflow:buffalo,data=alldata)

tempo<-lm(nh3~temp:time+alk+ph2:time+depth+rain2:buffalo+rain3:buffalo+dflow3m:buffalo+weekflow:buffalo,data=alldata)
tempo<-lm(nh3~temp:time,data=alldata)
tempo<-lm(nh3~alk+ph2:time,data=alldata)
tempo<-lm(nh3~temp+ph2:time,data=alldata)
tempo<-lm(nh3~temp+ph2:time+rain2:buffalo,data=alldata)
tempo<-lm(nh3~temp+ph2:time+dflow3m:buffalo,data=alldata)
tempo<-lm(nh3~temp+alk+ph2:time+dflow3m:buffalo,data=alldata)
tempo<-lm(nh3~temp+alk+ph2:time+weekflow:buffalo,data=alldata)
tempo<-lm(nh3~temp+dflow3m+buffalo:I(dist==10.4):time+ph2:time+weekflow:buffalo:rain2,data=alldata)
tempo<-lm(nh3~temp+dflow3m+buffalo:I(dist==10.4):time+ph2:time+alk:buffalo:time+rain2:ph2+rain3:ph2+rain2:alk+rain3:alk,data=alldata)
tempo<-lm(nh3~temp+dflow3m+buffalo:I(dist==10.4):time+ph2:buffalo:dist+ph2:buffalo:time+alk:buffalo:dist+rain2:ph2+rain3:ph2,data=alldata)
tempo<-lm(nh3~temp+dflow3m+buffalo:I(dist==10.4):time+ph2:buffalo:dist+ph2:buffalo:time+alk:buffalo:dist,data=alldata)
tempo<-lm(nh3~temp+dflow3m:time+rain3+rflow3+buffalo:I(dist==10.4):time+ph2:buffalo:dist+ph2:buffalo:time,data=alldata)
tempo<-lm(nh3~temp+dflow3m:time+weekflow+alk:I(dist==10.4):time+ph2:buffalo:dist+ph2:buffalo:time,data=alldata)
tempo1<-lm(nh3~temp+dflow3m:time+weekflow+alk:I(dist==10.4):time+ph2:buffalo:dist+ph2:buffalo:time,data=alldata)
tempo<-lm(nh3~temp+weekflow:time+rflow3:time+rain3+alk:I(dist==10.4):time+ph2:buffalo:dist+ph2:buffalo:time,data=alldata)

tempo<-lm(nh3~temp+weekflow:time+rflow3:time+buffalo:I(dist==10.4):time+ph2:buffalo:time+ph2:time:I(dist^2),data=alldata)
tempo<-lm(nh3~temp+weekflow:time+buffalo:I(dist==10.4):time+ph2:buffalo:time+ph2:time:I(dist^2),data=alldata)
tempo<-lm(nh3~temp+rflow3:time+buffalo:I(dist==10.4):time+ph2:buffalo:time+ph2:time:I(dist^2)+alk:time:I(dist^2),data=alldata)
tempo<-lm(nh3~temp+rflow3:time+buffalo:I(dist==10.4):time+ph2:buffalo:time+ph2:time:I(dist^2)+alk:time:dist,data=alldata)
tempo<-lm(nh3~temp+rflow3:time+buffalo:I(dist==10.4):time+ph2:buffalo:time+ph2:time:I(dist^2)+alk:time:dist,data=alldata)
tempo<-lm(nh3~buffalo:I(dist>=10.4):time,data=alldata) # good
tempo<-lm(nh3~buffalo:dist:time,data=alldata)
tempo<-lm(nh3~temp+rflow3:time,data=alldata)
tempo<-lm(nh3~ph2:buffalo:time,data=alldata)
tempo<-lm(nh3~ph2:time:I(dist^2),data=alldata) # good
tempo<-lm(nh3~alk:time:dist,data=alldata)      # good
tempo<-lm(nh3~buffalo:I(dist==10.4):time+alk:time:dist+ph2:time:I(dist^2)+ph2:buffalo:time,data=alldata)
tempo<-lm(nh3~buffalo:I(dist==10.4):time+ph2:time:I(dist^2),data=alldata)##
tempo<-lm(nh3~alk:time:dist+ph2:time:I(dist^2)+ph2:buffalo:time,data=alldata)##
tempo<-lm(nh3~alk:time:creek+ph2:time:I(dist^2)+alk:buffalo:time,data=alldata)##
tempo<-lm(nh3~alk:time:creek+ph2:(time==1):I(dist^2),data=alldata)##
tempo<-lm(nh3~temp:(time==1):creek+ph2:time:I(dist^2)+ph2:buffalo:(time==3),data=alldata)
tempo<-lm(nh3~temp:time:creek+ph2:buffalo:(time==3),data=alldata)
tempo<-lm(nh3~alk:time:creek+ph2:time:I(dist^2)+alk:buffalo:time,data=alldata)
tempo<-lm(nh3~ph2:temp+buffalo:(time==3)+creek:(time==1),data=alldata)
tempo<-lm(nh3~buffalo:(time==3)+creek:dist:time+creek:(time==1),data=alldata)##

anova(tempo)  
summary(tempo) 
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
#observed nh3 plots with spatiotemporal model predictions (including other variables)
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"nh3"],type="p",pch=19,col="black",xlab="Distance (km)",ylab="Ammonia (mg/L)",main="Ammonia prediction with environmental variable",ylim=c(0.05,0.15),cex=1,lwd=3,lty=1)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"nh3"],col="blue",pch=15,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"nh3"],col="green3",pch=8,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"nh3"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
legend("topleft",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

options( scipen = 1 )
anova(tempo)
anova(tempo,tempo1)
summary(tempo)
avPlots(tempo)
plot(tempo) ## checking the diagnostic plots to check the multiple regresstion assumptions
qqnorm(tempo$residuals)
qqline(tempo$residuals)
hist(tempo$residuals)
tempo.check<-lm(residuals(tempo)~time+time:buffalo+time:dist:buffalo,data=alldata)
summary(tempo.check)
anova(tempo.check) # No clear evidence of spatiotemporal patters in residuals 

#*€*€*€*€*# some more graphs *€*€*€*€*# check this part
a<-xyplot(nh3~dist,group=time,data=alldata,ylim=c(0,0.8),type="p",pch=19,lty=2,col=c("black","blue","green3","red"))
b<-xyplot(exp(fitted(tempo))~dist,group=time,data=alldata,type="l",lwd=2,col=c("black","blue","green3","red"))
c<-xyplot(toc~time,data=alldata,panel = function(x,y) {panel.abline(v=10.9,lty=2)}) 
a+as.layer(b)+as.layer(c)
legend("topleft",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")

xyplot(residuals(tempo)~dist,groups=time,data=alldata,type="b",pch=19,lwd=2,col=c("black","blue","green3","red"))

xyplot(no3~dist,group=time,data=alldata,type="b",pch=19,lty=2,col=c("black","blue","green3","red"))
xyplot(nh3~dist,group=time,data=alldata,type="b",pch=19,lty=2,col=c("black","blue","green3","red"))
xyplot(do~dist,group=time,data=alldata,type="b",pch=19,lty=2,col=c("black","blue","green3","red"))
xyplot(do~toc,groups=time,data=alldata,type="p",pch=19:24,lwd=2,col=c("black","blue","green3","red"))
xyplot(log(nh3)~ph2,groups=time,data=alldata,type="p",pch=19:24,lwd=2,col=c("black","blue","green3","red"))

cor.test(alldata$nh3,alldata$ph2)

##*€*€*€*€* Paper#4-Figure11 *€*€*€*€*#

tiff(file="Paper4-Figure11.tif",width=6,height=8,units="in",pointsize = 12,bg ="transparent",res=1000,compression="lzw")
par(mfrow=c(3,1), mar=c(3,3,2,1.5),mai = c(0.5, 0.9, 0.3, 0.3),cex=1,cex.axis=0.8,las=1)

#ammonia plotted against spatial position
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"nh3"],type = "b",ylim=c(0,0.15),pch=19,cex=0.8,xlab="",ylab="Ammonia (mg/L)",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"nh3"],type="b",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"nh3"],type="b",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"nh3"],type="b",col="red",pch=17,cex=1.2,lwd=3,lty=2)
#legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

#ammonia with spatiotemporal model predictions
#tempo<-lm(nh3~buffalo:(time==3)+creek:time,data=alldata) ## final model
tempo<-lm(nh3~buffalo:(time==3)+creek:time,data=alldata) ## final model
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
#observed nh3 plots with spatiotemporal model predictions
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"nh3"],type="p",pch=19,cex=0.8,col="black",xlab="",ylab="Ammonia (mg/L)",main="",ylim=c(0,0.15),lwd=3,lty=5)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"nh3"],col="blue",pch=8,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"nh3"],col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"nh3"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
#legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

#ammonia plots with spatiotemporal model predictions (including other variables)
#tempo<-lm(nh3~alk:time:dist+ph2:time:I(dist^2)+ph2:buffalo:time,data=alldata)##
tempo<-lm(nh3~buffalo:(time==3)+creek:time,data=alldata) ## final model
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
#observed nh3 plots with spatiotemporal model predictions (including other variables)
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"nh3"],type="p",pch=19,cex=0.8,col="black",xlab="",ylab="Ammonia (mg/L)",main="",ylim=c(0,0.15),lwd=3,lty=5)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"nh3"],col="blue",pch=8,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"nh3"],col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"nh3"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
#legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

dev.off()

#”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€
#”€”€”€”€”€ alkalinity 

#alkalinity plotted against spatial position
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"alk"],type = "b",ylim=c(0,30),pch=19,cex=1.5,xlab="Distance(km)",ylab="Alkalinity (mg/L)",main="Alkalinity plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"alk"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"alk"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"alk"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

# Multiple regression analysis for alkalinity vs time, distance and eff to check spatiotemporal variation of alkalinity
# observed alkalinity plots with spatiotemporal model prediction
tempo<-lm(alk~time,data=alldata) ## yes
tempo<-lm(alk~eff:time,data=alldata) ## no
tempo<-lm(alk~buffalo:time,data=alldata) ## yes
tempo<-lm(alk~+eff:dist,data=alldata) ## no
tempo<-lm(alk~time+buffalo:time,data=alldata) ## yes
tempo<-lm(alk~eff:dist:time,data=alldata)
tempo<-lm(alk~time+buffalo:time+eff:dist:time,data=alldata) ## yes
tempo<-lm(alk~time+buffalo:time+eff:dist:time+eff:I(dist^2):time,data=alldata)
tempo<-lm(alk~time+buffalo:time+eff:I(dist^2):time,data=alldata)
tempo<-lm(alk~time+eff:time+eff:dist:time+eff:I(dist^2):time,data=alldata) 
tempo<-lm(alk~buffalo:dist,data=alldata)
tempo<-lm(alk~buffalo+time+time:buffalo+time:buffalo:dist+eff:time+eff:dist+eff:dist:time+eff:I(dist^2):time,data=alldata) 
tempo<-lm(alk~time+eff:I(dist^2):time,data=alldata)
tempo<-lm(alk~time+time:dist+buffalo:time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time,data=alldata)
tempo<-lm(alk~time+time:dist+buffalo:time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time,data=alldata)

tempo<-lm(alk~veg60m:time:dist+time+time:dist+buffalo:time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time,data=alldata)
tempo<-lm(alk~time,data=alldata)
tempo<-lm(alk~creek,data=alldata)
tempo<-lm(alk~buffalo,data=alldata)
tempo<-lm(alk~dist,data=alldata)
tempo<-lm(alk~creek:time,data=alldata)
tempo<-lm(alk~buffalo:time,data=alldata)
tempo<-lm(alk~time+dist:time,data=alldata)
tempo<-lm(alk~buffalo:time:I(dist==10.4)+creek:time,data=alldata)
tempo<-lm(alk~time+creek:time+buffalo:time+buffalo:I(dist==10.4):time+buffalo:I(dist^2):time,data=alldata)
tempo<-lm(alk~time+time:dist+buffalo:time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time,data=alldata)

# final model ##*€*€*€*€* Paper#4-appendix-Table 12 and Table 13 *€*€*€*€*##
tempo<-lm(alk~time+time:dist+buffalo:time+buffalo:dist+buffalo:dist:time+buffalo:I(dist^2):time,data=alldata)
tempo<-lm(alk~time+buffalo:time+creek:time+I(dist^2):time,data=alldata)
tempo<-lm(alk~time+time:I(dist^2)+buffalo:time+creek:time,data=alldata)
tempo<-lm(alk~time+buffalo:time:dist+creek:time,data=alldata)
tempo<-lm(alk~time+buffalo:time+creek:time,data=alldata) # final model 

anova(tempo)  ##*€*€*€*€* Paper#4-appendix-Table 12 *€*€*€*€*##
summary(tempo)##*€*€*€*€* Paper#4-appendix-Table 13 *€*€*€*€*##
plot(tempo)
avPlots(tempo)

######################
library(caret)
library(mlbench)
library(mboost)

lmfit<-train(alk~time+buffalo:time+creek:time+I(dist^2):time,data=alldata)
lmfit<-train(alk~time+time:dist+buffalo:time+creek:time,data=alldata)
lmfit<-train(alk~time+buffalo:time+creek:time,data=alldata) ### final model

print(lmfit)
##mtry: Number of variables randomly sampled as candidates at each split.
##The Root Mean Squared Error (RMSE)

############################v
library(pander)
panderOptions('digits', 2)
panderOptions('round', 2)
panderOptions('keep.trailing.zeros', TRUE)

#################
anova.m1<-anova(tempo)
anova(tempo)##*€*€*€*€* Paper#4-appendix-Table 12 *€*€*€*€*##
pander(anova.m1)

summary.m1<-summary(tempo) ##*€*€*€*€* Paper#4-appendix-Table 13 *€*€*€*€*##
pander(summary.m1)

tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"alk"],type="p",pch=19,col="black",xlab="Distance (km)",ylab="Alkalinity (mg/L)",main="Alkalinity with spatiotemporal prediction",ylim=c(10,21),cex=1,lwd=3,lty=1)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"alk"],col="blue",pch=15,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"alk"],col="green3",pch=8,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"alk"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

###residual plot 
plot(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"2"],type = "b",ylim=c(0,40),pch=19,col="black",cex=1.5,xlab="Distance(km)",ylab="alkalinity",main="residuals",lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"2"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"2"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"2"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
legend("topleft",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

plot(tempo) ## checking the diagnostic plots to check the multiple regresstion assumptions
qqnorm(tempo$residuals)
qqline(tempo$residuals)
hist(tempo$residuals)

###*€*€*€*€*# check the scatterplot for alkalinity and water quality variables
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~alk+eff+dayflow+dist+log(temp)+log(toc),data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

edit(alldata)
plot(alldata$alk,alldata$temp)
cor.test(alldata$alk,alldata$temp)

##*€*€*## check the scatterplot for alkalinity and vegetation
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~log(alk)+alk+vel+depth+width+cfpom+sed+canop,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

##*€*€*## check the scatterplot for alkalinity and rainfall
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~log(alk)+alk+veg30m+veg60m+veg90m+rain1+rain2+rain3+dflowm+dflow3m+dayflow+weekflow+rflow+rflow3,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

#alkalinity regression with other variables
tempo<-lm(alk~eff,data=alldata)##
tempo<-lm(alk~dayflow,data=alldata)## 
tempo<-lm(alk~dist,data=alldata)##
tempo<-lm(alk~temp,data=alldata) ## significant with Adjusted R-squared:  0.8242
tempo<-lm(alk~ph2,data=alldata) ## significant with Adjusted R-squared:  0.2007
tempo<-lm(alk~depth,data=alldata) ## significant with Adjusted R-squared:  0.2341  
tempo<-lm(alk~rain2,data=alldata) ## significant with Adjusted R-squared:  0.8189
tempo<-lm(alk~rain3,data=alldata) ## significant with Adjusted R-squared:  0.9223
tempo<-lm(alk~dflow3m,data=alldata) ## significant with Adjusted R-squared:  0.5242
tempo<-lm(alk~weekflow,data=alldata) ## significant with Adjusted R-squared:  0.5532
tempo<-lm(alk~dayflow,data=alldata) ## significant with Adjusted R-squared:  0.7665
tempo<-lm(alk~rflow,data=alldata) ## significant with Adjusted R-squared:  0.3301
tempo<-lm(alk~rflow3,data=alldata) ## significant with Adjusted R-squared:  0.05343

summary(tempo)

# Multiple regression analysis for alkalinity and variables affecting it to check which variables explain spatiotemporal variation in alkalinity
#### Linear modelling for alkalinity

tempo<-lm(log(alk)~temp+rain2+rain3+dflow3m+dayflow+weekflow+rflow+depth,data=alldata)
tempo<-lm(log(alk)~temp+rain2+rain3+dflow3m+dayflow+weekflow+rflow+buffalo:dist:time,data=alldata)
tempo<-lm(alk~temp:time+rain2+rain3+dflow3m+dayflow:I(dist^2):time+weekflow+rflow,data=alldata)
tempo<-lm(alk~buffalo+creek+temp+rain1+rain2+rain3+dflow3m+weekflow+dayflow+rflow,data=alldata)
tempo<-lm(alk~creek:time+rain1+rain2+rain3+dflow3m+weekflow+dayflow+rflow+veg60m:dist:time,data=alldata)
tempo<-lm(alk~creek:time+rain1+rain2+rain3+weekflow+dayflow+rflow+veg60m:I(dist^2):time,data=alldata)
tempo<-lm(alk~creek:time+time:I(dist^2):weekflow+buffalo:time:dist+rain2+rain3+weekflow+dayflow+rflow+veg60m:dist:I(time==4),data=alldata)
tempo<-lm(alk~creek:time+time:I(dist^2):dayflow+buffalo:time:dist+rain2+rain3+time:I(dist^2):weekflow+dayflow+rflow+veg60m:dist:I(time==4),data=alldata)
tempo<-lm(alk~creek:time+time:I(dist^2):dayflow+buffalo:time:dist+rain2+rain3+time:I(dist^2):weekflow+dayflow+rflow+veg60m:dist:time,data=alldata)
tempo<-lm(alk~creek:time+time:I(dist^2):dayflow+buffalo:time:dist+rain2+rain3+time:I(dist^2):weekflow+dayflow+veg60m:dist:time,data=alldata)
tempo<-lm(alk~creek:time+buffalo:time:dist+rain2+rain3+time:I(dist^2):weekflow+dayflow+veg60m:I(dist^2):time,data=alldata)
tempo<-lm(alk~creek:time+buffalo:time+rain2+rain3+time:I(dist^2):weekflow+dayflow,data=alldata)


tempo<-lm(alk~creek:time,data=alldata)
tempo<-lm(alk~buffalo:time,data=alldata)
tempo<-lm(alk~rain3:time,data=alldata)
tempo<-lm(alk~rain2:time,data=alldata)
tempo<-lm(alk~dayflow,data=alldata)
tempo<-lm(alk~dayflow:I(dist^2):time,data=alldata)
tempo<-lm(alk~time:weekflow:I(dist^2),data=alldata)
tempo<-lm(alk~creek:time+buffalo:time+rain2+dayflow+weekflow,data=alldata) 
tempo<-lm(alk~creek:time+buffalo:time+rain2+dayflow,data=alldata) ### final model  

#anova(tempo,tempo1) 

############################v
library(pander)
panderOptions('digits', 2)
panderOptions('round', 2)
panderOptions('keep.trailing.zeros', TRUE)
#################

anova(tempo)   ##*€*€*€*€* Paper#4-appendix-Table 14 *€*€*€*€*##
summary(tempo) ##*€*€*€*€* Paper#4-appendix-Table 15 *€*€*€*€*##
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
#tempo1<-cbind(cbind(exp(fitted(tempo)),exp(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
#observed alkalinity plots with spatiotemporal model predictions (including other variables)
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"alk"],type="p",pch=19,col="black",xlab="Distance (km)",ylab="Alkalinity (mg/L)",main="Alkalinity prediction with environmental variable",ylim=c(10,21),cex=1,lwd=3,lty=1)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"alk"],col="blue",pch=15,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"alk"],col="green3",pch=8,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"alk"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)

legend("topleft",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

options( scipen = 1 )
anova(tempo)
anova(tempo,tempo1)
summary(tempo)
avPlots(tempo)

plot(tempo) ## checking the diagnostic plots to check the multiple regresstion assumptions
qqnorm(tempo$residuals)
qqline(tempo$residuals)
hist(tempo$residuals)
tempo.check<-lm(residuals(tempo)~time+buffalo:time+creek:time,data=alldata) # final model ##
summary(tempo.check)
anova(tempo.check) # No clear evidence of spatiotemporal patters in residuals 

##*€*€*€*€* alkalinity plot (Paper#4-Figure12) *€*€*€*€*#
tiff(file="Paper#4-Figure 12-alkalinity.tif",width=6,height=8,units="in",pointsize = 12,bg ="transparent",res=1000,compression="lzw")
par(mfrow=c(3,1), mar=c(3,3,2,1.5),mai = c(0.5, 0.9, 0.3, 0.3),cex=1,cex.axis=0.8,las=1)

#par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"alk"],type = "b",ylim=c(0,25),pch=19,cex=0.8,xlab="",ylab="Alkalinity (mg/L)",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"alk"],type="b",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"alk"],type="b",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"alk"],type="b",col="red",pch=17,cex=1.2,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

#Alkalinity with spatiotemporal model predictions
tempo<-lm(alk~time+buffalo:time+creek:time,data=alldata) # final model
anova(tempo)
summary(tempo)
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
#observed Alkalinity plots with spatiotemporal model predictions
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"alk"],type = "p",ylim=c(0,25),pch=19,cex=0.8,xlab="",ylab="Alkalinity (mg/L)",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"alk"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"alk"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"alk"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.2,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.2,lwd=3,lty=2)
legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

#observed Alkalinity plots with spatiotemporal model predictions (including other variables)
tempo<-lm(alk~creek:time+buffalo:time+rain2+dayflow,data=alldata) ### final model   
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]

plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"alk"],type = "p",ylim=c(0,25),pch=19,cex=0.8,xlab="",ylab="Alkalinity (mg/L)",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"alk"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"alk"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"alk"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.2,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.2,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)
dev.off()

#”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€
#”€”€”€”€”€ pH 

#pH plotted against spatial position
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"ph2"],type = "b",ylim=c(6.5,9),pch=19,cex=1.5,xlab="",ylab="pH",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"ph2"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"ph2"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"ph2"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

# Multiple regression analysis for pH vs time, distance and eff to check spatiotemporal variation of pH
# observed pH plots with spatiotemporal model prediction
tempo<-lm(ph2~time,data=alldata) ## Adjusted R-squared:  0.6409
tempo<-lm(ph2~eff,data=alldata)  ## no
tempo<-lm(ph2~eff:I(dist^2),data=alldata)  ## no
tempo<-lm(ph2~eff:time,data=alldata) ## Adjusted R-squared:  0.289
tempo<-lm(ph2~eff+eff:time,data=alldata) ## Adjusted R-squared:  0.289 
tempo<-lm(ph2~eff:dist,data=alldata) ## no
tempo<-lm(ph2~buffalo,data=alldata)  ## no
tempo<-lm(ph2~buffalo:time,data=alldata) ##Adjusted R-squared:  0.4953
tempo<-lm(ph2~time+buffalo:time+time:eff,data=alldata)  ## no

anova(tempo)
summary(tempo)
plot(tempo)
avPlots(tempo)

tempo<-lm(ph2~eff:dist:time,data=alldata)  ## Adjusted R-squared:  0.3946
tempo<-lm(ph2~time+buffalo:time,data=alldata) ##  no
tempo<-lm(ph2~time+buffalo:time+eff:dist:time,data=alldata) ## 
tempo<-lm(ph2~time+buffalo:time+eff:dist:time+eff:I(dist^2):time,data=alldata)
tempo<-lm(ph2~time+buffalo:time+dist:buffalo:time,data=alldata) ##
tempo<-lm(ph2~time+buffalo:time:dist+creek:I(dist^2):time,data=alldata)

tempo<-lm(ph2~creek,data=alldata)
tempo<-lm(ph2~buffalo,data=alldata)
tempo<-lm(ph2~dist,data=alldata)
tempo<-lm(ph2~creek:time,data=alldata) ## Adjusted R-squared:  0.8338 
tempo<-lm(ph2~buffalo:time,data=alldata)
tempo<-lm(ph2~time+dist:time,data=alldata)
tempo<-lm(ph2~time+creek:time+time:dist:eff+buffalo:time+buffalo:dist,data=alldata)
tempo<-lm(ph2~time+creek:time+time:dist+buffalo:time:dist,data=alldata)
tempo<-lm(ph2~time+dist+time:dist+buffalo:time+creek:time,data=alldata)
tempo<-lm(ph2~time+eff+dist+dist:time+buffalo:time+creek:time+eff:dist:time+eff:time,data=alldata)
tempo<-lm(ph2~time+eff:time+buffalo:time+creek:time+eff:dist:time+eff:time,data=alldata)
tempo<-lm(ph2~time+dist+eff+creek+buffalo+time:I(dist^2):eff+creek:time+buffalo:time,data=alldata)
tempo<-lm(ph2~time+time:I(dist^2):eff+creek:time+buffalo:time,data=alldata)
tempo<-lm(ph2~time+dist+creek:time,data=alldata)
tempo<-lm(ph2~time+buffalo:time+creek:time+eff:time+eff:dist:time+time:I(dist^2):eff,data=alldata)
tempo<-lm(ph2~time+eff:I(time==3)+eff:dist:I(time==3)+I(time==3):I(dist^2):eff+creek:I(time==3),data=alldata)
tempo<-lm(ph2~time+eff:I(time==3)+eff:dist:I(time==3)+I(time==3):I(dist^2):eff+creek:I(time==2)+creek:I(time==3)+creek:I(time==4),data=alldata)
tempo<-lm(ph2~time+creek+buffalo+eff:I(dist==11.03):time,data=alldata)
tempo<-lm(ph2~time+I(dist==11.03):time:eff+I(dist==10.4):creek,data=alldata)
tempo<-lm(ph2~time+I(dist==11.03):time:eff+dist:I(time==2)+buffalo:time,data=alldata)
tempo<-lm(ph2~time+I(dist==11.03):time:eff+dist:I(time==2)+buffalo:time,data=alldata)
tempo<-lm(ph2~time+I(dist==11.03):time:eff+dist:I(time==3),data=alldata)
tempo<-lm(ph2~time+eff:I(dist==11.03):time,data=alldata)
tempo<-lm(ph2~time+eff:I(dist==11.03):time+eff:I(dist^2):time+creek:I(dist==8.7):time+buffalo:I(dist==10.4):time,data=alldata)
tempo<-lm(ph2~time+eff:I(dist==11.03):time+I(dist^2):time+creek:I(dist==8.7)+buffalo:I(dist==10.4):time,data=alldata)
tempo<-lm(ph2~time+eff:I(dist==11.03):time+I(dist^2):time+creek:I(dist==8.7)+buffalo:I(dist==10.4):time,data=alldata)
tempo<-lm(ph2~time+eff:I(dist==11.03):time+I(dist^2):time+creek:I(dist==8.7),data=alldata)
tempo<-lm(ph2~time+eff:I(dist==11.03):time+eff:I(dist^2):time,data=alldata)
tempo<-lm(ph2~time+eff:I(dist==11.03):time+eff:I(dist^2),data=alldata)
tempo<-lm(ph2~time+eff:I(dist==11.03):time,data=alldata)## final model

anova(tempo,tempo1)
anova(tempo)
summary(tempo)
plot(tempo)
avPlots(tempo)

##*€*€*€*€* Paper#4-appendix-Table 16 and 17*€*€*€*€*##
tempo<-lm(ph2~time+eff:I(dist==11.03):time,data=alldata)# final model ##*€*€*€*€* Paper#4-appendix-Table 16 *€*€*€*€*##
anova(tempo)   ##*€*€*€*€* Paper#4-appendix-Table 16 *€*€*€*€*##
summary(tempo) ##*€*€*€*€* Paper#4-appendix-Table 17 *€*€*€*€*##
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"ph2"],type="p",pch=19,col="black",xlab="Distance (km)",ylab="pH",main="pH with spatiotemporal prediction",ylim=c(6.5,9),cex=1,lwd=3,lty=1)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"ph2"],col="blue",pch=15,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"ph2"],col="green3",pch=8,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"ph2"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

###residual plot 
plot(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"2"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=1,ylim=c(0,9))
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"2"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"2"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"2"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

plot(tempo) ## checking the diagnostic plots to check the multiple regresstion assumptions
qqnorm(tempo$residuals)
qqline(tempo$residuals)
hist(tempo$residuals)

###*€*€*€*€*# check the scatterplot for pH and water quality variables
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~ph2+eff+dayflow+dist+veg30m+veg60m+veg90m,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

##*€*€*## check the scatterplot for pH and vegetation
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~ph2+alk+vel+depth+width+cfpom+sed+canop+buffalo,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

##*€*€*## check the scatterplot for pH and rainfall
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~ph2+rain1+rain2+rain3+dflowm+dflow3m+dayflow+weekflow+rflow+rflow3,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

#pH regression with other variables
tempo<-lm(ph2~temp,data=alldata)  ## Adjusted R-squared:  0.281
tempo<-lm(ph2~depth,data=alldata)  ## Adjusted R-squared:  0.158
tempo<-lm(ph2~rain2,data=alldata)  ## Adjusted R-squared:  0.455
tempo<-lm(ph2~rain3,data=alldata)  ## Adjusted R-squared:  0.261
tempo<-lm(ph2~dflowm,data=alldata) ## 
tempo<-lm(ph2~dflow3m,data=alldata) ## no
tempo<-lm(ph2~weekflow,data=alldata) ## Adjusted R-squared:  0.309 
tempo<-lm(ph2~dayflow,data=alldata)  ## no
tempo<-lm(ph2~rflow,data=alldata)    ## Adjusted R-squared:  0.322
tempo<-lm(ph2~rflow3,data=alldata)   ## Adjusted R-squared:  0.253  
tempo<-lm(ph2~dayflow,data=alldata)  ##  no

anova(tempo)
summary(tempo)

##########Checking correlation
plot(alldata$eff,alldata$ph2)
cor.test(alldata$eff,alldata$ph2)
plot(alldata$tn,alldata$ph2)
cor.test(alldata$tn,alldata$ph2)
plot(alldata$alk,alldata$ph2)
cor.test(alldata$alk,alldata$ph2)
plot(alldata$rain1,alldata$ph2)
cor.test(alldata$rain1,alldata$ph2)
plot(alldata$rain2,alldata$ph2)
cor.test(alldata$rain2,alldata$ph2)
plot(alldata$rain3,alldata$ph2)
cor.test(alldata$rain3,alldata$ph2)
plot(alldata$cfpom,alldata$ph2)
cor.test(alldata$cfpom,alldata$ph2)
plot(alldata$temp,alldata$ph2)
cor.test(alldata$temp,alldata$ph2)
plot(alldata$toc,alldata$ph2)
cor.test(alldata$toc,alldata$ph2)

# Multiple regression analysis for pH and variables affecting it to check which variables explain spatiotemporal variation in pH
#### Linear modelling for pH
tempo<-lm(ph2~time+eff:I(dist==11.03):time,data=alldata)# final spatiotemporal model

anova(tempo)   ##*€*€*€*€* Paper#4-appendix-Table 16  *€*€*€*€*##
options("scipen"=10, "digits"=2)
summary(tempo) ##*€*€*€*€* Paper#4-appendix-Table 17  *€*€*€*€*##

tempo<-lm(ph2~alk:time+rain3+eff:time,data=alldata)
tempo<-lm(ph2~rain1+rain2+rain3+temp+alk+vel+tp+tn+rflow+dflow3m+dayflow+weekflow+creek+buffalo,data=alldata) # 
tempo<-lm(ph2~rain2+rain3+temp+alk+vel+tp+tn+rflow+dflow3m+dayflow+weekflow+creek+buffalo,data=alldata) #
tempo<-lm(ph2~alk:eff:time+rain2+rain3+chla,data=alldata)
tempo<-lm(ph2~alk+rain3+rain2+chla:time,data=alldata)
tempo<-lm(ph2~time:I(dist==11.03):alk+temp:time,data=alldata)
tempo<-lm(ph2~time:I(dist==11.03):alk+temp:time+buffalo,data=alldata)
tempo<-lm(ph2~time:I(dist==11.03):alk+temp:time+creek,data=alldata)
tempo<-lm(ph2~cfpom+temp+chla+turb+vel+sed+canop+do+cfpom:time+temp:time+chla:time+turb:time+vel:time+sed:time+canop:time+do:time,data=alldata)
tempo<-lm(ph2~cfpom+toc+temp+chla+turb+vel+sed+canop+cfpom:time+toc:time,data=alldata)
tempo<-lm(ph2~cfpom+alk+rain2+eff:I(dist==11.03):time,data=alldata)
tempo<-lm(ph2~cfpom+alk+rain2+toc+eff:I(dist==11.03):time,data=alldata)
tempo<-lm(ph2~cfpom+toc+alk+rain2+rain3+rflow+rflow3+weekflow+eff:I(dist==11.03):time+time,data=alldata)
tempo<-lm(ph2~cfpom+toc+alk+rain2+rain3+rflow+rflow3+eff:I(dist==11.03):time+time,data=alldata)
tempo<-lm(ph2~cfpom+toc+alk+rain2+rflow+rflow3+eff:I(dist==11.03):time,data=alldata)
tempo<-lm(ph2~cfpom+toc+alk+rain2+rflow+rflow3+eff:I(dist==11.03):time,data=alldata)
tempo<-lm(ph2~cfpom+toc+alk+rain2+rflow+rflow3+eff:I(dist==11.03):time,data=alldata)
tempo<-lm(ph2~cfpom+alk+rain2+dflowm+dflow3m+rflow+rflow3+eff:I(dist==11.03):time,data=alldata)
tempo<-lm(ph2~cfpom:time+alk+rain2+eff:I(dist==11.03):time,data=alldata)
tempo<-lm(ph2~alk+rain2+rflow3+eff:I(dist==11.03):time,data=alldata)
tempo<-lm(ph2~alk+rain2+eff:I(dist==11.03):time,data=alldata)
tempo<-lm(ph2~cfpom:time+alk+rain2+rflow3+eff:I(dist==11.03):time,data=alldata)## 
tempo<-lm(ph2~cfpom+alk+rain2+rflow3+eff:I(dist==11.03):time,data=alldata)## 
tempo<-lm(ph2~cfpom+alk+rain2+rflow3,data=alldata)## 
tempo<-lm(ph2~alk+rain2+I(dist==11.03):time,data=alldata)## 
tempo<-lm(ph2~rain2+eff:I(dist==11.03):time,data=alldata)## 
tempo<-lm(ph2~rain2+cfpom+eff:I(dist==11.03):time,data=alldata)## 
tempo<-lm(ph2~rain1+rain2+eff:I(dist==11.03):time,data=alldata)## final model


anova(tempo) ##*€*€*€*€* Paper#4-appendix-Table 18  *€*€*€*€*##
options("scipen"=10, "digits"=2)
summary(tempo) ##*€*€*€*€* Paper#4-appendix-Table 19  *€*€*€*€*##
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
#tempo1<-cbind(cbind(exp(fitted(tempo)),exp(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
#observed pH plots with spatiotemporal model predictions (including other variables)
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"ph2"],type="p",pch=19,col="black",xlab="Distance (km)",ylab="PH",main="pH prediction with environmental variable",ylim=c(6,9),cex=1,lwd=3,lty=1)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"ph2"],col="blue",pch=15,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"ph2"],col="green3",pch=8,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"ph2"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)

legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

xyplot(ph2~time,groups=dist,data=alldata,type="b",pch=c(1:6),auto.key=list(columns=4))
xyplot(ph2~dist,groups=time,data=alldata,type="b",auto.key=list(columns=5))
xyplot(ph2~no,groups=time,data=alldata,type="b",auto.key=list(columns=5))

options( scipen = 1 )
anova(tempo)
anova(tempo,tempo1)
summary(tempo)
avPlots(tempo)
plot(tempo) ## checking the diagnostic plots to check the multiple regresstion assumptions
qqnorm(tempo$residuals)
qqline(tempo$residuals)
hist(tempo$residuals)
tempo.check<-lm(residuals(tempo)~time+eff:I(dist==11.03):time,data=alldata)
summary(tempo.check)
anova(tempo.check) # 

##*€*€*€*€* pH plot (Paper#4-Figure 13) *€*€*€*€*#

tiff(file="Paper#4-Figure13-pH.tif",width=6,height=8,units="in",pointsize = 12,bg ="transparent",res=1000,compression="lzw")
par(mfrow=c(3,1), mar=c(3,3,2,1.5),mai = c(0.5, 0.9, 0.3, 0.3),cex=1,cex.axis=0.8,las=1)

#par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"ph2"],type = "p",ylim=c(6,9),pch=19,cex=0.8,xlab="",ylab="pH",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"ph2"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"ph2"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"ph2"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"ph2"],type = "l",pch=19,cex=0.5,lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"ph2"],type="l",col="blue",pch=8,lwd=3,cex=0.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"ph2"],type="l",col="green3",pch=15,cex=0.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"ph2"],type="l",col="red",pch=17,cex=0.5,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)


tempo<-lm(ph2~time+eff:I(dist==11.03):time,data=alldata)# final spatiotemporal model
anova(tempo)
summary(tempo)
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]

plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"ph2"],type = "p",ylim=c(6,9),pch=19,cex=0.8,xlab="",ylab="pH",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"ph2"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"ph2"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"ph2"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

tempo<-lm(ph2~rain1+rain2+eff:I(dist==11.03):time,data=alldata)# final model with environmental variables
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]

plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"ph2"],type = "p",ylim=c(6,9),pch=19,cex=0.8,xlab="",ylab="pH",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"ph2"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"ph2"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"ph2"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)
dev.off()


#”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€
#”€”€”€”€”€ CPOM/FPOM

##*€*€*## check the scatterplot for cpom/fpom and rainfall
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~cfpom+veg30m+veg60m+veg90m+rain1+rain2+rain3+canop+creek+buffalo+eff,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

#CPOM/FPOM regression with other variables
tempo<-lm(cfpom~veg30m,data=alldata)  ## Adjusted R-squared:  0.257
tempo<-lm(cfpom~veg60m,data=alldata)  ## Adjusted R-squared:  0.1325 
tempo<-lm(cfpom~depth,data=alldata)  ## no
tempo<-lm(cfpom~rain1,data=alldata)  ## no
tempo<-lm(cfpom~rain2,data=alldata)  ## no
tempo<-lm(cfpom~rain3,data=alldata)  ## no
tempo<-lm(cfpom~dflowm,data=alldata) ## no
tempo<-lm(cfpom~dflow3m,data=alldata) ## no
tempo<-lm(cfpom~weekflow,data=alldata) ## no
tempo<-lm(cfpom~dayflow,data=alldata)  ## no
tempo<-lm(cfpom~rflow,data=alldata)    ## no
tempo<-lm(cfpom~rflow3,data=alldata)   ## no 
tempo<-lm(cfpom~dayflow,data=alldata)  ## no 
tempo<-lm(cfpom~creek,data=alldata)  ## Adjusted R-squared:  0.1788 
tempo<-lm(cfpom~buffalo,data=alldata)  ## Adjusted R-squared:  0.09958 

anova(tempo)
summary(tempo)

##########Checking correlation
plot(alldata$eff,alldata$cfpom)
cor.test(alldata$eff,alldata$cfpom)
plot(alldata$creek,alldata$cfpom)
cor.test(alldata$creek,alldata$cfpom)
plot(alldata$buffalo,alldata$cfpom)
cor.test(alldata$buffalo,alldata$cfpom)
plot(alldata$depth,alldata$cfpom)
cor.test(alldata$depth,alldata$cfpom)

# Multiple regression analysis for CPOM/FPOM spatiotemporal variation 
tempo<-lm(cfpom~time+dist+eff+eff:dist:time+eff:dist+eff:time,data=alldata)
tempo<-lm(cfpom~time+eff:time+creek:time,data=alldata)
tempo<-lm(cfpom~time+creek:time+buffalo:time+eff:time+eff:dist,data=alldata)
tempo<-lm(cfpom~time+creek:time+buffalo:time+eff:time+dist,data=alldata)
tempo<-lm(cfpom~time+creek:time+buffalo:time+eff+time:dist,data=alldata)
tempo<-lm(cfpom~time+creek:time+buffalo+eff+time:dist,data=alldata)
tempo<-lm(cfpom~time+creek:time+buffalo+eff:I(dist==11.03):time+time:dist,data=alldata)
tempo<-lm(cfpom~time+creek:time+buffalo:time+eff:I(dist==11.03):time+I(dist==11.53):time,data=alldata)# final spatiotemporal model


###########
lmfit<-train(cfpom~time+creek:time+buffalo:time+eff:I(dist==11.03):time+I(dist==11.53):time,data=alldata)
lmfit<-train(cfpom~time+creek:time+buffalo:time,data=alldata)
print(lmfit)
###########

anova(tempo)   
summary(tempo) 
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
#tempo1<-cbind(cbind(exp(fitted(tempo)),exp(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
#observed cfpom plots with spatiotemporal model predictions (including other variables)
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"cfpom"],type="p",pch=19,col="black",xlab="Distance (km)",ylab="CPOM/FPOM",main="CPOM/FPOM prediction with environmental variable",ylim=c(0,40),cex=1,lwd=3,lty=1)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"cfpom"],col="blue",pch=15,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"cfpom"],col="green3",pch=8,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"cfpom"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)
legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)


# Multiple regression analysis for Conductivity and variables affecting it to check which variables explain spatiotemporal variation in cfpom
#### Linear modelling for cfpom

tempo<-lm(cfpom~canop+veg30m+veg60m+veg90m+creek+buffalo+dayflow+dflowm+dflow3m+weekflow+rflow,data=alldata)#
tempo<-lm(cfpom~veg30m:time+rain2:time+dayflow,data=alldata)#
tempo<-lm(cfpom~veg30m:time+rain2:time+weekflow,data=alldata)#
tempo<-lm(cfpom~veg30m:time+rain2:time+I(dist==11.03):eff:time,data=alldata)#
tempo<-lm(cfpom~dayflow:time:creek+dayflow:time:buffalo+rain2:time+I(dist==11.03):eff:time,data=alldata)#
tempo<-lm(cfpom~dayflow:time:creek+time:buffalo+rain2:time+I(dist==11.03):eff:time,data=alldata)#
tempo<-lm(cfpom~dayflow:time:creek+time:buffalo+rain2:time+I(dist==11.03):time:eff+I(dist==11.53):time,data=alldata)#
tempo<-lm(cfpom~rain2:time:creek+rain2:time+depth:time+I(dist==11.03):eff:time,data=alldata)
tempo<-lm(cfpom~rain2:time:creek+rain2:time+depth:time+buffalo:time:dayflow+I(dist==11.03):eff:time,data=alldata)## 
tempo<-lm(cfpom~rain2:time:creek+rain2:time+buffalo:time:dayflow+I(dist==11.03):eff:time,data=alldata)## 
tempo<-lm(cfpom~rain2:time:creek+rain2:time+depth:time+buffalo:time:dayflow+I(dist==11.03):eff:time,data=alldata)## 

tempo<-lm(cfpom~time:creek+rain2:time+depth:time+buffalo:time+I(dist==11.03):eff:time,data=alldata)## final model



anova(tempo)   
summary(tempo) #
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
#tempo1<-cbind(cbind(exp(fitted(tempo)),exp(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
#observed cfpom plots with spatiotemporal model predictions (including other variables)
plot(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==1,"cfpom"],type="p",pch=19,col="black",xlab="Distance (km)",ylab="CPOM/FPOM",main="CPOM/FPOM prediction with environmental variable",ylim=c(0,40),cex=1,lwd=3,lty=1)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==2,"cfpom"],col="blue",pch=15,cex=1.2,lwd=3,lty=2)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==3,"cfpom"],col="green3",pch=8,cex=1.2,lwd=3,lty=3)
points(tempo1$dist[tempo1$time==1],tempo1[tempo1$time==4,"cfpom"],col="red",pch=17,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.5,lwd=3,lty=1)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=15,cex=1.5,lwd=3,lty=2)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=8,cex=1.5,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.5,lwd=3,lty=4)

legend("topright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17,18),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.6,title="months")
abline(v=10.9,lty=2)

xyplot(cfpom~time,groups=dist,data=alldata,type="b",pch=c(1:6),auto.key=list(columns=4))
xyplot(cfpom~dist,groups=time,data=alldata,type="b",auto.key=list(columns=5))
xyplot(cfpom~no,groups=time,data=alldata,type="b",auto.key=list(columns=5))

options( scipen = 1 )
anova(tempo)
anova(tempo,tempo1)
summary(tempo)
avPlots(tempo)
plot(tempo) ## checking the diagnostic plots to check the multiple regresstion assumptions
qqnorm(tempo$residuals)
qqline(tempo$residuals)
hist(tempo$residuals)
tempo.check<-lm(residuals(tempo)~time+creek:time+buffalo:time+eff:I(dist==11.03):time+I(dist==11.53):time,data=alldata)
summary(tempo.check)
anova(tempo.check) # No clear evidence of spatiotemporal patters in residuals 

##*€*€*€*€*€*€*€*€*#

tiff(file="Paper#4-Figure14-CPOMFPOM.tif",width=6,height=8,units="in",pointsize = 12,bg ="transparent",res=1000,compression="lzw")
par(mfrow=c(3,1), mar=c(3,3,2,1.5),mai = c(0.5, 0.9, 0.3, 0.3),cex=1,cex.axis=0.8,las=1)

#par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"cfpom"],type = "p",ylim=c(0,40),pch=19,cex=0.8,xlab="",ylab="CPOM/FPOM",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"cfpom"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"cfpom"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"cfpom"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"cfpom"],type = "l",pch=19,cex=0.5,lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"cfpom"],type="l",col="blue",pch=8,lwd=3,cex=0.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"cfpom"],type="l",col="green3",pch=15,cex=0.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"cfpom"],type="l",col="red",pch=17,cex=0.5,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)


tempo<-lm(cfpom~time+creek:time+buffalo:time+eff:I(dist==11.03):time+I(dist==11.53):time,data=alldata)# final spatiotemporal model
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]

plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"cfpom"],type = "p",ylim=c(0,40),pch=19,cex=0.8,xlab="",ylab="CPOM/FPOM",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"cfpom"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"cfpom"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"cfpom"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

tempo<-lm(cfpom~rain2:time:creek+rain2:time+depth:time+buffalo:time:dayflow+I(dist==11.03):eff:time,data=alldata)## final model# final model with environmental variables
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]

plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"cfpom"],type = "p",ylim=c(0,40),pch=19,cex=0.8,xlab="",ylab="CPOM/FPOM",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"cfpom"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"cfpom"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"cfpom"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)
dev.off()

#”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€
#”€”€”€”€”€ river flow rate

##*€*€*## check the scatterplot for flow rate
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~dayflow+weekflow+rflow+rflow3+rain1+rain2+rain3+dflowm+dflow3m+creek+buffalo+eff+time+dist,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

#dayflow regression with other variables
tempo<-lm(dayflow~rain1,data=alldata) ## Adjusted R-squared:  0.614 
tempo<-lm(dayflow~rain2,data=alldata) ## Adjusted R-squared:  0.522 
tempo<-lm(dayflow~rain3,data=alldata) ## Adjusted R-squared:  0.641
tempo<-lm(dayflow~dflow3m,data=alldata) ## Adjusted R-squared:  0.277 

anova(tempo)
summary(tempo)

##########Checking correlation
plot(alldata$eff,alldata$dayflow)
cor.test(alldata$eff,alldata$dayflow)
plot(alldata$rain1,alldata$dayflow)
cor.test(alldata$rain1,alldata$dayflow)
plot(alldata$rain2,alldata$dayflow)
cor.test(alldata$rain2,alldata$dayflow)
plot(alldata$rain3,alldata$dayflow)
cor.test(alldata$rain3,alldata$dayflow)
plot(alldata$dflow3m,alldata$dayflow)
cor.test(alldata$dflow3m,alldata$dayflow)
plot(alldata$creek,alldata$dayflow)
cor.test(alldata$creek,alldata$dayflow)
plot(alldata$buffalo,alldata$dayflow)
cor.test(alldata$buffalo,alldata$dayflow)

#regression analysis for dayflow spatiotemporal variation 
tempo<-lm(dayflow~rain2,data=alldata)
anova(tempo)
summary(tempo)

#spatiotemporal model
tempo<-lm(dayflow~time+buffalo:I(time==2),data=alldata)
tempo<-lm(dayflow~time+buffalo:time+creek:time,data=alldata)

## observed values
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"dayflow"],type = "b",ylim=c(0,6000),pch=19,cex=1.5,xlab="Distance(km)",ylab="Flow rate",main="a) Flow rate plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"dayflow"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"dayflow"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"dayflow"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topleft",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

# Multiple regression analysis for  creek flow rate
tempo<-lm(dayflow~time+rain1+rain2+rain3:time+buffalo,data=alldata)  
tempo<-lm(dayflow~time+rain1+rain2+buffalo,data=alldata) 
tempo<-lm(dayflow~rain1+rain3+buffalo:time,data=alldata)  
tempo<-lm(dayflow~rain2:time+time:creek,data=alldata)  
tempo<-lm(dayflow~time+buffalo,data=alldata) 
tempo<-lm(dayflow~rain1+rain3+I(time==2)+buffalo:I(time==2),data=alldata)  ## there were two stations measuring river flow rate; one upstream and one downstream, there is no accurate data for before and after creek to show their effect
tempo<-lm(dayflow~rain3+I(time==2)+buffalo:I(time==2),data=alldata)## final model 
tempo<-lm(dayflow~rain3+time+buffalo:I(time==2),data=alldata)

tempo<-lm(dayflow~time+dflowm+rain1+rain3+buffalo,data=alldata)
tempo<-lm(dayflow~time+buffalo:time,data=alldata)

anova(tempo)   ##*€*€*€*€*€*€*€*€*###
summary(tempo) ##*€*€*€*€*€*€*€*€*###

tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]

plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"dayflow"],type = "p",ylim=c(0,6000),pch=19,cex=1.2,xlab="Distance(km)",ylab="Flow rate",main="b) Flow rate prediction with environmental variable",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"dayflow"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"dayflow"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"dayflow"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.2,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.2,lwd=3,lty=2)
legend("topleft",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

plot(tempo) ## checking the diagnostic plots to check the multiple regresstion assumptions
qqnorm(tempo$residuals)
qqline(tempo$residuals)
hist(tempo$residuals)

#”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€
#”€”€”€”€”€ flow rate
tiff(file="aper#4-Figure15-flow rate.tif",width=6,height=6,units="in",pointsize = 12,bg ="transparent",res=1000,compression="lzw")
par(mfrow=c(2,1), mar=c(3,3,2,1.5),mai = c(0.5, 0.9, 0.3, 0.3),cex=1,cex.axis=0.8,las=1)

#par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"dayflow"],type = "p",ylim=c(0,6000),pch=19,cex=0.8,xlab="",ylab="Flow rate(ML/month)",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"dayflow"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"dayflow"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"dayflow"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"dayflow"],type = "l",pch=19,cex=0.5,lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"dayflow"],type="l",col="blue",pch=8,lwd=3,cex=0.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"dayflow"],type="l",col="green3",pch=15,cex=0.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"dayflow"],type="l",col="red",pch=17,cex=0.5,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

tempo<-lm(dayflow~rain3+I(time==2)+buffalo:I(time==2),data=alldata)## final model 
tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"dayflow"],type = "p",ylim=c(0,6000),pch=19,cex=0.8,xlab="",ylab="Flow rate(ML/month)",main="",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"dayflow"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"dayflow"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"dayflow"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1,lwd=3,lty=2)
#legend("bottomright",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

dev.off()


#”€”€”€”€”€”€”€”€”€”€”€”€”€”€”€
#”€”€”€”€”€depth

##*€*€*## check the scatterplot for flow rate
par(mar=c(5,5,4,2),cex=0.9)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  method=c("spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.8, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.3, txt2)
}
pairs(~depth+dayflow+weekflow+rflow+rflow3+rain1+rain2+rain3+dflowm+dflow3m+creek+buffalo+eff+time+dist,data=alldata,upper.panel=panel.cor,pch=20,na.action = na.omit)

#depth regression with other variables
tempo<-lm(depth~weekflow,data=alldata) ## Adjusted R-squared:  0.201 
tempo<-lm(depth~rflow,data=alldata) ## Adjusted R-squared:  0.183
tempo<-lm(depth~rflow3,data=alldata) ## Adjusted R-squared:  0.105
tempo<-lm(depth~rain1,data=alldata) ##no
tempo<-lm(depth~rain2,data=alldata) ## Adjusted R-squared:  0.277 
tempo<-lm(depth~rain3,data=alldata) ## Adjusted R-squared:  0.244
tempo<-lm(depth~dflow3m,data=alldata) ## Adjusted R-squared:  0.16 
tempo<-lm(depth~rain2+rain3,data=alldata) ## 

anova(tempo)
summary(tempo)

##########Checking correlation
plot(alldata$depth,alldata$dayflow)
cor.test(alldata$depth,alldata$dayflow)


#regression analysis for depth spatiotemporal variation 
tempo<-lm(depth~rain2:time+rain3:time+dflow3m:time+weekflow:time+rflow:time+rflow3:time+buffalo:time+creek,data=alldata)
anova(tempo)
summary(tempo)

## observed values
par(mar=c(5,5,4,2),cex=0.9)
plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"depth"],type = "b",ylim=c(0,100),pch=19,cex=1.5,xlab="Distance(km)",ylab="Flow rate",main="a) Flow rate plotted against spatial position",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"depth"],type="b",col="blue",pch=8,lwd=3,cex=1.5,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"depth"],type="b",col="green3",pch=15,cex=1.5,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"depth"],type="b",col="red",pch=17,cex=1.5,lwd=3,lty=2)
legend("topleft",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

# Multiple regression analysis for  creek flow rate
tempo<-lm(depth~time+rain1+rain2+rain3:time+buffalo,data=alldata)  
tempo<-lm(depth~time+rain1+rain2+buffalo,data=alldata) 
tempo<-lm(depth~rain1+rain3+buffalo:time,data=alldata)  
tempo<-lm(depth~rain2:time+time:creek,data=alldata)  
tempo<-lm(depth~time+buffalo,data=alldata) 
tempo<-lm(depth~rain1+rain3+buffalo:time+creek:time,data=alldata)  ## final model ## there were two stations measuring river flow rate; one upstream and one downstream, there is no accurate data for before and after creek to show their effect
tempo<-lm(depth~rain3,data=alldata) 
tempo<-lm(depth~time+creek:time+buffalo:time+dflow3m:rain3,data=alldata) 
tempo<-lm(depth~rain2+dist,data=alldata) 

anova(tempo)   ##*€*€*€*€*  *€*€*€*€*###
summary(tempo) ##*€*€*€*€*  *€*€*€*€*###

tempo1<-cbind(cbind((fitted(tempo)),(residuals(tempo))),alldata)
attributes(tempo1)
names(tempo1[,1:2])<-c("fitted","residual")
tempo1[,"2"]

plot(bugenv[bugenv$day==1,"dist"],bugenv[bugenv$day==1,"depth"],type = "p",ylim=c(0,100),pch=19,cex=1.2,xlab="Distance(km)",ylab="Flow rate",main="b) Flow rate prediction with environmental variable",lwd=3,lty=5)
points(bugenv[bugenv$day==92,"dist"],bugenv[bugenv$day==92,"depth"],type="p",col="blue",pch=8,lwd=3,cex=1.2,lty=4)
points(bugenv[bugenv$day==210,"dist"],bugenv[bugenv$day==210,"depth"],type="p",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(bugenv[bugenv$day==283,"dist"],bugenv[bugenv$day==283,"depth"],type="p",col="red",pch=17,cex=1.2,lwd=3,lty=2)
points(tempo1[tempo1$day==1,"dist"],tempo1[tempo1$day==1,"1"],type="l",col="black",pch=19,cex=1.2,lwd=3,lty=5)
points(tempo1[tempo1$day==92,"dist"],tempo1[tempo1$day==92,"1"],type="l",col="blue",pch=8,cex=1.2,lwd=3,lty=4)
points(tempo1[tempo1$day==210,"dist"],tempo1[tempo1$day==210,"1"],type="l",col="green3",pch=15,cex=1.2,lwd=3,lty=3)
points(tempo1[tempo1$day==283,"dist"],tempo1[tempo1$day==283,"1"],type="l",col="red",pch=17,cex=1.2,lwd=3,lty=2)
legend("topleft",inset=c(0,0),legend=c("Feb 14", "May 14", "Sep 2014", "Dec 2014"),lty=c(1,5),pch=c(19,15,8,17),lwd=2,col=c("black","blue","green3","red"),ncol=2,horiz=FALSE,cex=0.9,title="months")
abline(v=10.9,lty=2)

plot(tempo) ## checking the diagnostic plots to check the multiple regresstion assumptions
qqnorm(tempo$residuals)
qqline(tempo$residuals)
hist(tempo$residuals)

####”€”€”€”€”€#”€”€”€”€”€###d-separation statements
####”€”€”€”€”€

library(dagR)
library(ggm)
library(pcalg)
library(gRbase)
library(gRain)
library(igraph)
library(piecewiseSEM)
install.packages("ggm")
library(ggm)

###adjacency matrix
sink('dsep.txt', append=TRUE)
dag.draw(mymyrdag)

##”€”€”€”€”€##basis set for the conditional independencies 
## series1
mymyrdag<-DAG(bugs~nh3+temp+ph2+alk+cond,ph2~rain+time+dist+eff,temp~buffalo+creek+rain+dflow+time+airtemp,nh3~buffalo+creek+time,alk~creek+buffalo+rain+time+dayflow,cond~temp+creek+time+dist+rain+buffalo,dayflow~rain+buffalo+time, creek~dist,eff~dist,dflow~time,buffalo~dist,rain~climate,airtemp~climate,climate~time, order=TRUE) 
sink('DAG1.txt', append=TRUE)
print(mymyrdag)
basis.set<-basiSet(mymyrdag)
print(basis.set)
sink(NULL)

mymyrdag<- as.matrix(mymyrdag)
adjmydag<-graph.adjacency(mymyrdag,(mode="directed"),weighted=TRUE,diag=FALSE)
str(adjmydag)
plot(adjmydag)
plot.igraph(adjmydag,vertex.label=V(adjmydag)$name,vertex.label.color="black",edge.color="blue",edge.width=E(adjmydag)$weight, edge.arrow.size=0.5,vertex.size=20, vertex.label.dist=1, vertex.color="pink",layout=layout_in_circle)
plot.igraph(adjmydag,vertex.size=15,vertex.label=V(adjmydag)$name,edge.curved=0.5)


################################# ## testing the conditional independencies
#”€”€”€”€”€”

#5# d-separation (dayflow des dist| rain, buffalo, time)
tempo<-lm(dayflow~rain3+time+buffalo,data=alldata)
independence.test1<-lm(dayflow~rain3+time+buffalo+dist,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)### no enough data

#6# d-separation (alk des dist| creek, buffalo, time, rain, dayflow)
tempo<-lm(alk~creek:time+buffalo:time+rain2+dayflow,data=alldata)
independence.test1<-lm(alk~creek:time+buffalo:time+rain2+dayflow+dist,data=alldata) 
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.7384

#7# d-separation (dflow des dist| time)
tempo<-lm(dflowm~time,data=alldata)
independence.test1<-lm(dflowm~dist+time,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)### no enough data

#8# d-separation (temp des dist|creek, buffalo, time, airtemp, rain, dflow) 
tempo<-lm(temp~rain2+airtemp+dflowm+buffalo:time+creek:time,data=alldata)
independence.test1<-lm(temp~rain2+airtemp+dflowm+buffalo:time+creek:time+dist,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)### p=0.6868

#9# d-separation (nh3 des dist|creek, buffalo, time)   
tempo<-lm(nh3~buffalo:(time==3)+creek:time,data=alldata)
independence.test1<-lm(nh3~buffalo:(time==3)+creek:time+dist,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)### p=0.8941

#10# d-separation (bugs des dist| alk, ph2, nh3, temp, cond)
myr.cap3<-capscale(formula=myrbug.BC~dist+Condition(temp+cond+temp:cond+nh3+temp+ph2+ph2:temp),data=alldata,comm=myrbug,add=TRUE,na.action=na.omit)#
edit(alldata)
design<-scale(model.matrix(~temp+cond+temp:cond+nh3+temp+ph2+ph2:temp,data=alldata),center=FALSE,scale=FALSE)  
edit(design)
myrbug1.BC<-vegdist(sqrt(myrbug))
n<-dim(myrbug)[1]
p<-n-1
myrbug1.mds<-cmdscale(myrbug1.BC, k = p, eig = TRUE, add = TRUE, x.ret = FALSE)
edit(myrbug)
pco.predict<-qr.fitted(qr(design),myrbug1.mds$points)
pco.resid<-myrbug1.mds$points-pco.predict##compute pco residuals
plot(alldata$dist, pco.resid[,1],type="b", xlab="distance", ylab="pco1 residuals")
xyplot(pco.resid[,1]~alldata$dist, xlab="distance", ylab="pco1 residuals")
independence.check<-lm(pco.resid[,1]~alldata$dist, data=alldata)
summary(independence.check)
anova(independence.check)## p=0.6747

#11# d-separation (buffalo des creek|dist)
tempo<-lm(buffalo~dist,data=alldata)
independence.test1<-lm(buffalo~dist+creek,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.9994

#12# d-separation (eff des creek| dist)
tempo<-lm(eff~dist,data=alldata)
independence.test1<-lm(eff~dist+creek,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.2246

#13# d-separation (creek des time| dist)
tempo<-lm(creek~dist,data=alldata)
independence.test1<-lm(creek~dist+time,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## 

#17# d-separation (dayflow des creek| rain, dist, buffalo, time)
tempo<-lm(dayflow~rain2+dist+buffalo+time,data=alldata)
independence.test1<-lm(dayflow~rain2+dist+buffalo+time+creek,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## no enough data

#18# d-separation (ph2 des creek | dist, eff, time, rain)
tempo<-lm(ph2~rain2+eff:I(dist==11.03):time,data=alldata)
independence.test1<-lm(ph2~rain2+eff:I(dist==11.03):time+creek,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.3116

#19# d-separation (dflow des creek| dist, time)
tempo<-lm(dflow~time+dist,data=alldata)
independence.test1<-lm(dflow~time+dist+creek,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.9475

#20# d-separation (bugs des creek| alk, ph2, nh3, temp, cond, dist)
myr.cap3<-capscale(formula=myrbug.BC~creek+Condition(temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+dist),data=alldata,comm=myrbug,add=TRUE,na.action=na.omit)#
edit(alldata)
design<-scale(model.matrix(~temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+dist,data=alldata),center=FALSE,scale=FALSE)  
edit(design)
myrbug1.BC<-vegdist(sqrt(myrbug))
n<-dim(myrbug)[1]
p<-n-1
myrbug1.mds<-cmdscale(myrbug1.BC, k = p, eig = TRUE, add = TRUE, x.ret = FALSE)
edit(myrbug)
pco.predict<-qr.fitted(qr(design),myrbug1.mds$points)
pco.resid<-myrbug1.mds$points-pco.predict##compute pco residuals
plot(alldata$creek, pco.resid[,1],type="b", xlab="creek", ylab="pco1 residuals")
xyplot(pco.resid[,1]~myrenv$creek, xlab="creek", ylab="pco1 residuals")
independence.check<-lm(pco.resid[,1]~myrenv$creek, data=alldata)
summary(independence.check)
anova(independence.check)## p=0.7081

#21# d-separation (eff des buffalo | dist)
tempo<-lm(eff~dist,data=alldata)
independence.test1<-lm(eff~buffalo+dist,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.2237

#22# d-separation (buffalo des time | dist)
tempo<-lm(buffalo~dist,data=alldata)
independence.test1<-lm(buffalo~dist+time,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## 

#26# d-separation (ph2 des buffalo | dist, eff, time, rain)
tempo<-lm(ph2~rain2+eff:I(dist==11.03):time,data=alldata)
independence.test1<-lm(ph2~rain2+eff:I(dist==11.03):time+buffalo ,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.8774

#27# d-separation(dflow des buffalo | dist, time)   
tempo<-lm(dflowm~time+dist,data=alldata)
independence.test1<-lm(dflowm~time+dist+buffalo,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)### no enough data

#28# d-separation (bugs des buffalo| alk, ph2, nh3, temp, cond, dist)
myr.cap3<-capscale(formula=myrbug.BC~buffalo+Condition(temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+dist),data=alldata,comm=myrbug,add=TRUE,na.action=na.omit)#
edit(alldata)
design<-scale(model.matrix(~temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+dist,data=alldata),center=FALSE,scale=FALSE)  
edit(design)
myrbug1.BC<-vegdist(sqrt(myrbug))
n<-dim(myrbug)[1]
p<-n-1
myrbug1.mds<-cmdscale(myrbug1.BC, k = p, eig = TRUE, add = TRUE, x.ret = FALSE)
edit(myrbug)
pco.predict<-qr.fitted(qr(design),myrbug1.mds$points)
pco.resid<-myrbug1.mds$points-pco.predict##compute pco residuals
plot(alldata$buffalo, pco.resid[,1],type="b", xlab="buffalo", ylab="pco1 residuals")
xyplot(pco.resid[,1]~alldata$buffalo, xlab="buffalo", ylab="pco1 residuals")
independence.check<-lm(pco.resid[,1]~alldata$buffalo, data=alldata)
summary(independence.check)
anova(independence.check)## p=0.6073

#29# d-separation (eff des time | dist)
tempo<-lm(eff~dist,data=alldata)
independence.test1<-lm(eff~dist+time,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## 

#33# d-separation (dayflow des eff | dist, buffalo, rain, time)
tempo<-lm(dayflow~rain3+buffalo+time+dist,data=alldata)
independence.test1<-lm(dayflow~rain3+buffalo+time+dist+eff,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## no enough data

#34# d-separation (alk des eff | dist, creek, buffalo, time, rain, dayflow)
tempo<-lm(alk~creek:time+buffalo:time+rain2+dayflow,data=alldata) 
independence.test1<-lm(alk~creek:time+buffalo:time+rain2+dayflow+eff,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.5526

#35# d-separation (dflow des eff | dist, time)    
tempo<-lm(dflowm~time+dist,data=alldata)
independence.test1<-lm(dflowm~time+dist+eff,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## no enough data

#36# d-separation (temp des eff | dist, creek, buffalo, time, airtemp, rain, dflow) 
tempo<-lm(temp~rain2+airtemp+dflowm+buffalo:time+creek:time,data=alldata)
independence.test1<-lm(temp~rain2+airtemp+dflowm+buffalo:time+creek:time+eff,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.1669

#37# d-separation (cond des eff | dist, creek, time, dayflow, alk, dflow, temp)  
tempo<-lm(cond~temp+rain1+creek:time+buffalo:time,data=alldata)
independence.test1<-lm(cond~temp+rain1+creek:time+buffalo:time+eff,data=alldata) 
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.9334

#38# d-separation (nh3 des eff | dist, buffalo, time, creek)    
tempo<-lm(nh3~buffalo:(time==3)+creek:time,data=alldata)
independence.test1<-lm(nh3~buffalo:(time==3)+creek:time+eff,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.6478

#39# d-separation (bugs des eff |dist, alk, ph2, nh3, temp, cond)
myr.cap3<-capscale(formula=myrbug.BC~eff+Condition(temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+dist),data=alldata,comm=myrbug,add=TRUE,na.action=na.omit)#
edit(alldata)
design<-scale(model.matrix(~temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+dist,data=alldata),center=FALSE,scale=FALSE)  
edit(design)
myrbug1.BC<-vegdist(sqrt(myrbug))
n<-dim(myrbug)[1]
p<-n-1
myrbug1.mds<-cmdscale(myrbug1.BC, k = p, eig = TRUE, add = TRUE, x.ret = FALSE)
edit(myrbug)
pco.predict<-qr.fitted(qr(design),myrbug1.mds$points)
pco.resid<-myrbug1.mds$points-pco.predict##compute pco residuals
plot(alldata$eff, pco.resid[,1],type="b", xlab="effluent", ylab="pco1 residuals")
xyplot(pco.resid[,1]~alldata$eff, xlab="effluent", ylab="pco1 residuals")
independence.check<-lm(pco.resid[,1]~alldata$eff, data=alldata)
summary(independence.check)
anova(independence.check)## p=0.2961

#42# d-separation (bugs des time |dist, alk, ph2, nh3, temp, cond)
myr.cap3<-capscale(formula=myrbug.BC~time+Condition(temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+dist),data=alldata,comm=myrbug,add=TRUE,na.action=na.omit)#
edit(alldata)
design<-scale(model.matrix(~temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+dist,data=alldata),center=FALSE,scale=FALSE)  
edit(design)
myrbug1.BC<-vegdist(sqrt(myrbug))
n<-dim(myrbug)[1]
p<-n-1
myrbug1.mds<-cmdscale(myrbug1.BC, k = p, eig = TRUE, add = TRUE, x.ret = FALSE)
edit(myrbug)
pco.predict<-qr.fitted(qr(design),myrbug1.mds$points)
pco.resid<-myrbug1.mds$points-pco.predict##compute pco residuals
plot(alldata$time, pco.resid[,1],type="b", xlab="time", ylab="pco1 residuals")
xyplot(pco.resid[,1]~alldata$time, xlab="time", ylab="pco1 residuals")
independence.check<-lm(pco.resid[,1]~alldata$time, data=alldata)
summary(independence.check)
anova(independence.check)## p=0.7797

#63# d-separation (dflow des dayflow | rain, buffalo, time)
tempo<-lm(dflowm~rain2+buffalo+time,data=alldata)
independence.test1<-lm(dflowm~rain2+buffalo+time+dayflow,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.2933

#93# d-separation (temp des dayflow | buffalo, rain, creek, time, airtemp, dflow)  
tempo<-lm(temp~rain2+airtemp+dflowm+buffalo:time+creek:time,data=alldata)
independence.test1<-lm(temp~rain2+airtemp+dflowm+buffalo:time+creek:time+dayflow,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## 

#65# d-separation (cond des dayflow | dist, time, rain, creek, buffalo, temp)   
tempo<-lm(cond~temp+rain1+creek:time+buffalo:time+dist,data=alldata)
independence.test1<-lm(cond~temp+rain1+creek:time+buffalo:time+dist+dayflow,data=alldata) 
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1) ## p=0.2904

#66# d-separation (nh3 des dayflow | buffalo, rain, time, creek)    
tempo<-lm(nh3~buffalo:(time==3)+creek:time+dist+eff+rain1+cfpom,data=alldata)
independence.test1<-lm(nh3~buffalo:(time==3)+creek:time+dist+eff+rain1+cfpom+dayflow,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.8175

#67# d-separation (bugs des dayflow | buffalo, rain, alk, ph2, nh3, temp, cond)
myr.cap3<-capscale(formula=myrbug.BC~dayflow+Condition(temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+dist),data=alldata,comm=myrbug,add=TRUE,na.action=na.omit)#
edit(alldata)
design<-scale(model.matrix(~temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+dist,data=alldata),center=FALSE,scale=FALSE)  
edit(design)
myrbug1.BC<-vegdist(sqrt(myrbug))
n<-dim(myrbug)[1]
p<-n-1
myrbug1.mds<-cmdscale(myrbug1.BC, k = p, eig = TRUE, add = TRUE, x.ret = FALSE)
edit(myrbug)
pco.predict<-qr.fitted(qr(design),myrbug1.mds$points)
pco.resid<-myrbug1.mds$points-pco.predict##compute pco residuals
plot(alldata$dayflow, pco.resid[,1],type="b", xlab="distance", ylab="pco1 residuals")
xyplot(pco.resid[,1]~alldata$dayflow, xlab="distance", ylab="pco1 residuals")
independence.check<-lm(pco.resid[,1]~alldata$dayflow, data=alldata)
summary(independence.check)
anova(independence.check)## p=0.8699

#68# d-separation (ph2 des alk | dist, eff, time, rain, creek, buffalo, dayflow)  
tempo<-lm(ph2~rain2+eff:I(dist==11.03):time+buffalo+creek+dayflow,data=alldata)
independence.test1<-lm(ph2~rain2+eff:I(dist==11.03):time+buffalo+creek+dayflow+alk,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1) ## 0.9452

#69# d-separation (alk des dflow | dist, creek, buffalo, time, rain, dayflow)
tempo<-lm(alk~creek:time+buffalo:time+rain2+dayflow+dist,data=alldata)  
independence.test1<-lm(alk~creek:time+buffalo:time+rain2+dayflow+dist+dflowm,data=alldata) 
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.058 

#70# d-separation (alk des temp | dist, creek, buffalo, time, rain, dayflow, airtemp, dflow)  
tempo<-lm(alk~creek:time+buffalo:time+rain2+dayflow+dist+airtemp+dflowm,data=alldata) 
independence.test1<-lm(alk~creek:time+buffalo:time+rain2+dayflow+dist+airtemp+dflowm+temp,data=alldata) 
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.8121

#71# d-separation (alk des cond | dist, creek, buffalo, time, rain, dayflow, temp) 
tempo<-lm(alk~creek:time+buffalo:time+rain2+dayflow+dist+airtemp+dflowm,data=alldata)
independence.test1<-lm(alk~creek:time+buffalo:time+rain2+dayflow+dist+airtemp+dflowm+cond,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.0082 **

#72# d-separation (alk des nh3 |creek, buffalo, time, rain, dayflow) 
tempo<-lm(alk~creek:time+buffalo:time+rain2+dayflow,data=alldata) 
independence.test1<-lm(alk~creek:time+buffalo:time+rain2+dayflow+nh3,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.8121

#73# d-separation (ph2 des dflow | rain, dist, eff, time, cfpom)    
tempo<-lm(ph2~alk+rain2+eff:I(dist==11.03):time,data=alldata)
independence.test1<-lm(ph2~alk+rain2+eff:I(dist==11.03):time+dflowm,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1) ## p=0.1381 

#74# d-separation (ph2 des temp | dist, eff, time, rain, dayflow, alk, creek, buffalo, airtemp, dflow)     
tempo<-lm(ph2~alk+rain2+eff:I(dist==11.03):time+dayflow+alk+creek+buffalo+airtemp+dflow,data=alldata)
independence.test1<-lm(ph2~alk+rain2+eff:I(dist==11.03):time+dayflow+alk+creek+buffalo+airtemp+dflow+temp,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1) ## p=0.1381

#75# d-separation (cond des ph2 | dist, eff, time, rain, creek, buffalo, temp)   
tempo<-lm(cond~temp+rain1+creek:time+buffalo:time,data=alldata)
independence.test1<-lm(cond~temp+rain1+creek:time+buffalo:time+ph2,data=alldata) 
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1) ## p=0.94

#76# d-separation (nh3 des ph| dist, eff, buffalo, time, rain, creek)  
tempo<-lm(nh3~buffalo:(time==3)+creek:time+dist+eff+rain1,data=alldata)
independence.test1<-lm(nh3~buffalo:(time==3)+creek:time+dist+eff+rain1+ph2,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)##p=0.06137

#77# d-separation (cond des dflow | dist, time, rain, creek, buffalo, temp)   
tempo<-lm(cond~temp+rain1+creek:time+buffalo:time+dist,data=alldata)
independence.test1<-lm(cond~temp+rain1+creek:time+buffalo:time+dist+dflowm,data=alldata) 
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1) ## p=0.75

#78# d-separation (nh3 des dflow | time, creek, buffalo)
tempo<-lm(nh3~buffalo:(time==3)+creek:time,data=alldata)
independence.test1<-lm(nh3~buffalo:(time==3)+creek:time+dflowm,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.9108

#79# d-separation (bugs des dflow | buffalo, rain, alk, ph2, nh3, temp, cond)
myr.cap3<-capscale(formula=myrbug.BC~dflowm+Condition(temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+dist),data=alldata,comm=myrbug,add=TRUE,na.action=na.omit)#
edit(alldata)
design<-scale(model.matrix(~temp+cond+temp:cond+nh3+temp+ph2+ph2:temp+dist,data=alldata),center=FALSE,scale=FALSE)  
edit(design)
myrbug1.BC<-vegdist(sqrt(myrbug))
n<-dim(myrbug)[1]
p<-n-1
myrbug1.mds<-cmdscale(myrbug1.BC, k = p, eig = TRUE, add = TRUE, x.ret = FALSE)
edit(myrbug)
pco.predict<-qr.fitted(qr(design),myrbug1.mds$points)
pco.resid<-myrbug1.mds$points-pco.predict##compute pco residuals
plot(alldata$dflowm, pco.resid[,1],type="b", xlab="distance", ylab="pco1 residuals")
xyplot(pco.resid[,1]~alldata$dflowm, xlab="distance", ylab="pco1 residuals")
independence.check<-lm(pco.resid[,1]~alldata$dflowm, data=alldata)
summary(independence.check)
anova(independence.check)## p=0.81

#80# d-separation (nh3 des temp | time, creek, buffalo, airtemp, rain, dflow)
tempo<-lm(nh3~buffalo:(time==3)+creek:time+airtemp+rain1+dflowm,data=alldata)
independence.test1<-lm(nh3~buffalo:(time==3)+creek:time+airtemp+rain1+dflowm+temp,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.97

#81# d-separation (nh3 des cond | time, creek, buffalo, rainfall, temperature)
tempo<-lm(nh3~buffalo:(time==3)+creek:time,data=alldata)
independence.test1<-lm(nh3~buffalo:(time==3)+creek:time+cond,data=alldata)
summary(independence.test1)
anova(independence.test1)
anova(tempo,independence.test1)## p=0.69


## Fisher's	C-test ######
## P-values from Iteration1
C<--2*(log(0.7893)+log(0.6747)+log(0.1369)+log(0.9994)+log(0.6088)+log(0.6088)+log(0.9475)+log(0.9475)+log(0.509)+log(0.2237)+log(0.9382)+log(0.54)+log(0.6073)+log(0.712)+log(0.5236)+log(0.2137)+log(0.1686)+log(0.3849)+log(0.2961)+log(0.2361)+log(0.7797)+log(0.1737)+log(0.5236)+log(0.6424)+log(0.3655)+log(0.1756)+log(0.5979)+log(0.6215)+log(0.739)+log(0.9182)+log(0.3471)+log(0.8699)+log(0.3304)+log(0.08125)+log(0.3326)+log(0.8078)+log(0.8128)+log(0.8345)+log(0.06288)+log(0.66)+log(0.8)+log(0.89)+log(0.85)+log(0.63)+log(0.81)) # uses p-values from partial tests
C<--2*(log(0.7384)+log(0.6868)+log(0.8941)+log(0.6747)+log(0.9994)+log(0.2246)+log(0.3116)+log(0.9475)+log(0.7081)+log(0.2237)+log(0.8774)+log(0.6073)+log(0.5526)+log(0.221)+log(0.1669)+log(0.9334)+log(0.6478)+log(0.2961)+log(0.7797)+log(0.002)+log(0.2933)+log(0.2904)+log(0.8175)+log(0.8699)+log(0.9452)+log(0.058)+log(0.8121)+log(0.008)+log(0.8121)+log(0.2515)+log(0.1381)+log(0.9353)+log(0.06137)+log(0.751)+log(0.9108)+log(0.8098)+log(0.9704)+log(0.6872)) # uses p-values from partial tests

# (or likelihood ratio test) of conditional independencies
# (or likelihood ratio test) of conditional independencies
C
pchisq(C,df=2*38,lower.tail=FALSE) # p-value for C-test
##
###
## some graphs
plot(pmyrbug[,"pco1"],myrbug[,"Oligochaeta"])  # Checks on aggregate process and heat maps
model1 <- lm(pmyrdiver$pco1 ~ myrbug$Oligochaeta)
summary(model1)
plot(pmyrdiver$pco1 ~ myrbug$Oligochaeta)
abline(model1)

plot(alldata$ph2,alldata$rain2)
cor.test(alldata$ph2,alldata$rain2)

plot(alldata$ph2,alldata$rain3)
cor.test(alldata$ph2,alldata$rain3)

plot(alldata$ph2,alldata$cfpom)
cor.test(alldata$ph2,alldata$cfpom)
cor.test(alldata$ph2[alldata$ph2<8],alldata$rain2[alldata$ph2<8])
plot(alldata$ph2[alldata$ph2<8],alldata$rain2[alldata$ph2<8])

############
discharge<- read.csv('discharge.csv',header=TRUE,nrows=23)
colnames(discharge) <- c("Month","Year","Discharge")
discharge$Year <- as.factor(discharge$Year)
discharge$Month <- as.character(discharge$Month)
discharge$Month <- as.Date(paste0("1-",discharge$Month),format="%d-%b-%y")
format(discharge$Month, format="%b-%y")

p <- ggplot(data =discharge,
            aes(x = Month, y = Discharge, group = Year, colour = Year)) + 
  geom_line() + theme_classic()

pp <- p+theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month")+
  xlab("Month") + ylab("Discharge (ML)")

ppp <- pp + theme(legend.position="top")

ggsave("discharge graph.jpeg",
       ppp, width=10, height=7, units="in", scale=1,limitsize = FALSE)



