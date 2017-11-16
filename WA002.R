#WAA02
#figure1
dat<-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/waa02.csv")
boxplot(dat$blood.pressure~dat$condition,main="Effect of medicine on Blood pressure",ylab="blood pressure",col=c('cyan1','darkgoldenrod1','coral1'),ylim=c(60,170))


#figure2
install.packages("gplots")
library(gplots)
means<-tapply(dat$blood.pressure, dat$condition, mean)
sds<-tapply(dat$blood.pressure, dat$condition,sd)     
ns<-tapply(dat$blood.pressure, dat$condition,length) 
sems = sds/sqrt(ns) 
barplot2(means, plot.ci=T,ci.l= means-sems,ci.u=means+sems,ylim = c(80,140),xpd=F,ylab="blood pressure",xlab="condition",col=c('cyan1','darkgoldenrod1','coral1'),main="Effect of medicine on Blood pressure")


#figure3
dens.cont=density(dat[dat$condition=='control',]$blood.pressure)
dens.A=density(dat[dat$condition=='medicine A',]$blood.pressure)
dens.B=density(dat[dat$condition=='medicine B',]$blood.pressure)
plot(dens.cont,col='blue',lwd=2,main="Effect of medicine on Blood pressure",xlab='blood pressure',ylab='density',xlim=c(60,160),ylim=c(0,0.030))
lines(dens.A,col='darkgoldenrod3',lwd=2)
lines(dens.B,col='coral2',lwd=2)
legend("topleft", c('control','medicineA','medicineB'),col=c('blue','darkgoldenrod3','coral2'),cex=1,lwd=2)


#figure4
par(mfrow=c(3,1))
hist(dat[dat$condition=='control',]$blood.pressure,main="Effect of medicine on Blood pressure",xlab="",ylab='density',xlim=c(60,160),ylim=c(0,0.030),probability=T,col='cyan1')
lines(dens.cont,col='blue',lwd=2)
text(70,0.025,'Control',cex=2)
hist(dat[dat$condition=='medicine A',]$blood.pressure,col='darkgoldenrod1',main="",xlab='',ylab='density',xlim=c(60,160),ylim=c(0,0.030),probability=T)
lines(dens.A,col='darkgoldenrod3',lwd=2)
text(70,0.025,'Medicine A',cex=2)
hist(dat[dat$condition=='medicine B',]$blood.pressure,col='coral1',main="",xlab='blood pressure',ylab='density',xlim=c(60,160),ylim=c(0,0.030),probability=T)
lines(dens.B,col='coral2',lwd=2)
text(70,0.025,'Medecine B',cex=2)
