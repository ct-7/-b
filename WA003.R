#WAA03:1

#figure1
par(mfrow=c(1,1))
dat<-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/waa03.csv")
dat.cont<-dat[dat$condition=='control',]
plot(dat.cont$w,dat.cont$blood.pressure,col='cyan2',pch=16,main="relationship between blood pressure and weight (by medicine type)",xlab='weight',ylab='blood pressure',xlim=c(40,80),ylim=c(60,160))
abline(lm(dat.cont$blood.pressure~dat.cont$w),col='cyan3',lwd=2)
par(new=T)
dat.A=dat[dat$condition=='medicine A',]
plot(dat.A$w,dat.A$blood.pressure,col='darkgoldenrod1',pch=16,main="relationship between blood pressure and weight (by medicine type)",xlab='weight',ylab='blood pressure',ylim=c(60,160),xlim=c(40,80))
abline(lm(dat.A$blood.pressure~dat.A$w),col='darkgoldenrod2',lwd=2)
par(new=T)
dat.B=dat[dat$condition=='medicine B',]
plot(dat.B$w,dat.B$blood.pressure,col='coral2',pch=16,main="relationship between blood pressure and weight (by medicine type)",xlab='weight',ylab='blood pressure',xlim=c(40,80),ylim=c(60,160))
abline(lm(dat.B$blood.pressure~dat.B$w),col='coral1',lwd=2)
legend("topleft", c('control','med.A','med.B'),col=c('cyan2','darkgoldenrod1','coral2'),cex=1,lwd=2)

#figure2
par(mfrow=c(3,1))
plot(dat.cont$w,dat.cont$blood.pressure,col='cyan2',pch=16,main="relationship between blood pressure and weight (by medicine type)",xlab='',ylab='blood pressure',xlim=c(40,80),ylim=c(60,160))
abline(lm(dat.cont$blood.pressure~dat.cont$w),col='cyan3',lwd=2)
text(45,150,'Control',cex=2)
plot(dat.A$w,dat.A$blood.pressure,col='darkgoldenrod1',pch=16,main="",xlab='',ylab='blood pressure',xlim=c(40,80),ylim=c(60,160))
abline(lm(dat.A$blood.pressure~dat.A$w),col='darkgoldenrod2',lwd=2)
text(45,150,'Medecine A',cex=2)
plot(dat.B$w,dat.B$blood.pressure,col='coral2',pch=16,main="",xlab='weight',ylab='blood pressure',xlim=c(40,80),ylim=c(60,160))
abline(lm(dat.B$blood.pressure~dat.B$w),col='coral1',lwd=2)
text(45,150,'Medecine B',cex=2)

#WAA03:2
dbinom(3,10,0.5)
x<-1:10
plot(x,dbinom(x,10,0.5),type="h",xlab="事象の生起回数",ylab="確率",lwd=20,col="red",xlim=c(0,10))

#WAA03:3
x<-1:20
dbinom(2,20,0.5)
plot(x,dbinom(x,20,0.5),type="h",xlab="事象の生起回数",ylab="確率",lwd=10,col="red",xlim=c(0,20))

