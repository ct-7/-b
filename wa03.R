#WAA03:1
#figure1
par(mfrow=c(1,1))
dat<-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/waa03.csv")
dat.cont<-dat[dat$condition=='control',]
plot(dat.cont$w,dat.cont$blood.pressure,col='cyan2',pch=16,main="relationship between blood pressure and weight (by medicine type)",xlab='weight',ylab='blood pressure',xlim=c(40,80),ylim=c(60,160))
abline(lm(dat.cont$blood.pressure~dat.cont$w),col='cyan3',lwd=2)
par(new=T)
dat.A=dat[dat$condition=='medicine A',]
plot(dat.A$w,dat.A$blood.pressure,col='darkgoldenrod1',pch=17,main="relationship between blood pressure and weight (by medicine type)",xlab='weight',ylab='blood pressure',ylim=c(60,160),xlim=c(40,80))
abline(lm(dat.A$blood.pressure~dat.A$w),col='darkgoldenrod2',lwd=2)
par(new=T)
dat.B=dat[dat$condition=='medicine B',]
plot(dat.B$w,dat.B$blood.pressure,col='coral2',pch=15,main="relationship between blood pressure and weight (by medicine type)",xlab='weight',ylab='blood pressure',xlim=c(40,80),ylim=c(60,160))
abline(lm(dat.B$blood.pressure~dat.B$w),col='coral1',lwd=2)
legend("topleft", c('control','med.A','med.B'),col=c('cyan2','darkgoldenrod1','coral2'),pch=c(16, 17, 15),cex=1,lwd=2)


#figure2
par(mfrow=c(3,1))
plot(dat.cont$w,dat.cont$blood.pressure,col='cyan2',pch=16,main="relationship between blood pressure and weight (by medicine type)",xlab='',ylab='blood pressure',xlim=c(40,80),ylim=c(60,160))
abline(lm(dat.cont$blood.pressure~dat.cont$w),col='cyan3',lwd=2)
text(45,150,'Control',cex=2)
plot(dat.A$w,dat.A$blood.pressure,col='darkgoldenrod1',pch=17,main="",xlab='',ylab='blood pressure',xlim=c(40,80),ylim=c(60,160))
abline(lm(dat.A$blood.pressure~dat.A$w),col='darkgoldenrod2',lwd=2)
text(45,150,'Medecine A',cex=2)
plot(dat.B$w,dat.B$blood.pressure,col='coral2',pch=15,main="",xlab='weight',ylab='blood pressure',xlim=c(40,80),ylim=c(60,160))
abline(lm(dat.B$blood.pressure~dat.B$w),col='coral1',lwd=2)
text(45,150,'Medecine B',cex=2)




#WAA03:2
#帰無仮説Ho:Ａ先生とＤ先生は同等ではない 対立仮設H1:Ａ先生とＤ先生は同等である　とおく。
#A先生が１０回中三回勝つ確率は
dbinom(3,10,0.5)
#[1] 0.1171875
sum(dbinom(0:3,10,0.5))
#[1] 0.171875
#となり、有意水準を５％にしても帰無仮説は有意とはならない。
#そのため対立仮設は棄却され、帰無仮説が採択される。
#よって、Ａ先生とＤ先生は同等ではないということが言える。


#WAA03:3
#帰無仮説Ho:Ａ先生に予知能力はない 対立仮設H1:Ａ先生に予知能力はある　とおく。
#A先生がコインを２０回投げて２回正解する確率は
sum(dbinom(0:2,20,0.5))
#[1]	0.0002012253
	sum(dbinom(0:2,20,0.5))+sum(dbinom(18:20,20,0.5))
#[1]	0.0004024506
#となり、有意水準を１％にしても帰無仮説は有意となる。
#そのた帰無仮説は棄却され、対立仮設が採択される。
#よって、Ａ先生には予知能力があるということが言える。

