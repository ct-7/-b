dat<-read.csv("http://www.matsuka.info/data_folder/hwsk8-17-6.csv")
plot(ani~otouto,data=dat,xlab="Score of Younger Brother", 
     ylab = "Score of Elder Brother", pch=20,cex=2,
     xlim=c(5,27),ylim = c(5,27))
dat.lm <- lm(ani~otouto, data=dat)
summary(dat.lm)
abline(dat.lm, col = 'red',lwd = 2.5)

# two sample t-test
boxplot(dat,col=c('skyblue','coral'),ylab = "score")
t.test(dat$ani, dat$otouto, var.equal=T)
dat2<-data.frame(score = c(dat$ani, dat$otouto),order=c(rep("ani",10),rep("otouto",10)))
plot(dat2$score~as.numeric(dat2$order),pch=20,xlab="order",
     ylab="score",xlim=c(0.5,2.5),cex=2,xaxt="n")
axis(1,c(1,2),c("ani","otouto"))
dat2.lm<-lm(score~order,data=dat2)
abline(dat2.lm,col='red',lwd=3)

# one sample t-test
dat.D = dat$ani - dat$otouto
boxplot(dat.D,col="skyblue",ylab="Difference")
t.test(dat.D)
dat.D.lm<-lm(dat.D~1) #one sample T-test の時は1
plot(dat.D~rep(1,10),pch=20,xlab="",ylab="Difference",cex=3)
summary(dat.D.lm)

# plotting errors
plot(ani~otouto,data=dat,xlab="Score of Younger Brother", 
     ylab = "Score of Elder Brother", pch=20,cex=2,
     xlim=c(5,27),ylim = c(5,27))
dat.lm <- lm(ani~otouto, data=dat)
abline(h=mean(dat$ani),lty=2,col="green",lwd=3)
abline(dat.lm,col='red',lwd=3)
pred.lm<-predict(dat.lm)
for (i.ani in 1:10){
  lines(rep(dat$otouto[i.ani],2),c(dat$ani[i.ani],pred.lm[i.ani]),
        col='blue',lwd=3)
}

# multiple regression
dat<-read.csv("http://www.matsuka.info/data_folder/tdkReg01.csv")
plot(dat)
dat.regALL<-lm(sales~price+design+material,data=dat)
summary(dat.regALL)
dat.reg4<-lm(sales~price+design,data=dat)
summary(dat.reg4) 
#Adjusted R でのモデルの比較

# ANCOVA
dat<-read.csv("http://www.matsuka.info/data_folder/ancova01.csv")
dat$pretest=dat$pretest*0.1
