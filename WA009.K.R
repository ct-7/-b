#WA009.1
dat <- read.csv("http://peach.l.chiba-u.ac.jp/course_folder/waa09_1.csv")
boxplot(dat$studyH~dat$result,xlab="hour studied",ylab="result",col=c("orange","gray"),horizontal = T)

dat.lr <- glm(result~studyH,family=binomial,data=dat)
summary(dat.lr)


#WA009.2
x=seq((min(dat$studyH)-2), (max(dat$studyH)+2), 0.1)
keisu  = coef(dat.lr)
y.prob=1/(1+exp(-1*(keisu[1]+keisu[2]*x)))
plot(dat$studyH, dat$result, pch=20, cex=3, xlab="hour studied",
     ylab="result", col='lightskyblue1', ylim = c(0.5, 2.5),yaxt="n")
axis(2, at = c(1,2),labels=c("not paseed","passed"))
lines(x,y.prob+1,lty=2,lwd=4,col='cornflowerblue')

単位を取得する確率/単位を落とす確率＝-17.2317+2.1487*studyH
log(p/1-p)=b0 + b1X1 + b2X2 + … + bnXn
p/(1-p)=e^(b0 + b1X1 + b2X2 + … + bnXn)

exp(dat.lr$coefficients) #オッズ比
coe <- su$coefficints

exp(β±1.96×標準誤差)


#WA009.3


#WA009.4
M=matrix(c(50,40,10,10),nrow=2,byrow=T)
chisq.test(M,correct=F)
chisq.test(M)


#WA009.5



#授業中の実習
colnames(M)<-c("cs","ps")
rownames(M)<-c("pass","np")
M.loglin<-loglin(M,list(1,2), fit=T,param=T)
M
1-pchisq(M$lrt,1)

