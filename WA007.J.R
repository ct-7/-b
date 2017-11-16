dat<-read.csv("http://www.matsuka.info/data_folder/dktb312.txt") #a1~4で変数の違いを入れている
dat2<-data.frame(result=dat$result,
                 c1=c(rep(0,8),rep(1,8),rep(0,16)), #反復の人に1を入れる
                 c2=c(rep(0,16),rep(1,8),rep(0,8)),
                 c3=c(rep(0,24),rep(1,8)))
dat2.lm<-lm(result~.,data=dat2)　
dat.lm <- lm(result~a,data=dat) #contrastを書かなくても自動でやってくれる

# or
#回帰モデルでトレンド解析
dat2<-dat[dat$method=="method.X",]
dat2.lm<-lm(result~duration, data=dat2,
            contrasts=list(duration="contr.poly"))
#コントラスト書き出し
cs = contr.poly(4)
#内積は0
t(cs[,1]%*%cs[,2])


dat3<-data.frame(result=dat$result, 
                 c1=c(rep(-3,8), rep(1,24)), 
                 c2=c(rep(0,8),rep(-2,8),rep(1,16)),
                 c3=c(rep(0,16),rep(-1,8),rep(1,8)))
dat3.lm<-lm(result~c1+c2+c3,data=dat3)


dat<-read.csv("http://www.matsuka.info/data_folder/dktb321.txt")
plot(dat$result~dat$duration,data=dat[dat$method=="method.x",])
result<-dat$result[dat$method=="method.X"]
CL=c(rep(-3,5),rep(-1,5),rep(1,5),rep(3,5))
CQ=c(rep(-1,5),rep(1,5),rep(1,5),rep(-1,5))
CC=c(rep(-3,5),rep(1,5),rep(-1,5),rep(3,5))

result2=result;
result2[16:20]=result2[16:20]-3
plot(tapply(result2,dat$duration[dat$method=="method.X"],mean),
     pch=20,col="red",lwd=3, type="o",cex=3,ylab="mean",xlab="time")
trend2.lm<-lm(result2~CL+CQ+CC)

Cont=contr.poly(4) 

##交互作用
subj.gender = c(rep("male",30),rep("female",30))
pic.gender = rep(c(rep("male",15),rep("female",15)),2)
# 15m-m, 15m-f, 15f-m, 15f-f 
eye.fix = c(round(rnorm(15,50,5)),
            round(rnorm(15,70,5)),
            round(rnorm(15,65,5)),
            round(rnorm(15,50,5)))
datE = data.frame(eye.fix = eye.fix, subj.gender= subj.gender,pic.gender=pic.gender)
interaction.plot(datE$subj.gender, datE$pic.gender, datE$eye.fix, 
                 trace.label = "Gender of Stimuli", xlab = "Gender of Participants",
                 ylab = "Number of fixation",lwd = 3,type = "b",pch = c(17,20),cex =3,
                 col = c('skyblue','coral'),legend =T)
datE.lm <- lm(eye.fix~subj.gender*pic.gender,data=datE)
dat.m1 <- lm(eye.fix~subj.gender,data=datE)
summary(dat.m1)
dat.m2 <- lm(eye.fix~pic.gender,data=datE)
summary(dat.m2)
dat.m3 <- lm(eye.fix~subj.gender+pic.gender,data=datE)
summary(dat.m3)
dat.m4 <- lm(eye.fix~subj.gender+pic.gender+subj.gender*pic.gender,data=datE)
summary(dat.m4)
dat.m5 <- lm(eye.fix~subj.gender*pic.gender,data=datE)
summary(dat.m5)



dat<-read.csv("http://www.matsuka.info/data_folder/ancova01.csv")
dat$pretest=dat$pretest*0.1

dat<-read.csv("http://www.matsuka.info/data_folder/tdkReg01.csv") 
dat.lm01<-lm(sales~price, data=as.data.frame(scale(dat)))
plot(dat.lm01,which=1)
plot(dat.lm01,which=2)


par(mfrow=c(2,2))
plot(dat.lm01)

dat<-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/dktb312.csv")
dat$method=factor(dat$method, levels(dat$method)[c(1,3,4,2)])

dat<-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/forbesdata.txt") 
boil.lm<-lm(log(pressure)~temp, data=boil)
par(mfrow=c(2,2))
plot(boil.lm)

boil.lm<-lm(log(pressure)~temp, data=boil[-12,])
plot(boil.lm)

dat<-read.csv("http://www.matsuka.info/data_folder/tdkReg02.csv")
plot(dat)
dat.lm<-lm(sales~., data=dat) 
install.packages("DAAG")
library(DAAG)
vif(dat.lm)