#後で確認
#WA006.1
dat<-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/waa05.csv") 
dat$condition = factor(dat$condition, levels(dat$condition)[2:1])
dat1 <- dat[dat$medicine == "Medicine A",]
dat1.pre <- dat1[dat1$condition == "pre",] 
dat1.post <- dat1[dat1$condition == "post",] 
dat.lm <- lm(dat1.pre$blood.pressure ~ dat1.post$blood.pressure,dat1)
summary(dat.lm)
#有意に差がある。よって、帰無仮説を棄却。効果が見られた。

#WA006.2
#切片は差になる　回帰分析では切片をbとして検定する　t検定ではさを検定している
dat.pre <- dat[dat$condition == "pre",]
dat.post <- dat[dat$condition == "post",]
dat.medA.post <- dat$blood.pressure[dat$medicine == "Medicine A"]
dat.medB.post <- dat$blood.pressure[dat$medicine == "Medicine B"]
plot(as.numeric(dat.post$medicine)-1,dat.post$blood.pressure, 　　#as.numericだとmedicineA,Bは1と2になるため-1をする
     xlab = "medicine type",ylab = "blood.pressure",pch = 16,xlim = c(-0.5,1.5),xaxt = "n") #x軸を消す
axis(1,at = c(0,1),c("Medicine A","Medicine B")) #軸の書き換え atで目盛の場所
datAB.lm <- lm(dat.post$blood.pressure ~ dat.post$medicine,dat.post)
abline(datAB.lm,col = "red",lwd = 2)

#WA006.3
dat<-read.table("http://www.matsuka.info/data_folder/tdkPATH01.txt",header=T)
plot(dat$grade ~ dat$interest + dat$knowledge + dat$absence + dat$study)
gi.lm <- lm(dat$grade ~ dat$interest)
summary(gi.lm) #0.59
ga.lm <- lm(dat$grade ~ dat$absence)
summary(ga.lm) #0.66
gk.lm <- lm(dat$grade ~ dat$knowledge)
summary(gk.lm) #0.25
gs.lm <- lm(dat$grade ~ dat$study)
summary(gs.lm) #0.41
g.All <- lm(dat$grade ~ .,data = dat)
summary(g.All) #0.74
