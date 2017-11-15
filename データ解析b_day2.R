# RB
dat<-read.csv("http://www.matsuka.info/data_folder/dktb316.txt")
colnames(dat)<-c("id",'method','subj','words')
dat.aov<-aov(words~method+subj+Error(subj:method),data=dat)
summary(dat.aov)
TukeyHSD(summary(dat.aov)) #これはエラーになる

# Tukey HSD
str(summary(dat.aov))  #構造を確認
mse <- dat.aov.sum[[1]][[1]][3,3]
q<-qtukey(0.95,4,df=21)
hsd<-q*sqrt(mse/8) 
dat.means=tapply(dat$words, dat$method, mean)
ck.hsd<-abs(outer(dat.means,dat.means,'-'))>hsd ##それぞれの差分を計算
ck.hsd 


# SPF
dat<-read.csv("http://www.matsuka.info/data_folder/dktb3211.txt")
interaction.plot(dat$duration,  # x軸
                 dat$method,    # まとめる変数　　　　
                 dat$result,    # y軸 
                 pch=c(20,20), 
                 col=c("skyblue","orange"),
                 ylab="score",
                 xlab="Duration",
                 lwd=3,　
                 type='b',
                 cex=2, 
                 trace.label="Method")


source("http://peach.l.chiba-u.ac.jp/course_folder/tsme.txt") 
dat<-read.csv("http://www.matsuka.info/data_folder/dktb3211.txt") 
dat.aov<-aov(result~method*duration+Error(s+s:duration),dat)
# 魔法の言葉
TSME<-SPF.tsme(dat.aov,dat,"result") 
