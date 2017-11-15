#WAB05.1
dat <- read.csv("http://peach.l.chiba-u.ac.jp/course_folder/waa05.csv")
dat$condition = factor(dat$condition, levels(dat$condition)[2:1])
dat2<-dat[,1:3]
dat2$subj <- rep(paste("subj",1:50,sep =""),2)
interaction.plot(dat2$condition,  # x軸
                 dat2$medicine,    # まとめる変数　　　　
                 dat2$blood.pressure,    # y軸 
                 pch=c(20,20), 
                 col=c("skyblue","orange"),
                 ylab="Blood Pressure",
                 xlab="condition",
                 lwd=3,　
                 type='b',
                 cex=1.5,
                 trace.label="Medicine")
dat2.aov <- aov(blood.pressure ~ medicine + condition + medicine:condition + 
                Error(subj + subj:medicine + subj:condition + subj:medicine:condition),
                data = dat2)
dat2.aov.sum <- summary(dat2.aov)

# 単純主効果の検定
dat.aov.sEffect <- aov(blood.pressure ~ condition*medicine +
                       Error(subj + subj:medicine + subj:condition),dat2)
summary(dat.aov.sEffect)
source("http://peach.l.chiba-u.ac.jp/course_folder/tsme.txt") 
TSME<-RBF.tsme(dat.aov.sEffect, dat2,"blood.pressure")


#WAB05.2
dat <- read.csv("http://peach.l.chiba-u.ac.jp/course_folder/w04.csv")
head(dat)
interaction.plot(dat$stimulus,  # x軸
                 dat$method,    # まとめる変数　　　　
                 dat$score,    # y軸 
                 pch=c(20,20,20), 
                 col=c("skyblue","orange","palegreen2"),
                 ylab="Score",
                 xlab="Stimulus",
                 lwd=3,　
                 type='b',
                 cex=1.5,
                 trace.label="Method")
dat.aov <- aov(score ~ stimulus + method + stimulus:method + 
               Error(subj + subj:method + subj:stimulus + subj:method:stimulus),
               data = dat)
dat.aov.sum <- summary(dat.aov)
dat.aov.sum

str(dat.aov.sum)　#中身確認
#方法の多重比較
mse <- dat.aov.sum[[2]][[1]][[3]][2]
q <- qtukey(0.95,3,18)      #18はsubj:methodのDf
hsd <- q*sqrt(mse/15*3)       #*3はもう一つの要因のグループ数
dat.mean.method <- tapply(dat$score,dat$method,mean)
gaiseki.method <- outer(dat.mean.method,dat.mean.method,'-')
abs(gaiseki.method) > hsd

#刺激の多重比較
mse <- dat.aov.sum[[3]][[1]][[3]][2]
q <- qtukey(0.95,3,18)     #18はsubj:methodのDf
hsd <- q*sqrt(mse/15*3)    #(2/30)*3は2*(1/被験者の数)*(もう一つの要因のグループ数) 
dat.mean.stimulus <- tapply(dat$score,dat$stimulus,mean)
gaiseki.stimulus <- outer(dat.mean.stimulus,dat.mean.stimulus,'-')
abs(gaiseki.stimulus) > hsd
