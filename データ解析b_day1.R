install.packages("DT")
library("dplyr")
library("ggplot2")
library("DT")
library("broom")

dat<-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/Fdat_c1v1.csv")
dat.glm <- glm(affil ~ expt,family = binomial,data = dat)
summary(dat.glm)
x=seq((min(dat$expt)-2), (max(dat$expt)+2), 0.1)
cf = coef(dat.glm)
y.prob = 1/(1+exp(-1*(cf[1]+cf[2]*x)))

plot(dat$expt,dat$affil,ylab = "affil",xlab = "score",pch=20,yaxt ="n")
axis(2, at = c(1,2),labels=c("bs","jpn"))
lines(x,y.prob+1,lty=2,lwd=4,col='skyblue')

dat.c9 <- read.csv("http://peach.l.chiba-u.ac.jp/course_folder/Fdat_c9.csv")
head(dat.c9)
plot(dat.c9$med,dat.c9$bp)
dat.c9_aov <- aov(bp~med,data=dat.c9)
summary(dat.c9_aov)

dat.cp_lm <- lm(bp ~ med,data = dat.c9)
summary(dat.cp_lm)

####Q2###
dat.Q2 <- read.csv("http://peach.l.chiba-u.ac.jp/course_folder/Fdat_q2.csv")
# visualization
interaction.plot(dat.Q2$med,dat.Q2$ingred,dat.Q2$bp,type='o',
                 col = c("skyblue", "coral"),lwd =3, cex = 2,
                 pch=c(18,19),xlab = "quantitily", ylab = "Blood Pressure",
                 trace.label="type of medicine",legend = T)
# descriptive stats
tapply(dat.Q2$bp,list(dat.Q2$med,dat.Q2$ingred),mean)
### Linear Regression
dat.Q2.lm <- lm(bp~med*ingred,data = dat.Q2)
summary(dat.Q2.lm)
#一次の関係があるかどうか確認できる
dat.Q2.lm.contrast = lm(bp~med*ingred, contrasts=list(med=contr.poly), data=dat.Q2)
summary(dat.Q2.lm.contrast)
anova(dat.Q2.lm.contrast)
### ANOVA
dat.Q2.aov = aov(bp~med*ingred,data=dat.Q2)
summary(dat.Q2.aov)


####Q3###
dat.Q3 <- read.csv("http://peach.l.chiba-u.ac.jp/course_folder/Fdat_q3.csv")
plot(hours~A,data=dat.Q3)
dat.Q3.lm <- lm(hours~btype,data=dat.Q3)
summary(dat.Q3.lm)
dat.Q3.aov <- aov(hours~btype,data=dat.Q3)
summary(dat.Q3.aov)
TukeyHSD(dat.Q3.aov)

# Q3 example 2
# visualization
plot(dat.Q3_A1$hours, dat.Q3V1$A,type = 'p',pch=20,cex =2, 
     xlab = "hours cleaned", ylab= "Blood Type",yaxt ="n")
axis(2, at = c(0,1), labels = c("not A", "A"))
# GLM
dat.Q3_A1.glm <- glm(A~hours,family="binomial",data=dat.Q3_A1 )
# plotting result
x.temp = seq(100,330,1)
glm.cf = coef(dat.Q3V1.glm)
y.temp = 1/(1+exp(-1*(glm.cf[1]+glm.cf[2]*x.temp)))
lines(x.temp,y.temp,col='red',lwd=2)

###Q4###
dat.Q4 <-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/DAB2014_final_regression.csv")
head(dat.Q4)
plot(dat.Q4)
plot(dat.Q4$group,dat.Q4$score)
dat.Q4$group <- as.numeric(dat.Q4$group)
dat.Q4$gender <- as.numeric(dat.Q4$gender)
dat.Q4$income <- as.numeric(dat.Q4$income) #ダミー変数を作る

#回帰分析
##yのモデル##
dat.Q4_lm <- lm(dat.Q4$score ~ income,dat.Q4)
summary(dat.Q4_lm) 　
formula(dat.Q4_lm) 
coef(summary(dat.Q4_lm))[,1]
##xのモデル##
dat.Q4_lm_group <- lm(score~group,dat.Q4)
summary(dat.Q4_lm_group) 
formula(dat.Q4_lm_group)  
coef(summary(dat.Q4_lm_group))[,1]

p <- ggplot(dat.Q4,aes(x=group,y=score))
p <- p + geom_point() + geom_smooth(method=lm,color="black")
p
plot(dat.Q4$score ~ as.numeric(dat.Q4$group),pch = 20,xlab = "group",ylab = "score",xaxt = "n")
axis(1,c(1,2),c("g1","g2"))
abline(dat.Q4_lm_group)
plot(dat.Q4$score ~ as.numeric(dat.Q4$income),pch = 20,xlab = "income",ylab = "score",xaxt = "n")
axis(1,c(1,2),c("high","low"))
abline(dat.Q4_lm)

#分散分析
dat.Q4 <-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/DAB2014_final_regression.csv")
summary(aov(score ~ group,data=dat.Q4)) 
dat.Q4_Int_aov <- aov(score ~ gender * income,data=dat.Q4)
summary(dat.Q4_Int_aov)
interaction.plot(dat.Q4$gender,dat.Q4$income,dat.Q4$score,ylab = "score",xlab = "gendar")
dat.Q4_lm_Int <- lm(score ~ gender * income, data = dat.Q4)
summary(dat.Q4_lm_Int)
CRF.tsme(dat.Q4_Int_aov,dat.Q4)

TukeyHSD(dat.Q4_Int_aov)


#独立性の検定を行う
chisq.test(table(dat.Q4$group,dat.Q4$income))
