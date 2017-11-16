source("http://peach.l.chiba-u.ac.jp/course_folder/jn_plot.R")
protest<-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/protest.csv")
lm <- lm(liking~sexism*protest,data=protest)
summary(lm)
jn_plot(lm, "protest", "sexism", alpha=.05)
#リニアモデルのオブジェクト、定性的変数、連続的変数、有意水準

poly.lm<-lm(grade~study+study.sq, data= dat)
summary(poly.lm)
plot(grade~study,data=dat,pch=20,xlab = "hours studied",xlim=c(3,27), ylim =c(0,110),cex=2)
x = seq(0,30,0.1)
y = -15.16+9.07771*x-0.18686*x^2 
lines(x,y,col='red',lwd=3)

dat <- read.csv("http://www.matsuka.info/data_folder/tdkReg01.csv")
dat.lm <- lm(sales~price,data=dat)
par(mfrow=c(2,2))
plot(dat.lm)
