#WA007
#WA007.1A
dat<- read.csv("http://peach.l.chiba-u.ac.jp/course_folder/waa07_1.csv")
dat1.lm <- lm(dat$resp ~ as.numeric(dat$year))
summary(dat1.lm)
plot(as.numeric(dat$year)-1,dat$resp,xaxt="n",xlim=c(-0.5,1.5),pch=20,xlab="grade",ylab="Respnse")
axis(1,at=c(0,1),c("freshman","sophmore"))
abline(dat1.lm,col = "red",lwd=2)

#WA007.1B
dat2.lm <- lm(resp ~ dept,data=dat)
summary(dat2.lm)
plot(as.numeric(dat$dept)-1,dat$resp,xaxt="n",xlim=c(-0.5,1.5),pch=20,xlab="dept",ylab="Response")
axis(1,at=c(0,1),c("freshman","sophmore"))
abline(dat2.lm,col = "red",lwd=2)

#WA007.1C
dat3.lm <- lm(resp ~ year * dept,data=dat)
summary(dat3.lm)
interaction.plot(dat$year, dat$dept, dat$resp, trace.label = "department",
                 xlab = "Grade",ylab = "Response", lwd = 1.5,type="o",
                 pch = c(17,20),cex =1, col = c('skyblue','coral'),legend="T")

#WA007.2
dat <- read.csv("http://peach.l.chiba-u.ac.jp/course_folder/waa07_2.csv")
study.type <- data.frame(g1=dat$grade,c1=c(rep(0,10), rep(1,10), rep(0,30)),
                                      c2=c(rep(0,20), rep(1,10), rep(0,20)),
                                      c3=c(rep(0,30), rep(1,10), rep(0,10)),
                                      c4=c(rep(0,40), rep(1,10)))
dat.lm<-lm(g1~c1+c2+c3+c4,data=study.type)
summary(dat.lm)
plot(dat$study,dat$grade,pch=20,ylab="grade",xlab="hour studied",lwd=2)
x = seq(0,30,0.1)
y = -15.16+9.07771*x-0.18686*x^2 
lines(x,y,col='red',lwd=3)

#WA007.3
plot(dat$study,dat$grade,pch=20,xlab="hours studies",ylab="grade",ylim=c(0,110),cex=1.5)

cont <- contr.poly(4)
cont
CL = cont[,1]
CL1=c(rep(0,10),rep(CL[1],20),rep(0,50))
CQ = cont[,2]
CQ1=c(rep(0,))
result <- dat$grade[dat$study==5]
cont.lm <- lm(result~CL)

交互作用だとどの区間で有意な差があるのかわからない
→jn_plot(lm, "protest", "sexism", alpha=.05)で確認する
