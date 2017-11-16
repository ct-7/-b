#WAA11.1
dat <- read.csv("http://peach.l.chiba-u.ac.jp/course_folder/waa07_2.csv")
dat.aov <- aov(grade~study.type,data=dat)
summary(dat.aov)
TukeyHSD(dat.aov)

#WAA11.2
dat.means<-tapply(dat$grade,dat$study.type,mean)
cont1=c(-2,1,0,1,-2)
bunshi1=sum(cont1*dat.means)
bunbo1=sqrt(75*(sum((cont1^2)/8)))
F.value=(bunshi1/bunbo1)^2
new.F=3*qf(0.95,4,45)
F.value>new.F　　#new.Fの方が小さいと有意

cont2=c(2,-1,-2,-1,-2)
bunshi2=sum(cont2*dat.means)
bunbo2=sqrt(75*(sum((cont2^2)/8)))
F.value=(bunshi2/bunbo2)^2
new.F=3*qf(0.95,4,45)
F.value>new.F 

cont3=c(0,0,0,-1,1)
bunshi3=sum(cont3*dat.means)
bunbo3=sqrt(75*(sum((cont3^2)/8)))
F.value=(bunshi3/bunbo3)^2
new.F=3*qf(0.95,4,45)
F.value>new.F


#WAA11.3
dat <- read.table("http://www.matsuka.info/data_folder/aov01.txt",header=T)
head(dat)
dat.aov <- aov(shoesize~club,data=dat)
summary(dat.aov)
boxplot(dat$shoesize~dat$club,col=c("blue","cyan","gray"))

new.alpha = 1-(0.95)^(1/4)
N <- tapply(dat$shoesize,dat$club,length)

dat.means<-tapply(dat$shoesize,dat$club,mean)
cont1=c(-1,1,0)
bunshi1=sum(cont1*dat.means)
bunbo1=sqrt(2.243*(sum((cont1^2)/N))) 
t1.value=bunshi1/bunbo1
2*(1-pt(abs(t1.value),67)) 

cont2=c(-1,0,1)
bunshi2=sum(cont2*dat.means)
bunbo2=sqrt(2.243*(sum((cont2^2)/N))) 
t2.value=bunshi2/bunbo2
2*(1-pt(abs(t2.value),67)) 

cont3=c(0,-1,1)
bunshi3=sum(cont3*dat.means)
bunbo3=sqrt(2.243*(sum((cont3^2)/N))) 
t3.value=bunshi3/bunbo3
2*(1-pt(abs(t3.value),67)) 

cont4=c(-2,1,1)
bunshi4=sum(cont4*dat.means)
bunbo4=sqrt(2.243*(sum((cont4^2)/N))) 
t4.value=bunshi4/bunbo4
2*(1-pt(abs(t4.value),67)) 
