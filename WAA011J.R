f=c(24.1,23.9,24.4,24.4,23.5)
dat<-data.frame(ssize=c(f,m),gender=c(rep("f",5),rep("m",5)))
dat.aov<-aov(ssize ~ gender,data=dat)
dat<-read.table("http://www.matsuka.info/data_folder/aov01.txt")
dat.aov<-aov(shoesize~club, dat)
summary(dat.aov)
TukeyHSD(dat.aov) #全部のグループを比較する

dat<-read.csv("http://www.matsuka.info/data_folder/dktb312.txt",col.names=c("dump","method","result"))
levels(dat$method)<-c('free','repeat','sentence','image')
dat.aov <- aov(result~method,data=dat)
summary(dat.aov)
TukeyHSD(dat.aov)

#a priori Bonferroni
dat.means<-tapply(dat$result,dat$method,mean)
cont=c(-3,1,1,1)
bunshi=sum(cont*dat.means)
bunbo=sqrt(5.29*(sum((cont^2)/8))) #5.29=MSE
t.value=bunshi/bunbo
2*(1-pt(abs(t.value),28)) #abs=絶対値

#a  osteriori Scheffe
dat.means<-tapply(dat$result,dat$method,mean)
cont=c(-3,1,1,1)
bunshi=sum(cont*dat.means)
bunbo=sqrt(5.29*(sum((cont^2)/8)))
F.value=(bunshi/bunbo)^2
new.F=3*qf(0.95,3,28)
