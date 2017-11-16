dat<-read.table("http://www.matsuka.info/data_folder/aov01.txt",header=T)
head(dat)
summary(dat)
boxplot(dat$shoesize, col = "skyblue", main = "Dist. of Shoesize",ylab = "Size of shoe (in cm)")
boxplot(dat$h, col = "coral", main = "Dist. of Height", ylab = "Height (in cm)")
mean(dat$h)
var(dat$h)
sd(dat$h)
sqrt(var(dat$h))
cov(dat$h,dat$shoesize)
cov(dat[,1:2])
cor(dat[,1:2])
dat.meter = dat[,1:2]*0.01 
dat.h.ext5 = dat$h+5
dat.ss.ext1 = dat$shoesize+1
hANDshoe = dat$h+dat$shoesize
dat.h.meter.ext5 = dat$h*0.01+0.05
                                   
sum(dbinom(8:10,10,0.5))
sum(dbinom(3:5,5,1/8))

