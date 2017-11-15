# Split Plot Factorial (SFP)
source("http://peach.l.chiba-u.ac.jp/course_folder/tsme.txt")
dat<-read.csv("http://www.matsuka.info/data_folder/dktb3211.txt")
dat.aov<-aov(result~method*duration+Error(s+s:duration),dat)

TSME<-SPF.tsme(dat.aov,dat,"result")
dat.aov.sum<-summary(dat.aov)

str(dat.aov.sum)
temp = dat.aov.sum[[2]]
str(temp)
temp.p = temp[[1]]
temp.pp = temp.p[3,]
temp.pp[3]

# within-subjcts factor
DF_error.W = dat.aov.sum[[2]][[1]][3,1]
MS_error.W = dat.aov.sum[[2]][[1]][3,3]

q <- qtukey(0.95,4,DF_error.W)
hsd<-q*sqrt(MS_error.W/(5*2))
dat.means.duration<-tapply(dat$result,dat$duration,mean)
abs(outer(dat.means.duration,dat.means.duration,'-'))>hsd

# between-subjects factor
DF_error.B = dat.aov.sum[[1]][[1]][2,1]
MS_error.B = dat.aov.sum[[1]][[1]][2,3]

q <- qtukey(0.95,2,DF_error.B)
hsd<-q*sqrt(MS_error.B/(5*4))

dat.means.method<-tapply(dat$result,dat$method,mean)
abs(outer(dat.means.method,dat.means.method,'-'))>hsd

# randomized block factorial
dat<-read.csv("http://www.matsuka.info/data_folder/dktb3218.txt")
dat.aov<-aov(result~method+duration+method:duration +
               Error(s+method:s+duration:s+method:duration:s), data=dat)
TSME<-RBF.tsme(dat.aov, dat, "result")

dat.aov.sum<-summary(dat.aov)
# testing for method
mse  = dat.aov.sum[[2]][[1]][2,3]
qv=qtukey(0.95,2,4)
hsd=qv*sqrt(mse/(5*4))
dat.means.method=tapply(dat$result,dat$method,mean)
abs(outer(dat.means.method,dat.means.method,'-')) > hsd  #絶対ちがhsdより大きいか

# testing for duration
mse  = dat.aov.sum[[3]][[1]][2,3]
qv=qtukey(0.95,4,12)
hsd=qv*sqrt(mse/(5*2))
dat.means.duration=tapply(dat$result,dat$duration,mean)
abs(outer(dat.means.duration,dat.means.duration,'-')) > hsd