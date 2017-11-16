dat<-read.csv("http://www.matsuka.info/data_folder/datWA01.txt")
mean.M <-mean(dat$h[dat$gender=="M"])
sigma = 10
n.M = length(dat$h[dat$gender=="M"])
z.value=(mean.M-171)/(sqrt(sigma/n.M))
(1-pnorm(abs(z.value)))*2

#ZŒŸ’è
dat.M = dat[dat$gender == "M",]
dat.F = dat[dat$gender == "F",]
mu = 171
sigma = 10
mean.M = mean(dat.M$h)
mean.M
n.M = nrow(dat.M)
z.M = (mean.M - mu)/sqrt(10^2/n.M)
(1-pnorm(abs(z.M)))*2

mu = 158
sigma = 5
mean.F = mean(dat.F$h)
mean.F
n.F = nrow(dat.F)
z.F = (mean.F - mu)/sqrt(5^2/n.F)
(1-pnorm(abs(z.F)))*2

#tŒŸ’è‚ß‚ñ‚Ç‚¢•û
ssize = c(24,25,26,23.5,25,27,24,22,27.5,28)
ssize.mean = mean(ssize)
ssize.var = var(ssize)
N = 10
t.value=(ssize.mean-24)/(sqrt(ssize.var/N))
(1-pt(abs(t.value),df=9))*2

#Šy‚È•ûitŒŸ’è‡@j
ssize = c(24,25,26,23.5,25,27,24,22,27.5,28)
t.test(ssize,mu = 24)

t.test(dat.M$h,mu = 171)
t.test(dat.F$h,mu = 158)

dat.M = c(27,26.5,27.5,25,26.5,26)
t.test(dat.M,mu = 25)

#tŒŸ’è‡A
dat <- read.csv("http://peach.l.chiba-u.ac.jp/course_folder/mobH.csv")
dat.May = dat[dat$mob=="may",]
dat.Dec = dat[dat$mob == "dec",]
t.test(dat.May$height,dat.Dec$height,var.equal = T)
