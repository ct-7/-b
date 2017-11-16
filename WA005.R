#WAA05.1
dat<-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/waa05.csv") 
dat$condition = factor(dat$condition, levels(dat$condition)[2:1])

dat1 <- dat[dat$medicine == "Medicine A",]
dat2 <- dat[dat$medicine == "Medicine B",]
dat.medA.post <- dat[dat$medicine == "Medicine A" & dat$condition == "post",]
dat.medB.post <- dat[dat$medicine == "Medicine B" & dat$condition == "post",]
boxplot(dat1$blood.pressure ~ dat1$condition,xlab = "condition", 
        ylab = "Blood pressure",main = "Blood Pressure Before & After taking Medicine A",
        col = c("skyblue","skyblue1"))
boxplot(dat2$blood.pressure ~ dat2$condition,xlab = "condition", 
        ylab = "Blood pressure",main = "Blood Pressure Before & After taking Medicine B",
        ylim = c(115,150),col = c("orange","sienna1"))
  
#WAA05.2
# Two Sample T-test
d <- dat.medA.pre - dat.medA.post
t.test(d,mu = 0)
#有意に差が見られたため帰無仮説を棄却。効果が見られた。

#WAA05.3 
d <- dat.medB.pre - dat.medB.post
t.test(d,mu = 0)
#有意に差はないため帰無仮説を採択。効果が見られなかった。

#WAA05.4
t.test(dat.medA.post,dat.medB.post,var.equal = T)
#有意に差が見られたため帰無仮説を棄却。統計的な差が見られた。

#WA005.5