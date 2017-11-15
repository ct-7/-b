dat<-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/waa05.csv")
dat$condition = factor(dat$condition, levels(dat$condition)[2:1])
medicineA = dat[dat$medicine == "Medicine A",]
plot(medicineA$condition,medicineA$blood.pressure,
     main ="Blood Pressure Before & After Taking Medicine A",
     ylab = "Blood Pressure",xlab ="condition",col = c("steelblue1","deepskyblue3"))
medicineB = dat[dat$medicine == "Medicine B",]
plot(medicineB$condition,medicineB$blood.pressure,
     main ="Blood Pressure Before & After Taking Medicine B",
     ylab = "Blood Pressure",xlab ="condition",col = c("darkorange","darkorange3"))

par(mfrow=c(1,2))

medicineA.aov = aov(blood.pressure ~ condition,data = medicineA)
summary(medicineA.aov)

pre.subj = medicineB[medicineB$condition == "pre",]
post.subj = medicineB[medicineB$condition == "post",]
medicineB.t_test = t.test(x = pre.subj$blood.pressure,y = post.subj$blood.pressure,paired = T)
medicineB.t_test

difference = pre.subj$blood.pressure - post.subj$blood.pressure
t.test(difference,mu = 0)
medicineB.aov = aov(blood.pressure ~ condition,data = medicineB)
summary(medicineB.aov)

plot(dat)

interaction.plot(dat$condition,  # x軸
                 dat$medicine,    # まとめる変数　　　　
                 dat$blood.pressure,    # y軸 
                 pch=c(20,20), 
                 col=c("skyblue","orange"),
                 ylab="Blood Pressure",
                 xlab="condition",
                 lwd=3,　
                 type='b',
                 cex=1.5,
                 trace.label="Medicine")
dat.aov<-aov(blood.pressure ~ medicine*condition + Error(subj +subj:condition),dat)
summary(dat.aov)
source("http://peach.l.chiba-u.ac.jp/course_folder/tsme.txt") 
TSME<-SPF.tsme(dat.aov,dat,"blood.pressure") 

medicineB.aov = aov(blood.pressure ~ condition + subj + Error(subj:condition),data = medicineB)
medicineB.aov.sum <- summary(medicineB.aov)
