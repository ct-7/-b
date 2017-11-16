dat<-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/datW03R.txt")

#めんどくさいやり方
means<-tapply(dat$shoesize,list(dat$gender, dat$affil),mean)
Ns<-tapply(dat$shoesize,list(dat$gender, dat$affil),length)
sds<-tapply(dat$shoesize,list(dat$gender, dat$affil),sd)
sems<-sds/sqrt(Ns)
plot(c(0,1),means[,1],type='o',col='skyblue', xlim=c(-0.1,1.1), lwd=2, 
     cex=2, pch=20, ylim=c(min(means)*0.975, max(means)*1.025), xlab="gender", ylab="shoesize",
     xaxt="n")
axis(1,c(0,1),c("female","male"))
lines(c(0,1),means[,2],type='o',col='orange',lwd=2,cex=2,pch=20)
lines(c(0,0),c(means[1,1]-sems[1,1],means[1,1]+sems[1,1]),col="skyblue",lwd=2)
lines(c(0,0),c(means[1,2]-sems[1,2],means[1,2]+sems[1,2]),col="orange",lwd=2)
lines(c(1,1),c(means[2,1]-sems[2,1],means[2,1]+sems[2,1]),col="skyblue",lwd=2)
lines(c(1,1),c(means[2,2]-sems[2,2],means[2,2]+sems[2,2]),col="orange",lwd=2)
legend("topleft",c("CogSci","PsySci"),col=c("skyblue","orange"),lwd=2)

#簡単なやり方
interaction.plot(dat$gender,dat$affil,dat$shoesize, pch=c(20,20), 
                 col=c("skyblue","orange"), xlab="gender", ylab="shoesize", 
                 trace.label="Affiliation")
dat.aov=aov(shoesize~gender*affil, data=dat)
dat.aov.sum=summary(dat.aov)
dat.aov.sum

#単純主効果
# testing simple main effect of gender
means<-tapply(dat$shoesize, list(dat$gender,dat$affil), mean)
SS_gen_CS<- 5*(means[2,1]^2 + means[1,1]^2 -0.5*sum(means[,1])^2) # SS_gender CS
SS_gen_PS<- 5*(means[2,2]^2 + means[1,2]^2 -0.5*sum(means[,2])^2) # SS_gender PS
dat.aov.sum=summary(dat.aov)   # ANOVA table
MSe=dat.aov.sum[[1]][4,3]      # MSE from ANOVA table or MSe=0.62
dfE=dat.aov.sum[[1]][4,1]      # DF for error or dfE=16
dfG=1                          # DF for gender
F_gen_CS=(SS_gen_CS/dfG)/MSe   # F-value for gender effect given CS
F_gen_PS=(SS_gen_PS/dfG)/MSe   # F-value for gender effect given PS
P_gen_CS=1-pf(F_gen_CS,1,dfE)  # p-value for gender effect given CS
P_gen_PS=1-pf(F_gen_PS,1,dfE)  # p-value for gender effect given PS

# testing simple main effect of affiliation
SS_affil_F<- 5*(means[1,1]^2+means[1,2]^2-0.5*sum(means[1,])^2) #SS_affil | F
SS_affil_M<- 5*(means[2,1]^2+means[2,2]^2-0.5*sum(means[2,])^2) #SS_affil | M
dfA=1                         # DF for affil
F_affil_F=SS_affil_F/dfA/MSe         # F-value for affiliation effect | F
F_affil_M=SS_affil_M/dfA/MSe         # F-value for affiliation effect | M
P_affil_F=1-pf(F_affil_F,1,dfE)      # p-value for affiliation effect | F
P_affil_M=1-pf(F_affil_M,1,dfE)      # p-value for affiliation effect | M


#単純主効果からの多重比較
dat<-read.csv("http://www.matsuka.info/data_folder/dktb321.txt")
interaction.plot(dat$duration,dat$method,dat$result, pch=c(20,20), col=c("skyblue","orange"), ylab="score", xlab="Duration",lwd=3,type='b',cex=2,trace.label="Method")
dat.aov=aov(result~method*duration,data=dat)
summary(dat.aov)
# useful r functions
source("http://peach.l.chiba-u.ac.jp/course_folder/tsme2017.R")
#sourceを実行しないと使えない
#単純主効果の後のtukeyのHSDを求めるためのコマンド
CRF.tsme(dat.aov, dat)   