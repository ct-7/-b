#カイニ乗適合度検定 確率で入力しないと処理してくれない
chisq.test(c(72,23,16,49),p=rep(40,4),rescale.p=T)

#独立性の検定　行列・テーブルを入れると勝手に処理してくれる
M=matrix(c(52,48,8,42),nrow=2)
chisq.test(M,correct=F)　#correct=Tは連続補正


dat <- read.csv("http://www.matsuka.info/data_folder/cda7-16.csv")
dat.tab <- table(dat[,3:4])
#妊娠と生存
chisq.test(dat.tab)
#喫煙と生存
chisq.test(table(dat[,c(2,4)]))
#年齢と生存
chisq.test(table(dat[,c(1,4)]))

#ロジスティック回帰
dat<-read.csv("http://www.matsuka.info/data_folder/datWA01.txt")
dat.lr<-glm(gender~shoesize, family=binomial, data=dat)
summary(dat.lr) #ロジスティック回帰の結果出力
x=seq((min(dat$shoesize)-2), (max(dat$shoesize)+2), 0.1)
keisu  = coef(dat.lr)　#リニアモデルで推定されたものを取り出す

#線形モデル
y.prob=1/(1+exp(-1*(keisu[1]+keisu[2]*x)))

#線形を非線形に変える
plot(dat$shoesize, dat$gender, pch=20, cex=3, xlab="Shoe size", 
     ylab="gender", col='blue', ylim = c(0.5, 2.5), yaxt="n")
axis(2, at = c(1,2),labels=c("female","male"))
lines(x,y.prob+1,lty=2,lwd=4,col='green')
abline(dat.lm,lwd=4,col='red')


dat<-read.csv("http://www.matsuka.info/data_folder/cda7-16.csv")
chisq.test(table(dat[,3:4]))
dat.lrAge <- glm(survival~age,family=binomial,data=dat)
summary(dat.lrAge)
dat.lrCig <- glm(survival~Ncigarettes,family=binomial,data=dat)
summary(dat.lrCig)

#複雑な式
dat.glmAllMult=glm(survival~age*Ncigarettes*NdaysGESTATION,family=binomial,data=dat)
summary(dat.glmAllMult)


#いらない変数を削ってくれる
#一番高次の交互作用から変数をみる
library(MASS)
dat.glm.opt=stepAIC(dat.glmAllMult)　#使い方に注意　明確なモデルがある場合はそっちをつかう
summary(dat.glm.opt) #全てが有意である必要はない　
