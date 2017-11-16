freq<-c(33,37,7,23)
pref<-factor(c('soba','udon','soba','udon'))
region<-factor(c('east','east','west','west'))
dat<-data.frame(pref,region,freq)
dat.table=table(pref,region)
dat.table[cbind(pref,region)]<-freq

dat.loglin.row <- loglin(dat.table,1,param=T,fit=T) #行で分析する時は１、列は２
dat.loglin
dat.loglin.col <- loglin(dat.table,2,param=T,fit=T)
1-chisq(dat.loglin.row$lrt,2)
1-pchisq(dat.loglin.row$lrt,2) #帰無仮説が棄却されない方がいいモデル
exp(3.131699)*exp(0.4236489)

dat.loglinIND<-loglin(dat.table,list(1,2), fit=T,param=T)
1-pchisq(dat.loglinIND$lrt,1) #検定

SAT <- loglin(dat.table,list(1,2,c(1,2)),param=T,fit=T)
1-pchisq(SAT$lrt,0)

lsq =　dat.loglinIND$lrt
df =
自由度を減らすことによって減少できた誤差

freq<-c(9,5,2,4,16,10,26,28)
gender<-factor(c(rep('female',4),c(rep('male',4))))
affil<-factor(rep(c('L','L','E','E'),2))
circle<-factor(rep(c('tennis','astro'),4))
dat<-data.frame(genedr,affil,circle,freq)

dat.table<-table(gender,affil,circle)
dat.table[cbind(gender,affil,circle)]<-freq

model2.loglin<-loglin(dat.table,list(1), fit=T,param=T)
model2.loglin
1-pchisq(model2.loglin.row$lrt,2)

model3.loglin<-loglin(dat.table,list(1,3), fit=T,param=T)
model3.loglin

model4.loglin<-loglin(dat.table,list(1,2,3), fit=T,param=T)
model4.loglin

model5.loglin<-loglin(dat.table,list(1,2,3), fit=T,param=T)