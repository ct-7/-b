

##データ解析基礎論 a weekly assignment B04
####WAB04.1

~~~R
##data input
dat<-read.csv("http://peach.l.chiba-u.ac.jp/course_folder/waa05.csv")
dat$condition = factor(dat$condition, levels(dat$condition)[2:1])
medicineA = dat[dat$medicine == "Medicine A",]
medicineB = dat[dat$medicine == "Medicine B",]

#plot
#画面を二つに分割する
par(mfrow=c(1,2))
#medicineA
plot(medicineA$condition,medicineA$blood.pressure,
     main ="Blood Pressure Before & After Taking Medicine A",
     ylab = "Blood Pressure",
     xlab ="condition",
     col = c("steelblue1","deepskyblue3"))
#medicineB
plot(medicineB$condition,medicineB$blood.pressure,
     main ="Blood Pressure Before & After Taking Medicine B",
     ylab = "Blood Pressure",
     xlab ="condition",
     col = c("darkorange","darkorange3"))
~~~

![Plot](/Users/kaede/Pictures/Plot.png)
<div style="page-break-before:always"></div>
####WAB04.2
~~~R
medicineA.aov = aov(blood.pressure ~ condition + subj + Error(subj:condition),data = medicineA)
medicineA.aov.sum <- summary(medicineA.aov)
#出力結果
Error: subj:condition
          Df Sum Sq Mean Sq  F value   Pr(>F)    
condition  1  17056   17056 3473.079  < 2e-16 ***  有意
subj      49   2170      44    9.017 9.11e-13 ***　個人差が有意である
Residuals 49    241       5                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#qtukeyのやり方（TukeyHSDは被験者間要因のみ適用可能）
str(medicineA.aov.sum)　#中身確認
mse <- medicineA.aov.sum[[1]][[1]][3,3]
q <- qtukey(0.95,2,49)
hsd <- q*sqrt(mse/50) #50は被験者の数
dat.mean <- tapply(medicineA$blood.pressure,medicineA$condition,mean)
gaiseki <- outer(dat.mean,dat.mean,'-')
abs(gaiseki) > hsd
#出力結果
       pre  post
pre  FALSE  TRUE
post  TRUE FALSE
~~~
検定の結果、投薬前と投薬後では有意に差があることがわかったので、薬Aは血圧の低下に効果があると言える
<div style="page-break-before:always"></div>
####WAB04.3
~~~R
pre.subj = medicineB[medicineB$condition == "pre",]
post.subj = medicineB[medicineB$condition == "post",]
#paired を使う方法
medicineB.t_test = t.test(x = pre.subj$blood.pressure,y = post.subj$blood.pressure,paired = T)
medicineB.t_test
#differenceを計算してmuを使う方法
difference = pre.subj$blood.pressure - post.subj$blood.pressure
t.test(difference,mu = 0)

#出力結果は同じ
#Paired
	Paired t-test

data:  pre.subj$blood.pressure and post.subj$blood.pressure
t = -0.045186, df = 49, p-value = 0.9641
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -1.818946  1.738946
sample estimates:
mean of the differences -0.04 
#one sample	
　　One Sample t-test

data:  difference
t = -0.045186, df = 49, p-value = 0.9641
alternative hypothesis: true mean is not equal to 0
95 percent confidence interval:
 -1.818946  1.738946
sample estimates:
mean of x -0.04 
~~~
検定の結果、有意に差がないことがわかったので、薬Bは血圧の低下に効果があるとは言えない


####WAB04.4
~~~R
#被験者内要因のため上のコードは間違い
#medicineB.aov = aov(blood.pressure ~ condition,data = medicineB)
#summary(medicineB.aov)
#間違いの出力結果
            Df Sum Sq Mean Sq F value Pr(>F)
condition    1      0    0.04   0.001  0.973
Residuals   98   3449   35.20

#正解のコード
medicineB.aov = aov(blood.pressure ~ condition + subj + Error(subj:condition),data = medicineB)
medicineB.aov.sum <- summary(medicineB.aov)
#正解の出力結果
Error: subj:condition
          Df Sum Sq Mean Sq F value   Pr(>F)    
condition  1      0    0.04   0.002 0.964143    
subj      49   2489   50.80   2.593 0.000559 ***
Residuals 49    960   19.59                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
~~~
検定の結果、有意に差がないことがわかったので、薬Bは血圧の低下に効果があるとは言えない
<div style="page-break-before:always"></div>
####WAB04.5
~~~R
#plot
interaction.plot(dat$condition,  # x軸
                 dat$medicine,    # まとめる変数　　　　
                 dat$blood.pressure,    # y軸 
                 pch=c(20,20), 
                 col=c("skyblue","orange"),
                 ylab="Blood Pressure",xlab="condition",
                 lwd=3,type='b',cex=1.5,
                 trace.label="Medicine")
~~~
![Rplot01](/Users/kaede/Pictures/Rplot02.png)



~~~R
#anova
dat.aov<-aov(blood.pressure ~ medicine*condition + Error(subj + subj:condition),dat)
summary(dat.aov)
#出力結果
Error: subj
          Df Sum Sq Mean Sq F value Pr(>F)    
medicine   1   7614    7614   160.1 <2e-16 ***
Residuals 98   4659      48                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Error: subj:condition
                   Df Sum Sq Mean Sq F value Pr(>F)    
condition           1   8502    8502   694.0 <2e-16 ***
medicine:condition  1   8554    8554   698.3 <2e-16 ***
Residuals          98   1201      12                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
~~~

交互作用が有意であるため、単純主効果の検定を行う

~~~R
#単純主効果の検定
source("http://peach.l.chiba-u.ac.jp/course_folder/tsme.txt") 
TSME<-SPF.tsme(dat.aov,dat,"blood.pressure") 
#出力結果
simple main effect test for BETWEEEN subject factor 
               ss  df       ms        f      p
post     16154.41   1 16154.41 540.3366 0.0000    #postの条件で薬の効果が有意
pre         13.69   1    13.69   0.4579 0.4994
residual  5859.80 196    29.90                

 Tukey HSD test - between subject factor @ condition = post 　#薬Aと薬Bでは有意な差があった
           Medicine A Medicine B
Medicine A      FALSE       TRUE
Medicine B       TRUE      FALSE

 simple main effect test for WITHIN subject factor 
                 ss df       ms         f      p　　　#薬Aには有意な差がある
Medicine A 17056.36  1 17056.36 1.392e+03 0.0000
Medicine B     0.04  1     0.04 3.265e-03 0.9545
residual    1200.60 98    12.25                 

 Tukey HSD test - within subject factor @ medicine = Medicine A 　
      post   pre　　　　#薬Aでは投薬前後で有意な差があった
post FALSE  TRUE
pre   TRUE FALSE
~~~


