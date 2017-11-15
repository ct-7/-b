dat <- read.table("http://www.matsuka.info/data_folder/tdkPCA01.txt")
head(dat)
dat.pca <- princomp(dat)   #主成分分析
summary(dat.pca)           #結果
biplot(dat.pca)            #可視化

dat.pca$score       #主成分得点
dat.pca$loadings　　#負荷量


dat <- read.csv("http://www.matsuka.info/data_folder/tdkCFA.csv")
head(dat)
dat.pca <- princomp(dat)
summary(dat.pca)
screeplot(dat.pca)
screeplot(dat.pca,type='lines')   #成分の数を決定するために情報量を可視化
biplot(dat.pca)
biplot(dat.pca,choices=c(2,3))    #成分を変える
dat2s.pca <- primncomp(scale(dat2))  #分散に意味がないときはスケーリングした方が良い

