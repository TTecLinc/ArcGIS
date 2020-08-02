setwd("~")
library(spdep)
gal <- read.gal("Province31.gal")
gwt <- read.gwt2nb("Province31.gwt")
summary(gal)

# Check the internal structure of function 
str(gal)
#  $ : int [1:2] 15 19 
#  num 1 is continguous to num 15 & 19
gal [[1]] # POLY_ID==1

# the d between shanghai(1) to others 30
attr(gwt, "GeoDa")$dist[[1]]

# Space Weighted Matrix
gal.mat <- nb2mat(gal)
gal.mat[1,15]

dis <- attr(gwt,"GeoDa")$dist
#for (i in 1:31) dist[[i]] <-append(dist[[i]],0,after=i-1)
gwt.mat=do.call(rbind,dist)

all(diag(gwt.mat)==0)

diag(gwt.mat)<-1
any(gwt.mat==0)
gwt.mat<-1/gwt.mat
diag(gwt.mat)<-0
gwt.mat<- t(apply(gwt.mat, 1, function(x), x/sum(x)))


xx<-replicate(999, sample(mydata$product))
moran999 <- apply(xx, 2, function(x), {aa=monran.test(x,mat2listw(gal.mat));return(aa$estimate["Moran I statistic"])})
moran1 <- moran.test(mydata$product,listw=mat2listw(gal.mat)$estimate["Moran I statistic"])
morans <-c(moran999,moran1)

hist(morans,freq=F,breaks=100)
lines(density(morans))
abline(v=moran1)

