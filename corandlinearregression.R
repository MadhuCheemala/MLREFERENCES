library("MASS")
data(cats)
str(cats)

with(cats,cor(Bwt,Hwt))
with(cats,plot(Bwt,Hwt))
with(cats, cor.test(Bwt, Hwt))
with(cats, cor.test(Bwt, Hwt, alternative="greater", conf.level=.8))
with(cats, cor.test(~ Bwt + Hwt, subset=(Sex=="F")))
with(cats, cor.test(~ Bwt + Hwt, subset=(Sex=="M")))

with(cats, plot(Bwt, Hwt, type="n", xlab="Body Weight in kg",
                               ylab="Heart Weight in g",
                           main="Heart Weight vs. Body Weight of Cats"))
with(cats,points(Bwt[Sex=="F"],Hwt[Sex=="F"],pch=16,col="red"))
with(cats,points(Bwt[Sex=="M"],Hwt[Sex=="M"],pch=17,col="blue"))

data("cement")
str(cement)
cor(cement)
cov(cement)
corv<-cov(cement)
cov2cor(corv)
pairs(cement)


ls()
rm(cement, corv) 

coach1 = c(1,2,3,4,5,6,7,8,9,10)
coach2 = c(4,8,1,5,9,2,10,7,3,6)
cor(coach1, coach2, method="spearman")
cor.test(coach1, coach2, method="spearman")
cor(coach1, coach2, method="kendall")
cor.test(coach1, coach2, method="kendall")


data(cats) 
attach(cats) 
anova(lm(Hwt ~ Bwt))
lm.out<-lm(Hwt ~ Bwt)
options(show.signif.stars=F)

plot(Hwt ~ Bwt, main="Kitty Cat Plot")
abline(lm.out, col="red")


runif(10, min=1, max=3)

data("cats",package="MASS")

require(pdfetch)


CXW <- pdfetch_YAHOO("CXW",fields="adjclose",
                     from=as.Date("2000-01-01",
                                  to=as.Date("2014-09-30")))

summary(CXW)

plot(CXW)

returns <- na.omit(as.vector(diff(log(CXW$CXW))))
summary(returns)
plot(returns,type="l")

quantile(returns,c(0.25,0.5,0.75))

plot(ecdf(returns), main="Empirical CDF of CXW returns")
hist(returns,n=101,probability=TRUE)

plot(density(returns),main="Estimated pdf of CXW returns")

hist(returns,n=101,probability=TRUE)
lines(density(returns),lty="dashed")








