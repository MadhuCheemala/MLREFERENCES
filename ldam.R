require(MASS)

# Load data
data(iris)
r <- lda(formula = Species ~ ., 
         data = iris, 
         prior = c(1,1,1)/3)



library(DiscriMiner)
mylda = linDA(iris[,1:4], iris[,5])
summary(mylda)
mylda$functions

plot(r)


library(car)
# install.packages('rattle')
data(wine, package='rattle')
attach(wine)
head(wine)


scatterplotMatrix(wine[2:6])

wine$Type





library(car)
data(wine,package = "rattle")

str(wine)
library(MASS)
ldadata<-lda(Type~., data=wine)
scatterplotMatrix(wine[2:6])

ldadata
plot(ldadata)



wine.lda.values <- predict(ldadata)

str(wine.lda.values)
ldahist(data = wine.lda.values$x[,1], g=Type)


ldahist(data = wine.lda.values$x[,2], g=Type)
plot(wine.lda.values$x[,1],wine.lda.values$x[,2])
plot(ldadata)


text(wine.lda.values$x[,1],wine.lda.values$x[,2],Type,cex=0.7,pos=4,col="red") # add labels

url <- 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/admission.csv'

admit <- read.csv(url)

head(admit)
str(admit)



adm=data.frame(admit)

plot(adm$GPA,adm$GMAT,col=adm$De)
text(adm$GPA,adm$GMAT,adm$De,cex=0.7,pos=4,col="red")


m1=lda(De~.,adm)
m1
predict(m1,newdata=data.frame(GPA=3.21,GMAT=497))
m2=qda(De~.,adm)
m2


predict(m2,newdata=data.frame(GPA=3.21,GMAT=497))

n=85
nt=60
neval=n-nt
rep=100


### LDA
set.seed(123456789)
errlin=dim(rep)
for (k in 1:rep) {
  train=sample(1:n,nt)
  ## linear discriminant analysis
  m1=lda(De~.,adm[train,])
  predict(m1,adm[-train,])$class
  tablin=table(adm$De[-train],predict(m1,adm[-train,])$class)
  errlin[k]=(neval-sum(diag(tablin)))/neval
}
merrlin=mean(errlin)
merrlin


## QDA
set.seed(123456789)
errqda=dim(rep)
for (k in 1:rep) {
  train=sample(1:n,nt)
  ## quadratic discriminant analysis
  m1=qda(De~.,adm[train,])
  predict(m1,adm[-train,])$class
  tablin=table(adm$De[-train],predict(m1,adm[-train,])$class)
  errqda[k]=(neval-sum(diag(tablin)))/neval
}
merrqda=mean(errlin)
merrqda

library(klaR)
partimat(De~.,data=adm,method="lda") 

## read data 
credit <- read.csv("http://www.biz.uiowa.edu/faculty/jledolter/DataMining/germancredit.csv")
head(credit,2) # See details about codification in the attached documentation.

cred1=credit[, c("Default","duration","amount","installment","age")]
head(cred1)
summary(cred1)

hist(cred1$duration)
cred1=data.frame(cred1)


library(MASS)
attach(cred1)
## LDA: class proportions of the training set used as prior probabilities
zlin=lda(Default~.,cred1)

# Confusion Matrix:
table(predict(zlin)$class, Default)

predict(zlin,newdata=data.frame(duration=6,amount=1100,installment=4,age=67))

predict(zlin,newdata=data.frame(duration=6,amount=1100, installment=4,age=67))$class 


zqua=qda(Default~.,cred1)

# Confusion Matrix:
table(predict(zqua)$class, Default)
predict(zqua,newdata=data.frame(duration=6,amount=1100,installment=4,age=67))
predict(zqua,newdata=data.frame(duration=6,amount=1100, installment=4,age=67))$class


























