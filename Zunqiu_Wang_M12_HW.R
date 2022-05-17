#1
#a
library(ALL)
data(ALL)
IsB <- factor(ALL$BT %in% c("B","B1","B2","B3","B4"))

#b
data <- as.matrix(exprs(ALL[c('39317_at', '38018_g_at'),]))
library(rpart)
c.tr <- rpart(IsB ~., data = data.frame(t(data)))
rpartpred <- predict(c.tr, type="class")
#rpartpred <- factor(rpartpred, levels=c('TRUE', 'FALSE'), labels = factor(c('Is B', 'Not B')))
#IsB <- factor(IsB, levels=c('TRUE', 'FALSE'), labels = factor(c('Is B', 'Not B')))
table(rpartpred, IsB) 

library(ROCR)
pred.prb <- predict(c.tr, type="prob")[,2]
pred <- prediction(pred.prb, IsB == "TRUE")
perf <- performance(pred, 'tpr','fpr')
plot(perf)

#c
mcr <- sum(table(rpartpred, IsB)[2], table(rpartpred, IsB)[3])/length(IsB)
mcr

fnr <- table(rpartpred, IsB)[3]/sum(table(rpartpred, IsB)[3],table(rpartpred, IsB)[4])
fnr

fpr <- table(rpartpred, IsB)[2]/sum(table(rpartpred, IsB)[1],table(rpartpred, IsB)[2])
tnr <- 1-fpr
tnr

performance(pred, 'auc')@y.values[[1]]

#d
library(caret)
data.rpart <- data.frame(IsB,t(data))
n <- dim(data.rpart)[1]
index <- 1:n
K <- 10
flds <- createFolds(index, k=K)
fnr.cv.raw <- rep(NA,K)
for (i in 1:K) {
  testID <- flds[[i]]
  data.tr <- data.rpart[-testID,]
  data.test <- data.rpart[testID,]
  rpart.cv <- rpart(IsB~.,data = data.tr)
  rpart.cv.pred <- predict(rpart.cv, newdata=data.test, type="class")
  fnr.cv.raw[i] <- sum(rpart.cv.pred == "FALSE" & data.test$IsB == 'TRUE')/sum(data.test$IsB == 'TRUE')
}
fnr.cv <- mean(fnr.cv.raw)
fnr.cv

#e
data.lr <- data.frame(IsB,t(data))
fit.lr <- glm(IsB ~., family = binomial(link = 'logit'), data = data.lr)
summary(fit.lr)
confint(fit.lr, level = 0.8)

#f
n <- dim(data.lr)[1]
mcr.cv.raw <- rep(NA,n)
for (i in 1:n) {
  data.tr <- data.lr[-i,]
  data.test <- data.lr[i,]
  fit.lr <- glm(IsB~.,family=binomial(link='logit'),data = data.tr)
  pred.prob <- predict(fit.lr, newdata=data.test, type="response")
  pred.B <- (pred.prob > 0.5)
  mcr.cv.raw[i] <- sum(pred.B != data.test$IsB)/length(pred.B)
}
lg.mcr.cv <- mean(mcr.cv.raw)
lg.mcr.cv

#g
PCA <- prcomp(t(exprs(ALL)), scale = TRUE)
PropVar <- summary(PCA)$importance[2,]
plot(1:length(PropVar), PropVar, xlab='number of principal components', ylab='proportion of variance explained')  
K <- 1:20
plot(K, PropVar[1:20])

#h
library(e1071)
PCA.five <- PCA$x[,1:5]
fit.svm <- svm(PCA.five, IsB, type = 'C-classification', kernel = 'linear')
svm.pred <- predict(fit.svm, PCA.five)
table(svm.pred, IsB)
sum(svm.pred == 'TRUE' & IsB == 'TRUE')/sum(IsB == 'TRUE')

#i
n <- dim(PCA.five)[1]
mcr.cr.raw <- rep(NA,n)
for (i in 1:n) {
  svm.cv <- svm(PCA.five[-i,], IsB[-i], type = 'C-classification', kernel = 'linear')
  svm.pred <- predict(svm.cv, t(PCA.five[i,]))
  mcr.cv.raw[i] <- mean(svm.pred!=IsB[i])
}
svm.mcr.cv <- mean(mcr.cv.raw)
svm.mcr.cv

#2
library(VGAM)

data(iris)

# 3 models
classification <- function(iris2) {
  iris2.lr <- vglm(Species~. , family = multinomial, data = iris2)
  pred.prob <- predict(iris2.lr, as.data.frame(iris2[,-1]), type = "response")
  pred.lr <- apply(pred.prob, 1, which.max)
  pred.lr <- factor(pred.lr, levels=c("1", "2", "3"), labels = levels(iris2$Species))
  
  mcr.lr <- mean(pred.lr!=iris2$Species)
  
  
  mcr.cv.raw<-rep(NA, n) 
  for (i in 1:n) {
    lgr.fit <- vglm(Species~., family=multinomial, data=iris2[-i,])
    pred.prob <- predict(lgr.fit, iris2[i,], type="response")
    pred <- apply(pred.prob, 1, which.max) 
    pred <- factor(pred, levels=c("1","2","3"), labels=levels(iris2$Species)) 
    mcr.cv.raw[i]<- mean(pred!=Species[i]) 
  }
  mcr.lr.cv<-mean(mcr.cv.raw)
  result1 <- data.frame(mcr=mcr.lr,mcr.cv=mcr.lr.cv)
  
  
  iris2.svm <- svm(data.pca, Species, type = "C-classification", kernel = "linear") 
  svmpred <- predict(iris2.svm, data.pca)  
  mcr.svm<- mean(svmpred!=Species) 
  
  mcr.cv.raw<-rep(NA, n) 
  for (i in 1:n) { 
    svm.fit <- svm(data.pca, Species, type = "C-classification", kernel = "linear")
    pred.svm <- predict(svm.fit, t(data.pca[i,])) 
    mcr.cv.raw[i]<- mean(pred.svm!=iris2$Species[i]) 
  }
  mcr.svm.cv<-mean(mcr.cv.raw)
  
  result2 <- data.frame(mcr=mcr.svm,mcr.cv=mcr.svm.cv)
  result2 <- rbind(result1,result2)
  
  
  fit <- rpart(Species ~ ., data = iris2, method = "class")
  pred.tr<-predict(fit, iris2, type = "class")
  mcr.tr <- mean(pred.tr!=Species)
  
  mcr.cv.raw<-rep(NA, n)
  for (i in 1:n) { 
    fit.tr <- rpart(Species ~ ., data = iris2[-i,], method = "class")
    pred <- predict(fit.tr, iris2[i,], type = "class")
    mcr.cv.raw[i]<- mean(pred!=Species[i])
  }
  mcr.tr.cv<-mean(mcr.cv.raw)
  result3 <- data.frame(mcr=mcr.tr,mcr.cv=mcr.tr.cv)
  result3 <- rbind(result2,result3)
  return(result3)
}

pca.iris<-prcomp(iris[,1:4], scale=TRUE)
Species <- iris$Species
n <- length(Species)
results <- list()
for (k in 1:4){
  data.pca <- as.matrix(pca.iris$x[, 1:k])
  iris2 <- data.frame(Species, data.pca)
  results[[k]] <- classification(iris2)
}

big_data = do.call(cbind,results)
colnames(big_data) <- c('k1.mcr','k1.mcr.cv','k2.mcr','k2.mcr.cv','k3.mcr','k3.mcr.cv','k4.mcr','k4.mcr.cv')
rownames(big_data) <- c('lr', 'svm', 'tr')
big_data

