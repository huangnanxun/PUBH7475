#package list
library("lars")
library("MASS")
library("glmnet")
library("caret")
set.seed(7475)
raw_train <- read.csv("D://Study/master/PUBH7475/proj/block_train.csv",head = T)
raw_test <- read.csv("D://Study/master/PUBH7475/proj/block_test.csv",head = T)
data_train <- raw_train[,-c(1,2,21,29)]
data_test <- raw_test[,-c(1,2,21,29)]

data_train$PedCount <- round(data_train$PedCount)
data_train$IfSideWalk <- as.factor(data_train$IfSideWalk)
data_train$IfBikeLane <- as.factor(data_train$IfBikeLane)
data_train$IfDowntown <- as.factor(data_train$IfDowntown)
data_train$IfMain <- as.factor(data_train$IfMain)
data_train$IfSecondary <- as.factor(data_train$IfSecondary)

fit_gaussian_all<-glm(log(PedCount)~IfSideWalk+IfBikeLane+PopDen+JobDen+IntNum+TraStpNum+AADT+PerCommercial+PerIndustrial+
      PerOpenspace+Entropy+IfDowntown+PerChild+PerOld+PerMale+PerWhite+PerHisp+PerPoverty+AvgHHSize+AvgVeh+
        IfMain+IfSecondary+StrtLitNum, family = gaussian, data=data_train)
summary(fit_gaussian_all)

fit_gaussian_selected<-glm(log(PedCount)~AADT+PerCommercial+PerIndustrial+Entropy+IfDowntown+PerMale+PerWhite+AvgHHSize+AvgVeh+IfMain+IfSecondary, family = gaussian, data=data_train)
summary(fit_gaussian_selected)

fit_Gamma_all<-glm(log(PedCount)~IfSideWalk+IfBikeLane+PopDen+JobDen+IntNum+TraStpNum+AADT+PerCommercial+PerIndustrial+
                     PerOpenspace+Entropy+IfDowntown+PerChild+PerOld+PerMale+PerWhite+PerHisp+PerPoverty+AvgHHSize+AvgVeh+
                     IfMain+IfSecondary+StrtLitNum, family = Gamma,data=data_train)
summary(fit_Gamma_all)

fit_Gamma_selected<-glm(log(PedCount)~PerCommercial+PerIndustrial+Entropy+IfDowntown+PerWhite+AvgHHSize+AvgVeh+IfSecondary+StrtLitNum, family = Gamma,data=data_train)
summary(fit_Gamma_selected)

fit_negative_bin_all<-glm.nb(PedCount~., data=data_train)
summary(fit_negative_bin_all)
fit_negative_bin_selected<-glm.nb(PedCount~IfSideWalk+IfBikeLane+IntNum+AADT+PerCommercial+PerIndustrial+Entropy+PerMale+PerWhite+AvgHHSize+AvgVeh+IfSecondary+StrtLitNum, data=data_train)
summary(fit_negative_bin_selected)

data_test$IfSideWalk <- as.factor(data_test$IfSideWalk)
data_test$IfBikeLane <- as.factor(data_test$IfBikeLane)
data_test$IfDowntown <- as.factor(data_test$IfDowntown)
data_test$IfMain <- as.factor(data_test$IfMain)
data_test$IfSecondary <- as.factor(data_test$IfSecondary)

gaussian_predict<-as.matrix(predict(fit_gaussian_selected, data_test))
gaussian_SSE <- sum((log(data_test$PedCount) - gaussian_predict)^2)
gaussian_MSE <- gaussian_SSE/146

gaussian_all_predict<-as.matrix(predict(fit_gaussian_all, data_test))
gaussian_all_SSE <- sum((log(data_test$PedCount) - gaussian_all_predict)^2)
gaussian_all_MSE <- gaussian_all_SSE/146

Gamma_predict<-as.matrix(predict(fit_Gamma_selected, data_test))
Gamma_SSE <- sum((log(data_test$PedCount) - 1/Gamma_predict)^2)
Gamma_MSE <- Gamma_SSE/146


negative_bin_predict<-as.matrix(predict(fit_negative_bin_selected, data_test))
negative_bin_SSE <- sum((log(data_test$PedCount) - negative_bin_predict)^2)
negative_bin_MSE <- negative_bin_SSE/146

negative_bin_all_predict<-as.matrix(predict(fit_negative_bin_all, data_test))
negative_bin_all_SSE <- sum((log(data_test$PedCount) - negative_bin_all_predict)^2)
negative_bin_all_MSE <- negative_bin_all_SSE/146

#gaussian_predict_error_rate<-sum(((abs((log(data_test$PedCount)-poisson_predict)/log(data_test$PedCount))*100) <= 25) == FALSE)/length(raw_test[,1])


lasso.fit<-glmnet(as.matrix(data_train[,-1]), log(data_train$PedCount),alpha = 1)
plot(lasso.fit, xvar = "lambda", ylim=c(0,2))
abline(v=log(0.0110027),lwd=3,lty=2)
cv.lasso.fit<-cv.glmnet(x=data.matrix(data_train[,-1]), log(data_train$PedCount), nfold=10, family="gaussian")
lasso.fit1<-glmnet(data.matrix(data_train[,-1]), log(data_train$PedCount), family="gaussian", lambda=cv.lasso.fit$lambda.min)
which(cv.lasso.fit$glmnet.fit$lambda == cv.lasso.fit$lambda.min)
lasso_yhat<-predict(lasso.fit1, newx=data.matrix(data_test[,-1]))
plot(log(data_test$PedCount),lasso_yhat,xlim=c(0,10),ylim=c(0,10))
abline(a=0, b=1)
lasso_SSE <- sum((lasso_yhat-log(data_test$PedCount))^2)
lasso_MSE <- lasso_SSE/146


raw_full <- read.csv("D://Study/master/PUBH7475/proj/block_results.csv",head = T)
data_full <- raw_full[,-c(1,2,21,29)]
data_full$IfSideWalk <- as.factor(data_full$IfSideWalk)
data_full$IfBikeLane <- as.factor(data_full$IfBikeLane)
data_full$IfDowntown <- as.factor(data_full$IfDowntown)
data_full$IfMain <- as.factor(data_full$IfMain)
data_full$IfSecondary <- as.factor(data_full$IfSecondary)

data_full_numeric<-data_full[,-c(1,2,3,14,23,24,25)]
data_full_numeric.pca <- prcomp(data_full_numeric, center = TRUE,scale. = TRUE)
summary(data_full_numeric.pca)
data_full_numeric.pca


#fit.nnet <- nnet(logPedCount ~. , data = data_train_logy, size=30, decay=1000, maxit=10000)
data_test$IfSideWalk <- as.factor(data_test$IfSideWalk)
data_test$IfBikeLane <- as.factor(data_test$IfBikeLane)
data_test$IfDowntown <- as.factor(data_test$IfDowntown)
data_test$IfMain <- as.factor(data_test$IfMain)
data_test$IfSecondary <- as.factor(data_test$IfSecondary)
#predict(fit.nnet, data_test[,-1], type="raw")
data_train_logy<-data_train
data_train_logy$PedCount <- log(data_train_logy$PedCount)
colnames(data_train_logy)[1] <- "logPedCount"
my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
fit.nnet <- train(logPedCount ~ ., data = data_train_logy, method = "nnet", maxit = 1000,tuneLength = 4, trace = F, linout = 1)
fit.nnet.predict <- predict(fit.nnet, newdata = data_test[,-1])
nnet_SSE <- sum((fit.nnet.predict-log(data_test$PedCount))^2)
nnet_MSE <- nnet_SSE/146

list("method" = c("gaussian","Gamma","Poisson","Negative binomial"), "MSE" = c(gaussian_MSE,Gamma_MSE,poisson_MSE,negative_bin_MSE))

hist(data_full$PedCount,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,main="PedCount before Logarithm", xlab="PedCount value", ylab="PedCount frequency")
hist(log(data_full$PedCount),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,main="PedCount after Logarithm", xlab="PedCount value", ylab="PedCount frequency")


library(gbm)
library(randomForest)
library(caret)
block_test <- read.csv("D://Study/master/PUBH7475/proj/block_test.csv")
block_train <- read.csv("D://Study/master/PUBH7475/proj/block_train.csv")
block_test_y <- block_test[3]
test_ones <- rep(1, nrow(block_test))

# boosting
gbm_fit <- gbm(log(PedCount) ~ .-PerBlack, block_train[3:28], distribution = "gaussian", n.trees = 50000, shrinkage = 0.001, cv.folds = 5)
gbm.perf(gbm_fit, method = "cv")
gbm_yhat <- predict(gbm_fit, block_test[3:28])
gbm_err <- sum((gbm_yhat-log(block_test_y))^2)/nrow(block_test)
gbm_dataframe <- data.frame(as.data.frame(gbm_yhat),log(block_test_y))
plot(gbm_dataframe, xlim = c(0,10), ylim = c(0,10))

# random forest
rf_fit <- randomForest(log(PedCount) ~ .-PerBlack, block_train[3:28], ntree = 100, mtry = 10, nodesize = 1, importance = T)
rf_yhat <- predict(rf_fit, newdata = block_test)
rf_err <- sum((rf_yhat-log(block_test_y))^2)/nrow(block_test)
rf_dataframe <- data.frame(as.data.frame(rf_yhat),log(block_test_y))
plot(rf_dataframe, xlim = c(0,10), ylim = c(0,10))

#cluster result read
cluster_rst_table <- read.csv("D://Study/master/PUBH7475/proj/cluster_result.csv")


par(mfrow=c(2,4))
plot(gaussian_predict,log(data_test$PedCount),xlim=c(0,10),ylim=c(0,10),main="Gaussian", xlab="Predicted Y", ylab="Real Y")
abline(a=0, b=1)
plot(1/Gamma_predict,log(data_test$PedCount),xlim=c(0,10),ylim=c(0,10),main="Gamma", xlab="Predicted Y", ylab="Real Y")
abline(a=0, b=1)
plot(negative_bin_all_predict,log(data_test$PedCount),xlim=c(0,10),ylim=c(0,10),main="Negative Binomial", xlab="Predicted Y", ylab="Real Y")
abline(a=0, b=1)
plot(lasso_yhat,log(data_test$PedCount),xlim=c(0,10),ylim=c(0,10),main="Lasso", xlab="Predicted Y", ylab="Real Y")
abline(a=0, b=1)
plot(gbm_dataframe, xlim = c(0,10), ylim = c(0,10),main="GBM", xlab="Predicted Y", ylab="Real Y")
abline(a=0, b=1)
plot(rf_dataframe, xlim = c(0,10), ylim = c(0,10),main="RF", xlab="Predicted Y", ylab="Real Y")
abline(a=0, b=1)
plot(fit.nnet.predict,log(data_test$PedCount),xlim=c(0,10),ylim=c(0,10),main="NN", xlab="Predicted Y", ylab="Real Y")
abline(a=0, b=1)
plot(cluster_rst_table$yhat,cluster_rst_table$yact,xlim=c(0,10),ylim=c(0,10),main="Clustering", xlab="Predicted Y", ylab="Real Y")
abline(a=0, b=1)

dev.off()