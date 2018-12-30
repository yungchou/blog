cat('start.from.scratch = ',(start.from.scratch = FALSE))

if(start.from.scratch){

  #------------------
  # Data Preparation
  #------------------
  source('yc.housing.rtm.train.prep.R')   # Prepare train.prep
  #names(train.prep)

  #----------------------
  # Imputation of Values
  #----------------------
  if (!require('mice')) install.packages('mice'); library(mice)
  train.tmp <- mice(train.prep,  m=2, seed=111);
  train.imp <- complete(train.tmp,2)

  write.csv(train.imp, 'data/yc.train.imp.csv')

  #-------------------------------
  # Feature Selection with Boruta
  #-------------------------------
  source('yc.boruta.R')

}

#----------------------
# IDENTIFYING FEATURES
#----------------------
# Features selected by Boruta
train.boruta.selected.features <- readRDS('boruta/train.boruta.selected.features.rds')

# The following features have been repeatedly identified as 'not significant'
# during test runs.
insignificant.features <- c(
  'MSSubClass','CentralAir','FullBath'
  ,'Exterior1st','Exterior2nd','HalfBath'
  ,'HouseStyle','MasVnrType','BsmtQual','BsmtFinType1','BsmtUnfSF'
  ,'Fireplaces','GarageType','GarageYrBlt','GarageFinish','GarageArea'
  ,'GarageQual','GarageCond','PavedDrive','WoodDeckSF','OpenPorchSF'
)

# Read in the prepared data
train.imp <- read.csv('data/yc.train.imp.csv')

#----------------------------
# REMOVING UNWANTED FEATURES
#----------------------------
#names(train.imp)
train <- train.imp[c(train.boruta.selected.features,'SalePrice')]
train <- train[!names(train)%in%insignificant.features]
#names(train)

#-------------------------------------
# REMOVING NEAR ZERO-VARANCE FEATURES
#-------------------------------------
if (!require('caret')) install.packages('caret'); library(caret)

nzv <- names(train[ nearZeroVar(train,saveMetrics=FALSE) ])
cat('caret reports ',length(nzv),'near zero-variables as the following:\n\n');nzv

if(length(nzv)>0){train <- train[, !names(train)%in%nzv]}
cat('The listed, ',length(nzv),' near-zero variables have been removed.\n\n', 'At this time, the data set has ',nrow(train),' observations with ', ncol(train),' variables remaining.')

#names(train)

#-------------------
# REMOVING OUTLIERS
#-------------------
# The following observations have been identified during test runs as outliers
# with latge Cook's distance and noticeable influence. They are removed here.
train <- train[-c(186,250,305,314,336,524,636,692,707,1025,1170,1174,1183,1299),]

cat('At this time, the data set has ',nrow(train),' observations with ', ncol(train),' variables remaining.')

#train <- train[,-1] # Leave out the Id field

removeThese <- FALSE  # It turned out removing these, reduce R^2
if(removeThese){
  #summary(train$LotArea)
  #boxplot(train$LotArea, main='LotArea')
  #sum(train$LotArea>40000)
  train <- train[train$LotArea<40000,]  # remove this actually reduced R^2

  #summary(train$BsmtFinSF1)
  #boxplot(train$BsmtFinSF1,main='BsmtFinSF1')
  #sum(train$BsmtFinSF1>1600)
  train <- train[train$BsmtFinSF1<1600,] # remove this actually reduced R^2

  #summary(train$GarageCars)
  #boxplot(train$GarageCars,main='GarageCars')
  #sum(train$GarageCars==4)
  train <- train[train$GarageCars<4,]   # remove this actually reduced R^2
}

#--------------------
# DATA VISUALIZATION
#--------------------
#names(train)

# Distribution plot of SalePrice
par(mfrow=c(1,1),mar=c(4,4,1,1),mgp=c(3,1,0))
hist(log(train$SalePrice),las=1,col='lightblue',freq=FALSE,
     main='SalePrice Distribution',xlab='log(SalePrice)')
lines(density(log(train$SalePrice)),col='red',lwd=2)
par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))

summary(train$SalePrice)

# Original factor variables which were converted into integer
factor <- c('BldgType','OverallQual','OverallCond','BsmtExposure'
            ,'BedroomAbvGr','TotRmsAbvGrd','GarageCars')
numeric <- names(train[!names(train)%in%c(factor,'SalePrice')])

# Distribution plots
par(mfrow=c(ceiling(length(factor)/3),3),mar=c(2,5,5,2))
sapply(factor, function(x){plot(
  train[,x],log(train$SalePrice),las=1,main=x,ylab='log(SalePrice)'
  )})
sapply(numeric, function(x){plot(
  train[,x],log(train$SalePrice),las=1,main=x,ylab='log(SalePrice)'
)});par(mfrow=c(1,1),mar=c(1,1,1,1))

# Panel plot
if (!require('psych')) install.packages('psych'); library(psych)
pairs.panels(scale(train[c(numeric,'SalePrice')]),las=1,cex.cor=3,lm=TRUE,stars=TRUE)

# Correlation plot
if (!require('corrplot')) install.packages('corrplot'); library(corrplot)
corrplot.mixed(cor(train),outline=FALSE,tl.cex=0.8,mar=c(1,1,1,1),
  title='Correlation Matrix')

# ggplot correlation matrix
if (!require('ggcorrplot')) install.packages('ggcorrplot'); library(ggcorrplot)
ggcorrplot(round(cor(train),1),type="lower",lab=TRUE,title='Correlation Matrix'
#            , outline.col = "white", method = "circle"
            )

#sapply(train, typeof)
#names(train)

plotit <- FALSE
if(plotit){
  # Each Variables vs. SalePrice
  par(mfrow=c(ceiling(length(train)/3),3),mar=c(2,4,2,2))
  for (i in names(train[-16])){
    plot(train[,i],log(train[,16]),main=i,las=1,ylab='log(SalePrice')
  };par(mfrow=c(1,3),,mar=c(1,1,1,1))
}

#names(train)

#########################################################################
# Skewness is a measure of a dataset’s symmetry. ref: [-1,-0.5,0,0.5,1]
# Kurtosis is the degree of peakedness of a distribution.
# The skewness and kurtosis statistics appear to be very dependent on the sample size.
# Ref: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4321753/
# by Peter Westfall
# In short, skewness and kurtosis are practically worthless.
# Shewhart made this observation in his first book.
# The statistics for skewness and kurtosis simply do not provide
# any useful information beyond that already given by the measures
# of location and dispersion."

#if (!require('moments')) install.packages('moments'); library(moments)
#skewness(train)
#kurtosis(train)

#########################################################################

#-------------------
# PARTITIONING DATA
#-------------------
set.seed(0-0)
part <- sample(2,nrow(train),replace=TRUE,prob=c(0.7,0.3))
train.set <- train[part==1,]
test.set  <- train[part==2,]

# Cross-Validation Settings
library(caret)
params <- trainControl(method='repeatedcv',
                       number=10, repeats=5, verboseIter=TRUE)

#----------------------
# 1. LINEAR REGRESSION
#----------------------
if (!require('glmnet')) install.packages('glmnet'); library(glmnet)

set.seed(1-1)

lm <- train(SalePrice~.*(GrLivArea + OverallQual + TotalBsmtSF
                         + X1stFlrSF
                         + YearBuilt )
            ,train.set,
            method='lm',
            preProcess = c("center", "scale"),
            trControl=params)

par(mfrow=c(2,2),mar=c(3,5.3,3,3),mgp=c(4,1,0))
plot(lm$finalModel,las=1); par(mfrow=c(1,1),mar=c(1,1,1,1))


lm.results <- lm$results
summary(lm)
saveRDS(lm, 'results/model.1.lm.rds')

pred.lm <-  predict(lm, test.set[-16])

rmse.lm <- sqrt(mean( (pred.lm-train.set$SalePrice)^2 ) )

if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)

Linear <- data.frame(predicted = pred.lm,
                     observed = test.set[,16]) %>%
  ggplot(aes(x=observed, y=predicted)) +
  geom_point(color='black', pch=21, size=4, fill='red', alpha=0.3) +
  geom_abline(slope=1, intercept=0) +
  labs(title = "Linear Regression",
       x = 'observed value', y = 'predicted value')

#---------------------
# 2. Ridge Regression
#---------------------
grid = seq(0.01, 10, length = 10)

set.seed(2-2)
ridge <-train(SalePrice~.*(GrLivArea + OverallQual + TotalBsmtSF
                           + X1stFlrSF
                           + YearBuilt )
              , train.set,
               method='glmnet', trControl=params,
               preProcess = c("center", "scale"),
               tuneGrid=expand.grid(alpha=0, lambda=grid))

saveRDS(ridge, 'results/model.2.ridge.rds')
ridge$bestTune

par(mfrow=c(1,1),mar=c(5,5,2,1),mgp=c(3.7,1,0))
plot(ridge$finalModel,las=1,lwd=2,
     xvar='lambda',xlab='Ridge Regression Log Lambda')
plot(ridge$finalModel,las=1,lwd=2,
     xvar='dev',xlab='Ridge Regression Fraction Deviance Explained')
plot(varImp(ridge),main='Ridge Regression Feature Importance')
par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))

pred.ridge <-  predict(ridge, test.set[-16])
rmse.ridge <- sqrt(mean( (pred.ridge-train.set$SalePrice)^2 ) )

#if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
#if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)

L2Norm <- data.frame(predicted = pred.ridge,
                     observed = test.set[,16]) %>%
  ggplot(aes(x=observed, y=predicted)) +
  geom_point(color='black', pch=21, size=4, fill='red', alpha=0.3) +
  geom_abline(slope=1, intercept=0) +
  labs(title = "Ridge Regression",
       x = 'observed value', y = 'predicted value')

#---------------------
# 3. Lasso Regression
#---------------------
grid = seq(0.01, 100, length = 10)

set.seed(3-3)
lasso <- train(SalePrice~.*(GrLivArea + OverallQual + TotalBsmtSF
                            + X1stFlrSF
                            + YearBuilt )
               , train.set,
               method='glmnet', trControl=params,
               preProcess = c("center", "scale"),
               tuneGrid=expand.grid(alpha=1, lambda=grid))

saveRDS(lasso, 'results/model.3.ridge.rds')
lasso$bestTune

par(mfrow=c(1,1),mar=c(5,5,2,1),mgp=c(3.7,1,0))
plot(lasso$finalModel,las=1,lwd=2,
     xvar='lambda',xlab='Lasso Regression Log Lambda')
plot(lasso$finalModel,las=1,lwd=2,
     xvar='dev',xlab='Lasso Regression Fraction Deviance Explained')
plot(varImp(lasso),main='Lasso Regression Feature Importance')
par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))

pred.lasso <-  predict(lasso, test.set[-16])
rmse.lasso <- sqrt(mean( (pred.lasso-train.set$SalePrice)^2 ) )

#if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
#if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)

L1Norm <- data.frame(predicted = pred.lasso,
                     observed = test.set[,16]) %>%
  ggplot(aes(x=observed, y=predicted)) +
  geom_point(color='black', pch=21, size=4, fill='red', alpha=0.3) +
  geom_abline(slope=1, intercept=0) +
  labs(title = "Lasso Regression",
       x = 'observed value', y = 'predicted value')


#----------------
# 4. Elastic Net
#----------------
grid = seq(0.01, 100, length = 10)

set.seed(4-4)
elastic <- train(SalePrice~.*(GrLivArea + OverallQual + TotalBsmtSF
                              + X1stFlrSF
                              + YearBuilt )
                 , train.set, method='glmnet', trControl=params,
                 preProcess = c("center", "scale"),
                 tuneGrid=expand.grid(alpha=grid,
                                      lambda=grid))
saveRDS(elastic, 'results/model.4.elastic.rds')

elastic$bestTune

plot(elastic)
par(mfrow=c(1,1),mar=c(5,5,2,1),mgp=c(3.7,1,0))
plot(elastic$finalModel,las=1,lwd=2,
     xvar='lambda',xlab='Elastic Net Log Lambda')
plot(elastic$finalModel,las=1,lwd=2,
     xvar='dev',xlab='Elastic Net Fraction Deviance Explained')
plot(varImp(elastic),main='Elastic Net Feature Importance')
par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))


pred.elastic <-  predict(elastic, test.set[-16])
rmse.elastic <- sqrt(mean( (pred.elastic-train.set$SalePrice)^2 ) )

#if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
#if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)

Elastic <- data.frame(predicted = pred.elastic,
                      observed = test.set[,16]) %>%
  ggplot(aes(x=observed, y=predicted)) +
  geom_point(color='black', pch=21, size=4, fill='red', alpha=0.3) +
  geom_abline(slope=1, intercept=0) +
  labs(title = "Elastic Net",
       x = 'observed value', y = 'predicted value')

#----------------
# 4.1 Comparison
#----------------
if (!require('caret')) install.packages('caret'); library(caret)

model.list <- list(Linear=lm, Ridege=ridge, Lasso=lasso, Elastic=elastic)
comparison <- resamples(model.list)
saveRDS(summary(comparison), 'results/model.comparison.rds')
bwplot(comparison, main='Model Comparision')
#xyplot(comparison, metric='RMSE')
# do a 3d plot here

#----------------
# 4.2 Best Model
#----------------
bestOne <- elastic$bestTune

#coef(bestOne, s=elastic$bestTune$lambda)

saveRDS(elastic, 'model.4.elastic.bestOne.rds')
#elastic <- readRDS('elastic.bestOne.rds')

#---------------
# 5. PREDICTION
#---------------
predict.train <- predict(elastic, train.set)
training <- sqrt(mean(train.set$SalePrice - predict.train)^2)

predict.test <- predict(elastic, test.set)
testing  <- sqrt(mean(test.set$SalePrice - predict.test)^2)

cat('mse trianing = ', training, ', testing = ', testing)
#plot(predict.train, las=1, cex.axis=0.7, main='Prediction by Training Data', xlab='Training Data')
#plot(predict.test,  las=1, cex.axis=0.7, main='Prediction by Testing Data' , xlab='Testing Data' )

#------------
if (!require('mice')) install.packages('mice'); library(mice)

source('yc.housing.rtm.test.prep.R')
test.tmp <- mice(test.prep, m=2, seed=111);
test.tmp <- complete(test.tmp,2);
#names(test.tmp)

test <- test.tmp[c(train.boruta.selected.features)]
test <- test[!names(test)%in%insignificant.features]

#names(test)
#test.id <- test[1]
#test <- test[-1]

prediction.lm      <- predict(lm     , test)
write.csv(prediction.lm, 'results/prediction.house.price.lm.csv' )
prediction.ridge   <- predict(ridge  , test)
write.csv(prediction.ridge, 'results/prediction.house.price.ridge.csv' )
prediction.lasso   <- predict(lasso  , test)
write.csv(prediction.lasso, 'results/prediction.house.price.lasso.csv' )
prediction.elastic <- predict(elastic, test)
write.csv(prediction.elastic, 'results/prediction.house.price.elastic.csv' )

#plot(prediction.lm, las=1, cex.axis=0.7, main='Prediction by lm' , xlab='Test Dataset' )
#plot(prediction.ridge, las=1, cex.axis=0.7, main='Prediction by ridge' , xlab='Test Dataset' )
#plot(prediction.lasso, las=1, cex.axis=0.7, main='Prediction by lasso' , xlab='Test Dataset' )
#plot(prediction.elastic, las=1, cex.axis=0.7, main='Prediction by elastic' , xlab='Test Dataset' )
