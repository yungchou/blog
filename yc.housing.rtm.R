log <- function(x){
  write(x,'blog.post/yc.housing.rtm.post.txt',append=TRUE, sep=' ')}

log(format(Sys.time(), "%a %b %d %Y %X"))

log('start.from.sratch = ')
log((start.from.scratch=FALSE))

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

# The following features have been consistently denoted by p-values as
# 'not significant' in a series of test runs in various configurations.
insignificant.features <- c(
  'HeatingQC'
  ,'MSSubClass','CentralAir','FullBath'
  ,'Exterior1st','Exterior2nd','HalfBath','X1stFlrSF','TotRmsAbvGrd'
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
train <- train[-c(186,250,305,314,336,524,636,692
                  ,198,584
                  ,497,633,689,889,1047
  ,707,804,899,1025,1170,1174,1183,1299,1325),]

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

plotit <- FALSE
if(plotit){

# Distribution plot of SalePrice
par(mfrow=c(1,1),mar=c(2,5,1,1)+0.1,mgp=c(3.5,1,0)
);hist(train$SalePrice,las=1,col='lightblue',freq=FALSE
     ,ylim=c(0,max(density(train$SalePrice)$y))
     ,main='SalePrice Distribution',xlab='log(SalePrice)'
);lines(density(train$SalePrice),col='red',lwd=2
);par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))

summary(train$SalePrice)

str(train)
# Original factor variables which were converted into integer
char <- c('BldgType','OverallQual','OverallCond','BsmtExposure'
            ,'BedroomAbvGr','TotRmsAbvGrd','GarageCars')
numeric <- names(train[!names(train)%in%c(factor,'SalePrice')])

# Distribution plots
par(mfrow=c(ceiling(length(char)/3),3),mar=c(2,4,2,2)+0.5)
sapply(char, function(x){
  plot(train[,x],log(train$SalePrice),las=1,main=x,ylab='log(SalePrice)')
  })
sapply(numeric, function(x){
  plot(train[,x],log(train$SalePrice),las=1,main=x,ylab='log(SalePrice)')
  });par(mfrow=c(1,1),mar=c(1,1,1,1))

# Panel plot
if (!require('psych')) install.packages('psych'); library(psych)
pairs.panels(scale(train[c(numeric,'SalePrice')]),las=1,cex.cor=3,lm=TRUE,stars=TRUE)

str(train)
#par(mfrow=c(1,1),mar=c(1,1,1,1))
pairs.panels(scale(train),las=1,cex.cor=10,lm=TRUE,stars=TRUE)

# Correlation plot
if (!require('corrplot')) install.packages('corrplot'); library(corrplot)
corrplot.mixed(cor(train),outline=FALSE,tl.cex=0.8,mar=c(1,1,1,1),
  title='Correlation Matrix')

# ggplot correlation matrix
if (!require('ggcorrplot')) install.packages('ggcorrplot'); library(ggcorrplot)
ggcorrplot(round(cor(train),1),type="lower",lab=TRUE,title='Train Dataset Correlation Matrix'
#            , outline.col = "white", method = "circle"
            )

#sapply(train, typeof)
#names(train)

  # Each Variables vs. SalePrice
  par(mfrow=c(ceiling(length(train)/4),4),mar=c(2,5,2,1)+0.1, mgp=c(3.7,1,0)
  );for (i in names(train[-17])){
    plot(train[,i],train[,17],main=i,las=1,ylab='SalePrice')
  };par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))

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
#names(train.set)

lm <- train(SalePrice~.
            +(OverallQual):(TotalBsmtSF+X2ndFlrSF#+YearBuilt
            ),train.set
            ,method='lm'
            ,preProcess = c("center", "scale")
            ,trControl=params)

summary(lm);#lm$results
saveRDS(lm, 'results/model.1.lm.rds')

#------------------
# Diagnostic plots
#------------------
par(mfrow=c(2,2),mar=c(3,5.3,3,3),mgp=c(4,1,0)
    );plot(lm$finalModel,las=1
    );par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))

plot(varImp(lm),main='Linear Regression Feature Importance')

#------------------------------------------
# Histogram and Density Plots of Residuals
#------------------------------------------
par(mar=c(5.5,5.5,4,2)+0.1,mgp=c(4,1,0)
    );hist(residuals(lm),las=1, freq=FALSE, col='lightblue'
           ,ylim=c(0,max(density(residuals(lm))$y))
    );lines(density(residuals(lm)),col='red',lwd=2
    );par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))

#-------------------
# Shapiro-Wilk Test
#-------------------
# The null-hypothesis of this test is that the population is normally
# distributed.
# If the p-value is less than the chosen alpha level, then the null
# hypothesis is rejected and there is evidence that the data tested
# are not normally distributed.
# If the p-value is greater than the chosen alpha level, then the null
# hypothesis that the data came from a normally distributed population
# can not be rejected.
# For example, considering an alpha level of 0.05, a data set with
# a p-value of 0.05 rejects the null hypothesis that the data are
# from a normally distributed population.
shapiro.test(residuals(lm))

#------------------
# Jarque–Bera Test
#------------------
# jarqueberaTest is a goodness-of-fit test of whether sample data
# have the skewness and kurtosis matching a normal distribution.
# The null hypothesis is a joint hypothesis that the skewness = 0 and
# the excess kurtosis = 0 or 3, same as those of a normal distribution.
if (!require('fBasics')) install.packages('fBasics'); library(fBascs)
jarqueberaTest(residuals(lm))

#--------------------
# Durbin-Watson Test
#--------------------
# The Durbin-Watson statistic ranges in value from 0 to 4.
# - A value near 2 indicates non-autocorrelation.
# - A value toward 0 indicates positive autocorrelation.
# - A value toward 4 indicates negative autocorrelation.
# A rule of thumb is that test statistic values in the range of
# 1.5 to 2.5 are relatively normal.
# Values outside of this range could be cause for concern.
if (!require('lmtest')) install.packages('lmtest'); library(lmtest)
dwtest(lm$finalModel)

#------------
# Prediction
#------------
#names(test.set)
pred.lm <-  predict(lm, test.set[-17])
rmse.lm <- sqrt(mean( (pred.lm-train.set$SalePrice)^2 ) )

if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)

Linear <- data.frame(predicted=pred.lm, observed=test.set[,'SalePrice']) %>%
  ggplot(aes(x=observed, y=predicted)) +
  geom_point(color='black', pch=21, size=4, fill='red', alpha=0.3) +
  geom_abline(slope=1, intercept=0, size=1) +
  labs(title="Linear Regression", x='Observed', y='Predicted');Linear

#---------------------
# 2. Ridge Regression
#---------------------
grid = seq(5650, 5750, length=20)

set.seed(2-2)
ridge <-train(SalePrice~.
              +OverallQual:(TotalBsmtSF+X2ndFlrSF#+YearBuilt
              ),train.set
              ,method='glmnet',trControl=params
              ,preProcess = c("center", "scale")
              ,tuneGrid=expand.grid(alpha=0, lambda=grid))

saveRDS(ridge, 'results/model.2.ridge.rds')
ridge$bestTune

plot(ridge, main='Ridge Regression')

par(mfrow=c(1,2),mar=c(5,5,2,1),mgp=c(3.7,1,0)
);plot(ridge$finalModel,las=1,lwd=2,
           xvar='lambda',xlab='Ridge Regression Log Lambda'
);plot(ridge$finalModel,las=1,lwd=2,
     xvar='dev',xlab='Ridge Regression Fraction Deviance Explained'
);par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))

plot(varImp(ridge),main='Ridge Regression Feature Importance')

#------------------------------------------
# Histogram and Density Plots of Residuals
#------------------------------------------
par(mar=c(5.5,5.5,4,2)+0.1,mgp=c(4,1,0)
);hist(residuals(ridge),las=1, freq=FALSE, col='lightblue'
       ,ylim=c(0,max(density(residuals(ridge))$y))
);lines(density(residuals(ridge)),col='red',lwd=2
);par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))

#------------
# Prediction
#------------
pred.ridge <-  predict(ridge, test.set[-17])
rmse.ridge <- sqrt(mean( (pred.ridge-train.set$SalePrice)^2 ) )

#if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
#if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)

L2Norm <- data.frame(predicted=pred.ridge, observed=test.set[,'SalePrice']) %>%
  ggplot(aes(x=observed, y=predicted)) +
  geom_point(color='black', pch=21, size=4, fill='red', alpha=0.3) +
  geom_abline(slope=1, intercept=0, size=1) +
  labs(title="Ridge Regression", x='Observed', y='Predicted');L2Norm

#---------------------
# 3. Lasso Regression
#---------------------
grid = seq(25,35,length=10)

set.seed(3-3)
lasso <- train(SalePrice~.
               +OverallQual:(TotalBsmtSF+X2ndFlrSF#+YearBuilt
               ),train.set
               ,method='glmnet',trControl=params
               ,preProcess = c("center", "scale")
               ,tuneGrid=expand.grid(alpha=1, lambda=grid))

saveRDS(lasso, 'results/model.3.ridge.rds')
lasso$bestTune

plot(lasso, main='Lasso Regression')

par(mfrow=c(1,2),mar=c(5,5,2,1),mgp=c(3.7,1,0)
);plot(lasso$finalModel,las=1,lwd=2,
     xvar='lambda',xlab='Lasso Regression Log Lambda'
);plot(lasso$finalModel,las=1,lwd=2,
     xvar='dev',xlab='Lasso Regression Fraction Deviance Explained'
);par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))

plot(varImp(lasso),main='Lasso Regression Feature Importance')

#------------------------------------------
# Histogram and Density Plots of Residuals
#------------------------------------------
par(mar=c(5.5,5.5,4,2)+0.1,mgp=c(4,1,0)
);hist(residuals(lasso),las=1, freq=FALSE, col='lightblue'
       ,ylim=c(0,max(density(residuals(lasso))$y))
);lines(density(residuals(lasso)),col='red',lwd=2
);par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))

#------------
# Prediction
#------------
pred.lasso <- predict(lasso, test.set[-17])
rmse.lasso <- sqrt(mean( (pred.lasso-train.set$SalePrice)^2 ) )

#if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
#if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)

L1Norm <- data.frame(predicted=pred.lasso, observed=test.set[,'SalePrice']) %>%
  ggplot(aes(x=observed, y=predicted)) +
  geom_point(color='black', pch=21, size=4, fill='red', alpha=0.3) +
  geom_abline(slope=1, intercept=0, size=1) +
  labs(title="Lasso Regression",
       x='Observed', y='Predicted'); L1Norm

#----------------
# 4. Elastic Net
#----------------
set.seed(4-4)
elastic <- train(SalePrice~.
                 +OverallQual:(TotalBsmtSF+X2ndFlrSF#+YearBuilt
                 ),train.set
                 ,method='glmnet', trControl=params
                 ,preProcess = c("center", "scale")
                 ,tuneGrid=expand.grid(
                   alpha =seq(0.1,0.5,length=20)
                  ,lambda=seq(45,60,length=20)
                   ))

saveRDS(elastic, 'results/model.4.elastic.rds')

elastic$bestTune

plot(elastic, main='Elastic Net')

plot(varImp(elastic),main='Elastic Net Feature Importance')

par(mfrow=c(1,2),mar=c(5,5,2,1),mgp=c(3.7,1,0)
);plot(elastic$finalModel,las=1,lwd=2,
     xvar='lambda',xlab='Elastic Net Log Lambda'
);plot(elastic$finalModel,las=1,lwd=2,
     xvar='dev',xlab='Elastic Net Fraction Deviance Explained'
);par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))

#------------------------------------------
# Histogram and Density Plots of Residuals
#------------------------------------------
par(mfrow=c(1,1),mar=c(5.5,5.5,4,2)+0.1,mgp=c(4,1,0)
);hist(residuals(elastic),las=1, freq=FALSE, col='lightblue'
       ,ylim=c(0,max(density(residuals(elastic))$y))
);lines(density(residuals(elastic)),col='red',lwd=2
);par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(3,1,0))

#------------
# Prediction
#------------
pred.elastic <- predict(elastic, test.set[-17])
rmse.elastic <- sqrt(mean( (pred.elastic-train.set$SalePrice)^2 ) )

#if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
#if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)

Elastic <- data.frame(predicted=pred.elastic, observed=test.set[,'SalePrice']) %>%
  ggplot(aes(x=observed, y=predicted)) +
  geom_point(color='black', pch=21, size=4, fill='red', alpha=0.3) +
  geom_abline(slope=1, intercept=0, size=1) +
  labs(title="Elastic Net", x='Observed', y='Predicted');Elastic

#---------------------
# 5. Model Comparison
#---------------------
if (!require('caret')) install.packages('caret'); library(caret)

model.list <- list(Linear=lm, Ridege=ridge, Lasso=lasso, Elastic=elastic)
resamp <- resamples(model.list)
saveRDS(summary(resamp), 'results/model.comparison.rds');summary(resamp)

jpeg(filename="blog.post/model.comparison.RMSE.jpeg")
bwplot(resamp,metric="RMSE",main='Model Comparison')
dev.off()

jpeg(filename="blog.post/model.comparison.Rsquared.jpeg")
bwplot(resamp,metric="Rsquared",main='Model Comparision')
dev.off()

jpeg(filename="blog.post/model.comparison.MAE.jpeg")
bwplot(resamp,metric="MAE",main='Model Comparision')
dev.off()

#str(resamp$values)
if (!require('plotly')) install.packages('plotly'); library(plotly)

p3d.rmse <- plot_ly(resamp$values,mode="markers"
#               ,colors=c("green","blue")
               ,y =resamp$values$'Ridege~RMSE'
               ,x =resamp$values$'Lasso~RMSE'
               ,z =resamp$values$'Linear~RMSE'
               ,marker =list(line =list(color='black',width = 1)
                             ,opacity=0.5, size=10
                             ,color='red')
) %>% add_markers() %>%
  layout(title = 'Model Comparison (RMSE)'
         ,scene = list(
           yaxis = list(title = 'Ridege~RMSE')
          ,xaxis = list(title = 'Lasso~RMSE')
          ,zaxis = list(title = 'Linear~RMSE'))
);saveRDS(p3d.rmse,'14032/5.p3d.rmse.rds'
);htmlwidgets::saveWidget(as_widget(p3d.rmse), "p3d.rmse.html");p3d.rmse

p3d.rsquared <- plot_ly(resamp$values,mode="markers"
                    #               ,colors=c("green","blue")
                    ,y =resamp$values$'Ridege~Rsquared'
                    ,x =resamp$values$'Lasso~Rsquared'
                    ,z =resamp$values$'Linear~Rsquared'
                    ,marker =list(line =list(color='black',width = 1)
                                  ,opacity=0.5, size=10
                                  ,color='blue')
) %>% add_markers() %>%
  layout(title = 'Model Comparison (Rsquared)'
         ,scene = list(
           yaxis = list(title = 'Ridege~Rsquared')
          ,xaxis = list(title = 'Lasso~Rsquared')
          ,zaxis = list(title = 'Linear~Rsquared'))
);saveRDS(p3d.rmse,'14032/5.p3d.rsquared.rds'
);htmlwidgets::saveWidget(as_widget(p3d.rsquared), "p3d.rsquared.html");p3d.rsquared

p3d.mae <- plot_ly(resamp$values,mode="markers"
                        #               ,colors=c("green","blue")
                        ,y =resamp$values$'Ridege~MAE'
                        ,x =resamp$values$'Lasso~MAE'
                        ,z =resamp$values$'Linear~MAE'
                        ,marker =list(line =list(color='black',width = 1)
                                      ,opacity=0.5, size=10
                                      ,color='green')
) %>% add_markers() %>%
  layout(title = 'Model Comparison (MAE)'
         ,scene = list(
           yaxis = list(title = 'Ridege~MAE')
          ,xaxis = list(title = 'Lasso~MAE')
          ,zaxis = list(title = 'Linear~MAE'))
);saveRDS(p3d.mae,'14032/5.p3d.mae.rds'
);htmlwidgets::saveWidget(as_widget(p3d.mae), "p3d.mae.html");p3d.mae

# This does not produce what I want. Leave it here for reference.
p3d <- subplot(p3d.rmse,p3d.rsquared,p3d.mae
               #,nrows=3
               #,shareZ=TRUE
               )#; p3d

#---------------
# 6. PREDICTION
#---------------
predict.train <- predict(elastic, train.set)
training <- sqrt(mean(train.set$SalePrice - predict.train)^2)

predict.test <- predict(elastic, test.set)
testing  <- sqrt(mean(test.set$SalePrice - predict.test)^2)

cat('mse trianing = ', training, ', testing = ', testing)

#-----------
# TEST DATA
#-----------
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
write.csv(prediction.lm, 'results/prediction.house.price.1.lm.csv' )
prediction.ridge   <- predict(ridge  , test)
write.csv(prediction.ridge, 'results/prediction.house.price.2.ridge.csv' )
prediction.lasso   <- predict(lasso  , test)
write.csv(prediction.lasso, 'results/prediction.house.price.2.lasso.csv' )
prediction.elastic <- predict(elastic, test)
write.csv(prediction.elastic, 'results/prediction.house.price.4.elastic.csv' )

