#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

attach(ToyotaCorolla)

#to check if there is any  NA in data
na.omit(ToyotaCorolla)

summary(ToyotaCorolla)


#data pre processing
data1<-ToyotaCorolla[ ,-19:-38]
#View(data1)

#data pre processing
data2<-data1[ ,-1:-2]
View(data2)

#data pre processing
data3<-data2[ ,-3:-4]
View(data3)

#data pre processing
data4<-data3[ ,-4]
View(data4)


#data pre processing
data5<-data4[ ,-5:-6:-7]
View(data5)

#data pre processing
data6<-data5[ ,-7]
View(data6)

attach(data6)

#plot

plot(data6)

#scatterplot

scatterplot3d::scatterplot3d(data6)

#coorelations 
cor(data6)

#
library(corpcor)

cor2pcor(cor(data6))

#simple linear regression model
modeltyo<-lm(Price~.,data = data6)


#summary of model
summary(modeltyo)

#Multiple R-squared:  0.8638
#Adjusted R-squared:  0.863


#Age_08_04     -1.217e+02  2.616e+00 -46.512  < 2e-16 ***
#KM            -2.082e-02  1.252e-03 -16.622  < 2e-16 ***
#HP             3.168e+01  2.818e+00  11.241  < 2e-16 ***
#cc            -1.211e-01  9.009e-02  -1.344  0.17909    
#Doors         -1.617e+00  4.001e+01  -0.040  0.96777    
#Gears          5.943e+02  1.971e+02   3.016  0.00261 ** 
#Quarterly_Tax  3.949e+00  1.310e+00   3.015  0.00262 ** 
#Weight         1.696e+01  1.068e+00  15.880  < 2e-16 ***

#as we can see that cc and doors are irrelavant in this contest 

data7<-data6[,-5:-6]

View(data7)

#modelwith out cc and doors

#modeltyo2<-lm(Price~.,data = data7)
modeltyo2<-train(Price~.,data7,method='lm',trcontrol=custom)

summary(modeltyo2)

predict(modeltyo2$finalModel)

#Multiple R-squared:  0.8636
#Adjusted R-squared:  0.863 

# to check the best fit varible we use step AIC function 

library(MASS)

stepAIC(modeltyo2)

#with a Step AIC of 20691.7......which is the lowest of all 
# we conclude as varibles in modeltyo2 as best fit 

#plot

plot(modeltyo2)

plot(modeltyo2$finalModel)


#indentifiying influntial varibles
influence.measures(modeltyo)
influenceIndexPlot(modeltyo)
influencePlot(modeltyo)

#delete influential data
modeltyo3<-lm(Price~.,data = data6[-81,])

summary(modeltyo3)

avPlots(modeltyo3)


####applying ridge and lasso regression model 


# initilaizing training and testing data set

ind<-sample(2,nrow(data7),replace = T,prob = c(0.7,0.3))

train<-data7[ind==1,]

test<-data7[ind==2,]

library(caret)

custom<-trainControl(method = "repeatedcv",number = 10,repeats = 5,verboseIter = T)

#ridge model
#as we are increasing the lambda we are decreasing the coefficients
set.seed(1234)
ridgemodel<-train(Price~.,data7,method='glmnet',tuneGrid=expand.grid(alpha=0,lambda=seq(0.0001,1,length=5)),trcontrol=custom)

ridgemodel

#The final values used for the model were alpha = 0 and lambda = 1.

#lambda    RMSE      Rsquared   MAE
# 1.000000  1372.4  0.8620133  1011.378


#plot

plot(ridgemodel)

plot(ridgemodel$finalModel,xvar = 'lambda',label = T)

plot(varImp(ridgemodel))

##lasso model
set.seed(1234)
lassomodel<-train(Price~.,data7,method='glmnet',tuneGrid=expand.grid(alpha=1,lambda=seq(0.0001,1,length=5)),trcontrol=custom)
lassomodel
#The final values used for the model were alpha = 1 and lambda = 1

#lambda    RMSE      Rsquared   MAE  
#1.000000  1365.582  0.8613606  1008.312

#plot

plot(lassomodel)

plot(lassomodel$finalModel,label = T,xvar = 'lambda')

plot(varImp(lassomodel))


#elastic model
#elasitc model is the combination of lasso and ridge model

elasticmodel<-train(Price~.,data7,method='glmnet',tuneGrid=expand.grid(alpha=seq(0,1,length=10),lambda=seq(0.0001,0.2,length=5)),trcontrol=custom)

elasticmodel

#plot

plot(elasticmodel)

plot(elasticmodel$finalModel,label = T,xvar = 'lambda')

plot(varImp(elasticmodel))


#comparing the models 

model_list<-list(LinearModel=modeltyo2,Ridge=ridgemodel,Lasso=lassomodel,ElasticNet=elasticmodel)

res<-resamples(list(LinearModel=modeltyo2,Ridge=ridgemodel,Lasso=lassomodel,ElasticNet=elasticmodel))

summary(res)

#plot

bwplot(res)
