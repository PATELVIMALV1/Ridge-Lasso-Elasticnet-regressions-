library(caret)
attach(`50_Startups`)

#to check if there is any  NA in data
na.omit(`50_Startups`)

data<-`50_Startups`

summary(`50_Startups`)

plot(`50_Startups`,cex=0.3)

cordata<-data[,-4]

#coorelation 
cor(cordata)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
library(corpcor)

cor2pcor(cor(cordata))

#plot the coorelation b/w the varibles
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(data,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")

#mpdel

#train and test
ind<-sample(2,nrow(data),replace = T,prob = c(0.7,0.3))

train<-data[ind==1,]

test<-data[ind==2,]

#simple linear regression model

model1<-lm(Profit~.,data = data)

summary(model1)
#Multiple R-squared:  0.9508	
#Adjusted R-squared:  0.9452



plot(model1)


model2<-lm(Profit~R.D.Spend,data)

summary(model2)
#Profit~Administration are no significant

model3<-lm(Profit~Marketing.Spend,data)

summary(model3)
#Profit~Marketing.Spend are significant

#model4<-lm(Profit~R.D.Spend+Marketing.Spend,data)
model4<-train(Profit~R.D.Spend+Marketing.Spend,data,method='lm',trcontrol=custom)

model4

summary(model4)

#Profit~R.D.Spend+Marketing.Spend both are significant

#  RMSE      Rsquared   MAE     
# 9394.955  0.9535989  7186.589


#plot

windows()
plot(model4$finalModel)

library(car)

vif(model1)

vif(model4)

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model1,id.n=2,id.cex=0.7)

avPlots(model4,id.n=2,id.cex=0.7)

#therefore vif function and avplots both have helped us conform that model4 is the best model
#therefore only R.D.Spend+Marketing.Spend has the highest contribution towards the profit 
#achiving the Multiple R-squared:  0.9505,	Adjusted R-squared:  0.9483

scatterplot(Marketing.Spend+R.D.Spend,Profit,data)

#ridge,lasso,elasticnet model

library(caret)

custom<-trainControl(method="repeatedcv",number=10,repeats=5,verboseIter=T)


ridgemodel<-train(Profit~.,data,method='glmnet',tuneGrid=expand.grid(alpha=0,lambda=seq(0.0001,1,length=5)),trcontrol=custom)

ridgemodel
#The final values used for the model were alpha = 0 and lambda = 1.

#lambda    RMSE      Rsquared   MAE  
# 1.000000 12036.54  0.9335637  8991.187

#plot

plot(ridgemodel$finalModel)

plot(ridgemodel$finalModel,xvar = 'lambda',label = T)

plot(varImp(ridgemodel))

#lasso model

lassomodel<-train(Profit~.,data,method='glmnet',tuneGrid=expand.grid(alpha=seq(0,1),lambda=seq(0.0001,1,length=5)),trcontrol=custom)
lassomodel
#The final values used for the model were alpha = 1 and lambda = 1.

# alpha      lambda    RMSE      Rsquared   MAE  
# 1.0000000  1.000000  10095.05  0.9437596  7833.836

#plot

plot(lassomodel)
plot(lassomodel$finalModel)
plot(varImp(lassomodel))


#elastic model

elasitcmodel<-train(Profit~.,data,method='glmnet',tuneGrid=expand.grid(alpha=seq(0,1,length=10),lambda=seq(0.0001,1,0.2)),trcontrol=custom)

elasitcmodel

# alpha      lambda   RMSE       Rsquared   MAE  
# 1.0000000  0.8001   9987.375   0.9506271  7645.367

#plot

plot(elasitcmodel)

plot(elasitcmodel$finalModel)

plot(varImp(elasitcmodel))


#compare models
model_list<-list(LinearModel=model4,Ridge=ridgemodel,Lasso=lassomodel,ElasticNet=elasitcmodel)
compare<-resamples(model_list)
compare
summary(compare)
#MAE 
#               Min.  1st Qu.   Median     Mean  3rd Qu.      Max. NA's
#LinearModel 5490.255 6155.453 7210.818 7186.589 7894.043 10876.675    0
#Ridge       5916.223 7415.352 8806.454 8639.291 9744.887 11346.602    0
#Lasso       5779.057 7351.552 7943.428 7983.277 8809.064  9779.478    0
#ElasticNet  5478.545 6645.301 7882.383 7645.367 8416.123  9403.179    0

#RMSE 
 #              Min.  1st Qu.    Median      Mean  3rd Qu.     Max. NA's
#LinearModel 6235.278 7873.040  9287.423  9394.955 10773.28 14244.80    0
#Ridge       7801.900 9183.823 10715.540 10943.155 12450.00 15324.84    0
#Lasso       7311.613 9240.184 10521.863 10535.477 12073.82 13613.39    0
#ElasticNet  7125.950 9162.489 10090.399  9987.375 11205.98 13000.40    0

#Rsquared 
#               Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#LinearModel 0.9088342 0.9354900 0.9577271 0.9535989 0.9674574 0.9842321    0
#Ridge       0.8825596 0.9251346 0.9312892 0.9330673 0.9462915 0.9759996    0
#Lasso       0.8859925 0.9280274 0.9452172 0.9424763 0.9613247 0.9726044    0
#ElasticNet  0.9188059 0.9360963 0.9534866 0.9506271 0.9611258 0.9820619    0

#plot

bwplot(compare)
