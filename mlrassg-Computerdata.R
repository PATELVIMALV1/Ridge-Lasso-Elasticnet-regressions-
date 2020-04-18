attach(Computer_Data)
data<-Computer_Data[,-1]
summary(data)

#coorelation

coorelation<-list(cor(price,speed,method = "spearman"),
                   cor(price,hd,method = "spearman"),
                   cor(price,ram,method = "spearman"),
                   cor(price,screen,method = "spearman"),
                   cor(price,ads,method = "spearman"),
                   cor(price,trend,method = "spearman")
)
coorelation

#standard devation

standarddevation<-list(sd(price),
                       sd(speed),
                       sd(hd),
                       sd(ram),
                       sd(screen),
                       sd(ads),
                       sd(trend)
                      )
standarddevation

#plot

plot(data)

panel.cor<-function(x,y,digits=2,prefix="",cex.cor=3,col='blue')
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

library(ggplot2)
windows()
ggplot(data = train,aes(x=speed+hd+ram+screen+ads+trend,y=price))+geom_point(color='red')+geom_line(color='yellow',data = data,aes(x=speed+hd+ram+screen+ads+trend,y=price))
ComputerData<-Computer_Data

ComputerData<-data.frame(ComputerData$price,ComputerData$speed,ComputerData$hd,
                         
                         ComputerData$ram,ComputerData$screen,ifelse(ComputerData$cd=='yes',1,0),
                         
                         ifelse(ComputerData$multi=='yes',1,0),ifelse(ComputerData$premium=='yes',1,0),
                         
                         ComputerData$ads,ComputerData$trend)
colnames(ComputerData)


ComputerData<-data.frame("price"=ComputerData$ComputerData.price,"speed"= ComputerData$ComputerData.speed,
                         
                         "hd"=ComputerData$ComputerData.hd,"ram"=ComputerData$ComputerData.ram,
                         
                         "screen"=ComputerData$ComputerData.screen,"cd"=ComputerData$ifelse.ComputerData.cd.....yes...1..0.,
                         
                         "multi"=ComputerData$ifelse.ComputerData.multi.....yes...1..0.,
                         
                         "premium"=ComputerData$ifelse.ComputerData.premium.....yes...1..0.,
                         
                         "ads"= ComputerData$ComputerData.ads,"trend"= ComputerData$ComputerData.trend)

#new data
View(ComputerData)

#setting train and test

ind<-sample(2,nrow(ComputerData),replace = T,prob = c(0.7,03))
train<-ComputerData[ind==1,]
test<-ComputerData[ind==2,]
library(caret)
custom<-trainControl(method = 'repeatedcv',number = 10,repeats = 5,verboseIter = T)



#simple linear model

model1<-train(price~.,data=ComputerData,method='lm',trcontrol=custom)

model1

summary(model1)

#Multiple R-squared:  0.7756,	Adjusted R-squared:  0.7752

#plot

plot(model1$finalModel)

plot(varImp(model1))

#expontial model

expomodel<-train(log(price)~.,data=ComputerData,method='lm',trcontrol=custom)


expomodel

summary(expomodel)

#Multiple R-squared:  0.7832,	Adjusted R-squared:  0.7829 

#plot

plot(expomodel$finalModel)

plot(varImp(expomodel))

#polynomial 2-degree

poly2model<-train(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+
                    
                    screen+I(screen^2)+cd+I(cd^2)+multi+I(multi^2)+
                    
                    premium+I(premium^2)+ads+I(ads^2)+trend+I(trend^2),data=ComputerData,method='lm',trcontrol=custom)
poly2model

summary(poly2model)

plot(poly2model$finalModel)

plot(varImp(poly2model))

#polymomial 3-degree 

poly3model<-train(price~speed+I(speed^2)+I(speed^3)+hd+I(hd^2)+I(hd^3)+ram+I(ram^2)+I(ram^3)+
                    
                    screen+I(screen^2)+I(screen^3)+cd+I(cd^2)+I(cd^3)+multi+I(multi^2)+I(multi^3)+
                    
                    premium+I(premium^2)+I(premium^3)+ads+I(ads^2)+I(ads^3)+trend+I(trend^2)+I(trend^3),data=ComputerData,,method='lm',trcontrol=custom)
poly3model

summary(poly3model)
#Multiple R-squared:  0.8116,	Adjusted R-squared:  0.811 

#plot

plot(poly3model$finalModel)

poly(varImp(poly3model))

#compare

model_list<-list(LinearModel=model1,ExpontialModel=expomodel,QuadraticModel=poly2model,   PolymomialModel=poly3model)

model_list

res<-resamples(model_list)

View(summary(res))     

#since polynomia 3_degree model has the highest R-Sqaured value=0.8116,thus selelecting poly3model 

View(predict(poly3model,newdata = test))


#plot

plot(poly3model$finalModel)


