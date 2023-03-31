setwd("C:/Users/14817/Desktop/MSDS/596 Regression & Time Series Analysis")
whitewine <- read.csv("wineQualityWhites.csv",header = TRUE)
attach(whitewine)

out.null<-lm(quality~1,data=whitewine)
out.full <- lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar
               +chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH
               +sulphates+alcohol,data=whitewine)
full=formula(lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar
                +chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH
                +sulphates+alcohol,data=whitewine))
summary(out.full)
head(whitewine)

out.both=step(out.full,scope=list(lower=~1,upper=full),
              k=2,direction="both",trace=TRUE)
out.both$coefficients

out.both=step(out.full,scope=list(lower=~1,upper=full),
               k=log(n),direction="both",trace=TRUE)
out.both$coefficients
lmod <- lm(quality ~ fixed.acidity + volatile.acidity + residual.sugar + 
  free.sulfur.dioxide + density + pH + sulphates + alcohol,whitewine)
summary(lmod)

#Adjusted Rsquared, AIC, BIC
print(c(summary(out.null)$adj.r.squared,summary(out.full)$adj.r.squared,summary(lmod)$adj.r.squared))
print(c(AIC(out.null),AIC(out.full),AIC(lmod)))
print(c(BIC(out.null),BIC(out.full),BIC(lmod)))

#5-fold Cross Validation
library(boot)
set.seed(12345)
print(cv.glm(data=whitewine,glm(quality ~ fixed.acidity + volatile.acidity + residual.sugar + free.sulfur.dioxide + density + pH + sulphates + alcohol,data=whitewine),K=5)$delta[1])
print(cv.glm(data=whitewine,glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar
                                +chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH
                                +sulphates+alcohol,data=whitewine),K=5)$delta[1])
print(cv.glm(data=whitewine,glm(quality~1,data=whitewine),K=5)$delta[1])

print(cv.glm(data=whitewine,glm(quality~fixed.acidity,data=whitewine),K=5)$delta[1])
print(cv.glm(data=whitewine,glm(quality~residual.sugar,data=whitewine),K=5)$delta[1])
print(cv.glm(data=whitewine,glm(quality~free.sulfur.dioxide,data=whitewine),K=5)$delta[1])
print(cv.glm(data=whitewine,glm(quality~density,data=whitewine),K=5)$delta[1])
print(cv.glm(data=whitewine,glm(quality~pH,data=whitewine),K=5)$delta[1])
print(cv.glm(data=whitewine,glm(quality~sulphates,data=whitewine),K=5)$delta[1])
print(cv.glm(data=whitewine,glm(quality~alcohol,data=whitewine),K=5)$delta[1])




qqnorm(residuals(lmod))
qqline(residuals(lmod))
shapiro.test(residuals(lmod))
dwtest(lmod)

#leverages

hatv<-hatvalues(lmod)
head(sort(hatv,decreasing=T))
which(hatv>2*9/nrow(whitewine))

plot(fitted(lmod),residuals(lmod),xlab="Fitted",ylab="Residuals")

#outliers

jackres<-rstudent(lmod)
head(jackres[order(abs(jackres),decreasing=T)])
abs(qt(0.025/4898,4898-9-1)) #val is 4.417339

#5 obs greater than Bonferonni corrected rejection criteria: 
#4746, 2782, 3308, 254, 446

#multicollinearity (notable issue):

x<-model.matrix(lmod)
#install.packages("car")
library(car)
vif(lmod) #average VIF>1 and VIF>10 for density and residual.sugar

e<-eigen(t(x) %*% x)
round(e$val,3)
sqrt(e$val[1],e$val)

#Influential Observations
#library(faraway)
halfnorm(cooks.distance(lmod),3,ylab="Cook's Distance") #Note observation 2782

quailty_factor <- factor(quality,levels = 1:10)
library(MASS)
pom <- polr(quailty_factor ~ fixed.acidity + volatile.acidity + residual.sugar + 
free.sulfur.dioxide + density + pH + sulphates + alcohol, data=whitewine)


whitewinemod <- whitewine[,-5]
whitewinemod <-whitewinemod[,-3]
whitewinemod <-whitewinemod[,-5]

library(corrplot)
M <- cor(whitewine)
M1<- cor(fixed.acidity,volatile.acidity,residual.sugar,free.sulfur.dioxide
         ,density,pH,sulphates,alcohol)
corrplot(M, method = "number")
corrplot(cor(whitewinemod),method="number")

b1<-BIC(lm(quality~1,data=whitewine)) 
b2<-BIC(lm(quality~alcohol+volatile.acidity,data=whitewine)) 
b3<-BIC(lm(quality~alcohol+volatile.acidity+residual.sugar,data=whitewine)) 
b4<-BIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide,data=whitewine)) 
b5<-BIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density,data=whitewine)) 
b6<-BIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH,data=whitewine))
b7<-BIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates,data=whitewine))
b8<-BIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+fixed.acidity,data=whitewine))
b9<-BIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+fixed.acidity+total.sulfur.dioxide,data=whitewine))
b10<-BIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+fixed.acidity+total.sulfur.dioxide+chlorides,data=whitewine))
b11<-BIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+fixed.acidity+total.sulfur.dioxide+chlorides+citric.acid,data=whitewine))
plot(c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11),xlab = "numbers of predictors", ylab = "BIC", main="BIC against Number of Predictors")
lines(c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11))

a1<-AIC(lm(quality~1,data=whitewine)) 
a2<-AIC(lm(quality~alcohol+volatile.acidity,data=whitewine)) 
a3<-AIC(lm(quality~alcohol+volatile.acidity+residual.sugar,data=whitewine)) 
a4<-AIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide,data=whitewine)) 
a5<-AIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density,data=whitewine)) 
a6<-AIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH,data=whitewine))
a7<-AIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates,data=whitewine))
a8<-AIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+fixed.acidity,data=whitewine))
a9<-AIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+fixed.acidity+total.sulfur.dioxide,data=whitewine))
a10<-AIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+fixed.acidity+total.sulfur.dioxide+chlorides,data=whitewine))
a11<-AIC(lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+fixed.acidity+total.sulfur.dioxide+chlorides+citric.acid,data=whitewine))
plot(c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11),xlab = "numbers of predictors", ylab = "AIC",main = "AIC for stepwise selection")
lines(c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11))




lm1<-lm(quality~1,data=whitewine)
lm2<-lm(quality~alcohol,data=whitewine)
lm3<-lm(quality~alcohol+volatile.acidity,data=whitewine) 
lm4<-lm(quality~alcohol+volatile.acidity+residual.sugar,data=whitewine)
lm5<-lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide,data=whitewine)
lm6<-lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density,data=whitewine)
lm7<-lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH,data=whitewine)
lm8<-lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates,data=whitewine)
lm9<-lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+fixed.acidity,data=whitewine)
lm10<-lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+fixed.acidity+total.sulfur.dioxide,data=whitewine)
lm11<-lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+fixed.acidity+total.sulfur.dioxide+chlorides,data=whitewine)
lm12<-lm(quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+fixed.acidity+total.sulfur.dioxide+chlorides+citric.acid,data=whitewine)
R2 <- c(summary(lm2)$adj.r.squared,summary(lm3)$adj.r.squared,
         summary(lm4)$adj.r.squared,summary(lm5)$adj.r.squared,
         summary(lm6)$adj.r.squared,summary(lm7)$adj.r.squared,
         summary(lm8)$adj.r.squared,summary(lm9)$adj.r.squared,
         summary(lm10)$adj.r.squared,summary(lm11)$adj.r.squared
         ,summary(lm12)$adj.r.squared)
plot(R2,xlab = "numbers of predictor",ylab = "adjusted Rsquared",main = "adjusted Rsquared")
lines(R2)


install.packages('randomForest')
library(pROC)
library(randomForest)

train_sub = sample(nrow(whitewine),7/10*nrow(whitewine))
train_data = whitewine[train_sub,]
test_data = whitewine[-train_sub,]
train_data$quality = as.factor(train_data$quality)
test_data$quality = as.factor(test_data$quality)
wine_randomforest <- randomForest(quality~., data = train_data,
                                   ntree =500,
                                   mtry=3,
                                   importance=TRUE ,
                                   proximity=TRUE)

varImpPlot(wine_randomforest, main = "variable importance")
pre_ran <- predict(wine_randomforest,newdata=test_data)
obs_p_ran = data.frame(prob=pre_ran,obs=test_data$quality)
table(test_data$quality,pre_ran,dnn=c("True Value","prediction value"))
ran_roc <- roc(test_data$quality,as.numeric(pre_ran))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='random forest ROC curve,mtry=3,ntree=500')
MDSplot(wine_randomforest,test_data$quality)

