install.packages("ISLR")
library(mlbench)
library(psych)
library(caret)
library(car)
library(glmnet)
library(ISLR)
library(boot)

Prob1=prob1_temp
prob1_tss_edit <- Prob1[-c(19,10,23),] #removing outliers
prob1_nitrate_edit <- Prob1[-c(2,18),] #removing outliers
Prob1_TSS = prob1_tss_edit[,c(-1,-18)] 
Prob1_Nitrate = prob1_tss_edit[,c(-1,-17)]

# Start with fitting a normal linear model: TSS and found multicollinearity 
TSS_model = glm(Total.Suspended.Solid..TSS.~.,data=Prob1_TSS)
vif(TSS_model)
sqrt(vif(TSS_model)) > 10


# Data Partition 
ind = sample(2, nrow(Prob1_TSS), replace = T, prob = c(0.8,0.2))
TSS_train = Prob1_TSS[ind==1,]
TSS_test = Prob1_TSS[ind==2,]

# Custom Control Parameter
custom = trainControl(method="repeatedcv",
                      number = 10,
                      repeats = 5, 
                      verboseIter = T)

# Linear Model 
set.seed(1)
TSS_Linear_model = train(Total.Suspended.Solid..TSS.~.,
                         TSS_train,
                         method = 'lm',
                         trControl = custom)
TSS_Linear_model$results
TSS_Linear_model
summary(TSS_Linear_model)
plot(TSS_Linear_model$finalModel)

# Lasso Regression 
TSS_Lasso = train(Total.Suspended.Solid..TSS.~.,
                  TSS_train,
                  method = 'glmnet',
                  tuneGrid = expand.grid(alpha=1,
                                         lambda= seq(0.0001,1,length=5)),
                  trControl=custom)
TSS_Lasso
plot(TSS_Lasso)
plot(TSS_Lasso$finalModel, xvar="lambda", label=T)
plot(TSS_Lasso$finalModel, xvar="dev", label=T)
plot(varImp(TSS_Lasso, scale=T))



# Elastic Net Regression 
set.seed(1)
TSS_EN = train(Total.Suspended.Solid..TSS.~.,
               TSS_train,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha=seq(0,1, length=10),
                                      lambda= seq(0.0001,1,length=5)),
               trControl=custom)
plot(TSS_EN)
plot(TSS_EN$finalModel)
plot(TSS_EN$finalModel, xvar="lambda", label=T)
plot(TSS_EN$finalModel, xvar="dev", label=T)
plot(varImp(TSS_EN, scale=T))



# Linear vs Lasso vs Elastic Net Model Comparison
model_list = list(LinearModel = TSS_Linear_model, LassoModel = TSS_Lasso,  
                  ElasticNet=TSS_EN)
res = resamples(model_list)
summary(res)
bwplot(res)
xyplot(res, metric = 'RMSE')
# RMSE is greatly reduced and R-squared increases! 

# Coefficients for Lasso and Elastic Net Regression model 
coef(TSS_Lasso$finalModel, TSS_Lasso$bestTune$lambda)
coef(TSS_EN$finalModel, TSS_EN$bestTune$lambda)






#################### TRIAL: Leave One Out Cross Validation#################### 
#MSE_LOOCV = cv.glm(Prob1_TSS,TSS_model)
#MSE_LOOCV$delta[1]

#for(i in 1:10){
#  model = glm()
#}
##############################################################################  
