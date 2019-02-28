# Read the file HousePrices.csv (attached).
# Create two different tests: one called train_data with 85% of the observations and another one called test_data with the remaining 15%. Try to do it sampling random rows, otherwise, just separate in first 85% rows and last 15%.
# Create a visual and revealing correlation matrix that summarizes the relationship among all pairs of variables, including SalePrice. Which variable best (lineary) explains SalePrice by itself? Are there some (potential) explanatory variables strongly correlated?
#   Build a linear model that explains SalePrice with some of the rest of variables.  Are all variables statistically significant at a 95% of confidence? What is the R Square of the model?
#   Analize the residuals. Are there outliers? Are they normally distributed? Does the variance seem constant?
#   Generate the fitted.values for the training set, and also the predicted values for the test set.
# Generate the metric Mean Absolute Error for both predictions, by averaging the absolute value of SalePrice minus its prediction.
# Try to create a new model but explaining now the log of SalePrice, and repeat the previous three points.

library(data.table)

raw_data<-fread('HousePrices.csv')

str(raw_data)

head(raw_data)


dev.off()
library(corrplot)
corrplot(cor(raw_data), 'ellipse')


# data partition

train_index<-sample(nrow(raw_data), floor(nrow(raw_data)*0.85))

train_data<-raw_data[train_index]
test_data<-raw_data[-train_index]


# feature selecction

library(randomForest)
names(train_data)
rf_1<-randomForest(x=train_data[,-'SalePrice', with=F], y=train_data$SalePrice,
                   ntree=1000, importance = T)


importance(rf_1)
varImpPlot(rf_1,type=2)

var_imp<-data.table(variable=rownames(importance(rf_1)),importance(rf_1))
var_imp<-var_imp[order(-IncNodePurity)]


corrplot(cor(train_data[,var_imp$variable[1:15], with=F]), 'number')

# eliminar Garage_Yr_Built, Garage_Area


relevant_vars<-var_imp$variable[1:15][! var_imp$variable[1:15]  %in% c('Garage_Yr_Blt','Garage_Area','1st_Flr_SF')]

relevant_vars

# lm_model


lm_model<-lm(as.formula(SalePrice ~ Overall_Qual+Year_Built+Gr_Liv_Area+Full_Bath+Total_Bsmt_SF+`Year_Remod/Add`+
                          Lot_Area+`2nd_Flr_SF`+BsmtFin_SF_1+Lot_Frontage+Fireplaces), 
             data=train_data)
summary(lm_model)

lm_model<-lm(SalePrice ~ Overall_Qual+Year_Built+Gr_Liv_Area+Total_Bsmt_SF+`Year_Remod/Add`+
               Lot_Area+`2nd_Flr_SF`+BsmtFin_SF_1+Lot_Frontage+Fireplaces, 
             data=train_data)
summary(lm_model)

corrplot(cor(lm_model$model), 'number')

res_analysis<-function(model=lm_model){
  
  par(mfrow=c(2,2))
  # 1. line plot
  plot(model$residuals, type='l', main='Model Residuals'); grid()
  
  # 2. fitted Vs residuals plot
  plot(fitted(model),model$residuals,  main='Fitted Vs Residuals'); grid()
  
  # 3. histogram
  norm_test<-shapiro.test(model$residuals)
  hist(model$residuals,  main='Histogram', xlab=paste0('p-Value = ', norm_test$p.value)); grid()
  
  # 4. boxplot
  boxplot(model$residuals, main='Boxplot'); grid()
  
  
  
}

res_analysis(lm_model)


# predictions

mae<-function(real, predicted){
  return(round(mean(abs(real-predicted))))
}

# train:fit of the model

fit_1<-lm_model$fitted.values
mae_training<-mae(train_data$SalePrice, fit_1)
mae_training

par(mfrow=c(1,1))
plot(train_data$SalePrice, type='l', xlab='train sample', 
     main=paste0('Training MAE = +/-',mae_training,'$')); grid()
lines(fit_1, col='red')


# test: predictions for new data
pred_1<-predict(lm_model, newdata = test_data)
mae_testing<-mae(test_data$SalePrice, pred_1)
mae_testing

par(mfrow=c(1,1))
plot(test_data$SalePrice, type='l', xlab='test sample', 
     main=paste0('Testing MAE = +/-',mae_testing,'$')); grid()
lines(pred_1, col='red')


# 2 log-linear model

lm_model_2<-lm(log(SalePrice) ~ Overall_Qual+Year_Built+Gr_Liv_Area+Total_Bsmt_SF+`Year_Remod/Add`+
                 Lot_Area+`2nd_Flr_SF`+BsmtFin_SF_1+Lot_Frontage+Fireplaces, 
               data=train_data)
summary(lm_model_2)


res_analysis_log<-function(model=lm_model_2){
  
  par(mfrow=c(2,2))
  # 1. line plot
  plot(model$residuals, type='l', main='Model Residuals'); grid()
  
  # 2. fitted Vs residuals plot
  plot(model$model$`log(SalePrice)` ,model$residuals,  main='Fitted Vs Residuals'); grid()
  
  # 3. histogram
  norm_test<-shapiro.test(model$residuals)
  hist(model$residuals,  main='Histogram', xlab=paste0('p-Value = ', norm_test$p.value)); grid()
  
  # 4. boxplot
  boxplot(model$residuals, main='Boxplot'); grid()
  
  
  
}

res_analysis_log(lm_model_2)


# predictions

mae<-function(real, predicted){
  return(round(mean(abs(real-predicted))))
}

# train:fit of the model

fit_2<-exp(lm_model_2$fitted.values)
mae_training_2<-mae(train_data$SalePrice, fit_2)

par(mfrow=c(1,1))
plot(train_data$SalePrice, type='l', xlab='train sample', 
     main=paste0('Log-Linear Training MAE = +/-',mae_training_2,'$')); grid()
lines(fit_2, col='red')


# test: predictions for new data
pred_2<-exp(predict(lm_model_2, newdata = test_data))
mae_testing_2<-mae(test_data$SalePrice, pred_2)

par(mfrow=c(1,1))
plot(test_data$SalePrice, type='l', xlab='test sample', 
     main=paste0('Log-Linear Testing MAE = +/-',mae_testing_2,'$')); grid()
lines(pred_2, col='red')








