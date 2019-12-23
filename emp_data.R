library(readr)
emp_data <- read_csv("E:/data sci/excelr/Assignments/Simple Linear Regression/emp_data.csv")

View(emp_data)  #view dataset

summary(emp_data)  #summary of dataset/EDA

#Scatterplot of input Vs otput
plot(emp_data$Salary_hike, emp_data$Churn_out_rate)  # plot(X,Y)
#From plot we can say that data is linearity is there,strength is moderate (sub to check r value),
#& Direction is negative

#attached dataset
attach(emp_data)

#Correlation between output to input
cor(Salary_hike, Churn_out_rate)     #cor(x,y)
#from value of correlation coe.(r) we can say that very good correlation between o/p & i/p

# Simple Linear Regression model
reg <- lm(Churn_out_rate ~ Salary_hike) # lm(Y ~ X)

#Summary of regression model
summary(reg)
#first thing is that variable is siginificant as value is less than 0.05
#R^2 value is more than 0.80 so we can say that model is bestfit.
#We can write eq. as CRT=244.36-0.101(SH)

#check fitted values(predicted)
reg$fitted.values
reg$residuals

#but we have to check with predicted values
pred <- predict(reg)

#Check for error associated with each obs.
reg$residuals
sum(reg$residuals)

#check for mean of sum of errors is equal to 0.
mean(reg$residuals)
hist(reg$residuals) # check errors are normally distributed or not.

#Check for RMSE value
sqrt(sum(reg$residuals^2)/nrow(emp_data))  #RMSE
sqrt(mean(reg$residuals^2))

#interval for 5% of confidence
confint(reg,level=0.95)

predict(reg,interval="predict")

#visualising model
library(ggplot2)

ggplot(data = emp_data, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike, y=pred))


#Inferences-
#From all above  value or correlatio coe.r is -0.91 which is very good
# function is linear in nature i.e. lm(CRT ~ SH), 
# Coe. are significant and coe.of Determination value (R^2) is 0.83 which is very good
# mean of errors is 4.449566e-17 which is almost 0 ans errors are almost normally distributed.
# RMSE value is 3.997528 which is nearest to lower range value
# so as model bestfitted but we need to go with more transformation due to errors normal distribution problem




# Logarithamic Model

# x = log(Salary hike) ; y = Churn out rate

#Scatterplot of input Vs output
plot(log(Salary_hike), Churn_out_rate)  # plot(log(X),Y)
#From plot we can say that data is linearity is there,strength is good (sub to check r value),
#& Direction is negative

#Correlation between output to input
cor(log(Salary_hike), Churn_out_rate)   # cor(log(X),Y)
#from value of correlation coe.(r) we can say that very good correlation between o/p & i/p

# Simple Linear Regression model-log transform
reg_log <- lm(Churn_out_rate ~ log(Salary_hike))   # lm(Y ~ X)

#Summary of regression model
summary(reg_log)
#first thing is that variable is siginificant as value is less than 0.05
#R^2 value is more than 0.80 so we can say that model is good and improved than previous
#We can write eq. as CRT=1381.5-176.1(log(SH))

#check fitted values(predicted)
reg_log$fitted.values
reg_log$residuals

#but we have to check with predicted values
predict(reg_log)

#Check for error associated with each obs.
reg_log$residuals
sum(reg_log$residuals)

#check for mean of sum of errors is equal to 0.
mean(reg_log$residuals)
hist(reg_log$residuals) # check errors are normally distributed or not.

#Check for RMSE value
sqrt(sum(reg_log$residuals^2)/nrow(emp_data))  #RMSE
sqrt(mean(reg_log$residuals^2))

#interval for 5% of confidence
confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

#visualising model
library(ggplot2)

ggplot(data = emp_data, aes(x = log(Salary_hike), y = `Churn_out_rate`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=log(Salary_hike), y=pred))

#Inferences-
#From all above  value or correlation coe.(r) is -0.92 which is best value ,
# function is linear in nature i.e. lm(CRT ~ log(SH)), 
# Coe. are significant and coe.of Determination value (R^2) is 0.8486 which is very good also improved than previous.
# mean of errors is 4.885849e-16 which is almost 0 ans errors are almost normally distributed.
# RMSE value is 3.786004 decreased slightly than previous model which is nearest to lower range value
# so as model bestfitted but we need to go with more transformation due to errors normal distribution problem




# Exponential Model

# x = Salary_hike and y = log(Churn_out_rate)

#Scatterplot of input Vs output
plot(Salary_hike , log(Churn_out_rate))   #plot(x,log(y)) 
#From plot we can say that data is linearity is there,strength is very good (sub to check r value),
#& Direction is negative

#Correlation between output to input
cor(Salary_hike, log(Churn_out_rate))    #cor(x,log(y))
#from value of correlation coe.(r) we can say that very good correlation between o/p & i/p

# Simple Linear Regression model-exp transform
reg_exp <- lm(log(Churn_out_rate) ~ Salary_hike )  #lm(log(Y) ~ X)

#Summary of regression model
summary(reg_exp)
#first thing is that variable is siginificant as value is less than 0.05
#R^2 value is more than 0.80 so it is good to go with best fit model
#We can write eq. as log(CRT)=6.638-0.001(SH)

#check fitted values(predicted)
reg_exp$fitted.values
reg_exp$residuals

#but we have to check with predicted values
predict(reg_exp)

#convert exp values to normal
logcrt <- predict(reg_exp)
crt <- exp(logcrt)

#Check for error associated with each obs.
error = emp_data$`Churn_out_rate` - crt
error
sum(error)

#check for mean of sum of errors is equal to 0.
mean(error)
hist(error) # check errors are normally distributed or not

#Check for RMSE value
sqrt(sum(error^2)/nrow(emp_data))  #RMSE
sqrt(mean(error^2))

#interval for 5% of confidence
confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

#visualising model
library(ggplot2)

ggplot(data = emp_data, aes(x = `Salary_hike`, y = log(`Churn_out_rate`))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=`Salary_hike`, y=pred))


#Inferences-
#From all above  value or correlation coe.(r) is -0.93 it means very good correlation ,
# function is linear in nature i.e. lm(log(CRT) ~ SH), 
# Coe. are significant and coe.of Determination value (R^2) is 0.87 it is good.
# mean of errors is 0.1034638 which is almost 0 ans errors are almost normally distributed.
# RMSE value is 3.541549 decreased slightly than previous model which is nearest to lower range value of delivery time
# so this model bestfit in all above transformations.




