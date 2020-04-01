# Project setup
library(tidyverse)
library(dplyr)
library(DescTools)
library(caret)
library(data.table)
library(car)
library(readxl)
library(kableExtra)

# Read the data file in
College <- read_excel("College.xlsx")
View(College)

# Calculate summary data for the overall dataset
summary(College)

# Calculate standard deviation for completion rate (ycomp)
sd(College$ycomp)

# Create a scatter plot of the completion rate data (ycomp)
dotchart(College$ycomp,labels=row.names(College$CollegeCode),cex=.7,
         main="Completion Rate by College",
         xlab="Completion Rate")

# Training set will be 30% of College dataset
set.seed(3, sample.kind="Rounding")

test_index <- createDataPartition(y = College$ycomp, times = 1, p = 0.30, list = FALSE)
testing<- College[-test_index,]
training<- College[test_index,]

# Validate the data in the training set
dim(training)

# Validate the data in the testing set
dim(testing)

# Set up multivariate model 1 with college size, headcount and traditional student proportions
mod1<-lm(ycomp ~ size + hc + trad, data = training)

# Execute model 1
summary(mod1)

# Set up prediction for model 1
predictions1<- predict(mod1, testing) 

# Create a residual map for model 1
resid(mod1)

# Calculate RMSE for model 1
RMSE(testing$ycomp, predictions1)

#Assess model 1 accuracy 
sigma(mod1)/mean(College$ycomp)



#Set up multivariate model 2 by adding ft equivalent status per student
mod2<-lm(ycomp ~ size + hc + trad + fteps, data = training)

#Execute model 2
summary(mod2)

#Set up prediction for model 2
predictions2<- predict(mod2, testing) 

#Create a residual map for model 2
resid(mod2)

#Calculate RMSE for model 2
RMSE(testing$ycomp, predictions2)

# Assess model 2 accuracy 
sigma(mod2)/mean(College$ycomp)



# Set up multivariate model 3 by adding course success and retention rates
mod3<-lm(ycomp ~ size + + hc + trad + fteps + s1117 + r1117, data = training)

# Execute model 3
summary(mod3)

# Set up prediction for model 3
predictions3<- predict(mod3, testing) 

# Create a residual map for model 3
resid(mod3)

# Calculate RMSE for model 3
RMSE(testing$ycomp, predictions3)

# Assess model 3 accuracy 
sigma(mod3)/mean(College$ycomp)



#Set up multivariate model 4 by adding proportion of financial aid 
mod4<-lm(ycomp ~ size + + hc + trad + fteps + s1117 + r1117 + finaid, data = training)

#Execute model 4
summary(mod4)

#Set up prediction for model 4
predictions4<- predict(mod4, testing) 

#Create a residual map for model 4
resid(mod4)

#Calculate RMSE for model 4
RMSE(testing$ycomp, predictions4)

#Assess model 4 accuracy 
sigma(mod4)/mean(College$ycomp)



#Set up multivariate model 5 by adding the proportions of sex 
mod5<- lm(ycomp ~ size + + hc + trad + fteps + s1117 + r1117 + finaid + male +female +xgender, data = training)

#Execute model 5
summary(mod5)

#set up prediction for model 5
predictions5<- predict(mod5, testing) 

#Create a residual map for model 5
resid(mod5)

#Calculate RMSE for model 5
RMSE(testing$ycomp, predictions5)

#Assess model 5 accuracy 
sigma(mod5)/mean(College$ycomp)



#Set up multivariate model 6 by adding the proportions of race  
mod6<- lm(ycomp ~ size + + hc + trad + fteps + s1117 + r1117 + finaid + male +female +xgender + aabl + aiak + asian + hisp + paci +multi + white, data = training)

#Execute model 6
summary(mod6)

#set up prediction for model 6
predictions6<- predict(mod6, testing) 

#Create a residual map for model 6
resid(mod6)

#Calculate RMSE for Model 6
RMSE(testing$ycomp, predictions5)

#Assess model 6 accuracy 
sigma(mod6)/mean(College$ycomp)



#Set up multivariate model 7 by adding the proportion of full-time faculty
mod7<- lm(ycomp ~ size + + hc + trad + fteps + s1117 + r1117 + finaid + male +female +xgender + aabl + aiak + asian + hisp + paci +multi + white + ftf, data = training)

#Execute model 7
summary(mod7)

#set up prediction for model 7
Predictions7<- predict(mod5, testing) 

#Create a residual map for model 7
resid(mod7)

#Calculate RMSE for Model 7
RMSE(testing$ycomp, predictions5)

#Assess model 7 accuracy 
sigma(mod7)/mean(College$ycomp)



