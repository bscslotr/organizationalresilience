#Downloading Libraries
# working directory i kodu açtigimiz folder a set etmek. 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
install.packages(c("sem", "lavaan", "lavaanPlot", "semPLS", "semPlot"))
install.packages("plspm")
libs <- c("readxl", "Rcmdr", "lavaan", "car", "nortest", "psych", "PMCMRplus")
for (i in libs){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
    install.packages(i)
  }
  library(i,character.only = TRUE)
}
lapply(libs, require, character.only = TRUE)
# Packages for SEM Analysis
library(lattice)
library(FactoMineR)
library(sem)
library(lavaan)
library(lavaanPlot)
library(semPLS)
library(semPlot)
library(plspm)
library(shiny)
library(car)
library(lme4)
library(Matrix)
library(psych)
library(mediation) #Mediation package
library(MASS)
library(rockchalk) #Graphing simple slopes; moderation
library(nlme)
library(multilevel) #Sobel Test
library(bda) #Another Sobel Test option
library(gvlma) #Testing Model Assumptions 
library(stargazer) #Handy regression tables
library(rcompanion)
library(ggplot2)
library(PerformanceAnalytics)
library(randomForest)
library(datasets)
library(caret)
library(sjPlot)
library(rsample)   # data splitting 
library(ggplot2)   # plotting
library(earth)     # fit MARS models
library(caret)     # automating the tuning process
library(vip)       # variable importance
library(pdp)
library(stats)
library(dplyr)
#Downloading the Data
library(readxl)
mdata <- read_excel("C:/Users/bau/Desktop/mdata.xlsx")
View(mdata)
attach(mdata)
# Names and dimentions of data
names(mdata)
dim(mdata)
# Summary of the dataset
summary(mdata)
sjp.corr(mdata)
#EXTRA
mdata$c <- round(mdata$c) # Round off the column to integer
mdata$c <- as.factor(mdata$c)
mdata$d <- round(mdata$d) # Round off the column to integer
mdata$d <- as.factor(mdata$d)
View(mdata)
#Train and Test Split
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(mdata), size = floor(.70*nrow(mdata)), replace = F)
training <- mdata[sample, ]
testing  <- mdata[-sample, ]
#Train control Method -  Cross Validation
ctrl <- trainControl(method = "cv", number = 5)
fit.cv <- train(c~c1+c2+c3+c4+c5+c6+c7+c8+c9, data = training, method = "rf", trControl =ctrl)
print(fit.cv)
plot(fit.cv)
fit.cv$results
pred <- predict(fit.cv,testing)
pred <- as.factor(pred)
testing$c <- as.factor(testing$c)
confusionMatrix(pred,testing$c)
print(varImp(fit.cv)) #Variable Importance
plot(varImp(fit.cv))
#A on C,  Acc = 0.65
fit1.cv <- train(c~a1+a2+a3+a4+a5, data = training, method = "rf", trControl =ctrl, tuneLength = 50)
print(fit1.cv)
plot(fit1.cv)
fit1.cv$results
pred1 <- predict(fit1.cv,testing)
pred1 <- as.factor(pred1)
confusionMatrix(pred1,testing$c)
print(varImp(fit1.cv)) #Variable Importance
plot(varImp(fit1.cv))
#B ob C, Acc= 0.66
fit2.cv <- train(c~b1+b2+b3+b4+b5, data = training, method = "rf", trControl =ctrl, tuneLength = 50)
print(fit2.cv)
plot(fit2.cv)
fit2.cv$results
pred2 <- predict(fit2.cv,testing)
pred2 <- as.factor(pred2)
confusionMatrix(pred2,testing$c)
print(varImp(fit2.cv)) #Variable Importance
plot(varImp(fit2.cv))
#D on C, Acc=0.57
fit3.cv <- train(c~d1+d2+d3+d4+d5, data = training, method = "rf", trControl =ctrl, tuneLength = 50)
print(fit3.cv)
plot(fit3.cv)
fit3.cv$results
pred3 <- predict(fit3.cv,testing)
pred3 <- as.factor(pred3)
confusionMatrix(pred3,testing$c)
print(varImp(fit3.cv)) #Variable Importance
plot(varImp(fit3.cv))










#RANDOM FOREST EXTRA CODE (RUN WITHOUT EXTRA PART ABOVE)
#Response variable should be a factor variable which is c
set.seed(222)
table(mdata$c)
rf1<- randomForest(c~c1+c2+c3+c4+c5+c6+c7+c8+c9, data=training, proximity=TRUE)
print(rf1)
#Prediction & Confusion Matrix - Training
p1 <- predict(rf1,training)
confusionMatrix(p1,training$c)
#Prediction & Confusion Matrix - Testing
p2 <- predict(rf1, testing)
confusionMatrix(p2, testing$c)
plot(rf1)
#No. of nodes for the trees
hist(treesize(rf1), main = "No. of Nodes for the Trees", col = "green")
#Variable Importance
varImpPlot(rf1)
