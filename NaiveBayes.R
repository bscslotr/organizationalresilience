#Downloading Libraries
# working directory i kodu açtigimiz folder a set etmek. 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
install.packages(c("sem", "lavaan", "lavaanPlot", "semPLS", "semPlot"))
install.packages("plspm")
libs <- c("readxl", "Rcmdr", "lavaan", "car", "nortest", "psych", "PMCMRplus")
for (i in libs){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
    install.packages(i)
  }.
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
library(naivebayes)
library(kernlab)
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
#Train and Test Split
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(mdata), size = floor(.70*nrow(mdata)), replace = F)
training <- mdata[sample, ]
testing  <- mdata[-sample, ]
# Naive Bayes Classification 0.91 on training 0.88 on test
model <- naive_bayes(c~c1+c2+c3+c4+c5+c6+c7+c8+c9, data = training, usekernel = T) 
plot(model) 
p <- predict(model, training, type = 'prob')
head(cbind(p, training))
p1 <- predict(model, training)
tab1 <- table(p1, training$c)
1 - sum(diag(tab1)) / sum(tab1)
#Testing
p2 <- predict(model, testing)
tab2 <- table(p2, testing$c)
1 - sum(diag(tab2)) / sum(tab2)
#Naive Bayes C and A - Training 0.63, 0.64 testing
plot(model1) 
p3 <- predict(model1, training, type = 'prob')
head(cbind(p3, training))
p4 <- predict(model1, training)
tab3 <- table(p4, training$c)
1 - sum(diag(tab3)) / sum(tab3)
#Testing
p5 <- predict(model1, testing)
tab4 <- table(p5, testing$c)
1 - sum(diag(tab4)) / sum(tab4)
#Naive Bayes C and B- Training 0.67, 0.66 testing
model2 <- naive_bayes(c~b1+b2+b3+b4+b5, data = training, usekernel = T) 
plot(model2) 
p4 <- predict(model2, training, type = 'prob')
head(cbind(p4, training))
p5 <- predict(model2, training)
tab5 <- table(p5, training$c)
1 - sum(diag(tab5)) / sum(tab5)
#Testing 
p6 <- predict(model2, testing)
tab6 <- table(p6, testing$c)
1 - sum(diag(tab6)) / sum(tab6)
#Naive Bayes C and D - Training 0.59, 0.51 testing
model3 <- naive_bayes(c~d1+d2+d3+d4+d5, data = training, usekernel = T) 
plot(model3) 
p7 <- predict(model3, training, type = 'prob')
head(cbind(p7, training))
p8 <- predict(model3, training)
tab7 <- table(p8, training$c)
1 - sum(diag(tab7)) / sum(tab7)
#Testing 
p9 <- predict(model3, testing)
tab8 <- table(p9, testing$c)
1 - sum(diag(tab8)) / sum(tab8)
#SVM Methods
news_classifier <- ksvm(c ~c1+c2+c3+c4+c5+c6+c7+c8+c9, data= training,kernel="vanilladot")
news_predictions <- predict(news_classifier, testing)
p11 <- table(news_predictions,testing$c)
accuracy <- sum(diag(p11))/sum(p11)*100
news_classifier1 <- ksvm(c ~d1+d2+d3+d4+d5, data= training,kernel="rbfdot")
news_predictions1 <- predict(news_classifier1, testing)
p12 <- table(news_predictions1,testing$c)
accuracy1 <- sum(diag(p12))/sum(p12)*100
news_classifier2 <- ksvm(c ~c1+c2+c3+c4+c5+c6+c7+c8+c9, data= training,kernel="polydot")
news_predictions2 <- predict(news_classifier2, testing)
p13 <- table(news_predictions2,testing$c)
accuracy2 <- sum(diag(p13))/sum(p13)*100
