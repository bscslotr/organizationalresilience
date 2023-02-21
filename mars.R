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
#PCA
PrincipalComponents <- princomp(mdata[c("c51",    
                                             "c52",  
                                             "c53",  
                                             "c54")], cor=TRUE)
summary(PrincipalComponents)
loadings(PrincipalComponents) # pc loadings
plot(PrincipalComponents,type="lines")
#EXTRA
mdata$c <- round(mdata$c)
mdata$d <- round(mdata$d)
mdata$c <-factor(mdata$c)
mdata$d <-factor(mdata$d)# Round off the column to integer
View(mdata)
#EXTRA
#Train and Test Split
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(mdata), size = floor(.70*nrow(mdata)), replace = F)
train <- mdata[sample, ]
test  <- mdata[-sample, ]
#Logistic Regression
mylogit <-glm(c~d, data = train, family = "binomial")
summary(mylogit)
predmy <- predict(mylogit, test, type = "response")
summary(predmy)
mylogit <-glm(c~a1+a2+a3+a4+a5, data = train, family = "binomial")
summary(mylogit)
predmy <- predict(mylogit, test, type = "response")
summary(predmy)
#Multiple Regression for the first part for c and relationship between c1+c2+c3+c4+c5+c6+c7+c8+c9
#Train
modelc1train <- lm(c ~ c1+c2+c3+c4+c5+c6+c7+c8+c9, data = train)
summary(modelc1train)
summary(modelc1train)$coefficient
avPlots(modelc1train)
vif(modelc1train)
#Test
modelc1test <- lm(c ~ c1+c2+c3+c4+c5+c6+c7+c8+c9, data = test)
summary(modelc1test)
summary(modelc1test)$coefficient
avPlots(modelc1test)
#Multiple Regression for the second part for A and its relationship with c on training dataset
modelatrain <- lm(c ~ a, data = train)
summary(modelatrain)
summary(modelatrain)$coefficient
modelcatrain <- lm(c ~ a1+a2+a3+a4+a5, data = train)
summary(modelcatrain)
summary(modelcatrain)$coefficient
vif(modelcatrain)
modelcsatrain <- lm(c1+c2+c3+c4+c5+c6+c7+c8+c9 ~ a1+a2+a3+a4+a5, data = train)
summary(modelcsatrain)
summary(modelcsatrain)$coefficient
#Multiple Regression for the second part for A and its relationship with c on test dataset
modelatest <- lm(c ~ a, data = test)
summary(modelatest)
summary(modelatest)$coefficient
modelcatest <- lm(c ~ a1+a2+a3+a4+a5, data = test)
summary(modelcatest)
summary(modelcatest)$coefficient
modelcsatest <- lm(c1+c2+c3+c4+c5+c6+c7+c8+c9 ~ a1+a2+a3+a4+a5, data = test)
summary(modelcsatest)
summary(modelcsatest)$coefficient
#Multiple Regression for the second part for B and its relationship with c on training dataset
modelbtrain <- lm(c ~ b, data = train)
summary(modelbtrain)
summary(modelbtrain)$coefficient
modelcbtrain <- lm(c ~ b1+b2+b3+b4+b5, data = train)
summary(modelcbtrain)
summary(modelcbtrain)$coefficient
modelcsbtrain <- lm(c1+c2+c3+c4+c5+c6+c7+c8+c9 ~ b1+b2+b3+b4+b5, data = train)
summary(modelcsbtrain)
summary(modelcsbtrain)$coefficient
#Multiple Regression for the second part for B and its relationship with c on test dataset
modelbtest <- lm(c ~ b, data = test)
summary(modelbtest)
summary(modelbtest)$coefficient
modelcbtest <- lm(c ~ b1+b2+b3+b4+b5, data = test)
summary(modelcbtest)
summary(modelcbtest)$coefficient
modelcsbtest <- lm(c1+c2+c3+c4+c5+c6+c7+c8+c9 ~ b1+b2+b3+b4+b5, data = test)
summary(modelcsbtest)
summary(modelcsbtest)$coefficient
#Multiple Regression for the second part for D and its relationship with c on training dataset
modeldtrain <- lm(c ~ d, data = train)
summary(modeldtrain)
summary(modeldtrain)$coefficient
modelcdtrain <- lm(c ~ d1+d2+d3+d4+d5, data = train)
summary(modelcdtrain)
summary(modelcdtrain)$coefficient
modelcsdtrain <- lm(c1+c2+c3+c4+c5+c6+c7+c8+c9 ~ d1+d2+d3+d4+d5, data = train)
summary(modelcsdtrain)
summary(modelcsdtrain)$coefficient
#Multiple Regression for the second part for D and its relationship with c on test dataset
modeldtest <- lm(c ~ d, data = test)
summary(modeldtest)
summary(modeldtest)$coefficient
modelcdtest <- lm(c ~ d1+d2+d3+d4+d5, data = test)
summary(modelcdtest)
summary(modelcdtest)$coefficient
modelcsdtest <- lm(c1+c2+c3+c4+c5+c6+c7+c8+c9 ~ d1+d2+d3+d4+d5, data = test)
summary(modelcsdtest)
summary(modelcsdtest)$coefficient
#MARS MODEL on training
# Fit a basic MARS model
mars1 <- earth(c1+c2+c3+c4+c5+c6+c7+c8+c9 ~ a1+a2+a3+a4+a5, data = train)
# Print model summary
print(mars1)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars1, nresponse = 1)
mars2 <- earth(c ~ c1+c2+c3+c4+c5+c6+c7+c8+c9, data = train)
# Print model summary
print(mars2)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars2, nresponse = 1)
mars3 <- earth(c ~ a1+a2+a3+a4+a5, data = train)
# Print model summary
print(mars3)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars3, nresponse = 1)
mars4 <- earth(c ~ b1+b2+b3+b4+b5, data = train)
# Print model summary
print(mars4)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars4, nresponse = 1)
mars5 <- earth(c ~ d1+d2+d3+d4+d5, data = train)
# Print model summary
print(mars5)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars5, nresponse = 1)
mars6 <- earth(c ~ a, data = train)
# Print model summary
print(mars6)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars6, nresponse = 1)
mars7 <- earth(c ~ b, data = train)
# Print model summary
print(mars7)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars7, nresponse = 1)
mars8 <- earth(c ~ a+b+d, data = train)
# Print model summary
print(mars8)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars8, nresponse = 1)
#MARS MODEL on testing
# Fit a basic MARS model
mars11 <- earth(c1+c2+c3+c4+c5+c6+c7+c8+c9 ~ a1+a2+a3+a4+a5, data = test)
# Print model summary
print(mars11)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars11, nresponse = 1)
mars22 <- earth(c ~ c1+c2+c3+c4+c5+c6+c7+c8+c9, data = test)
# Print model summary
print(mars22)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars22, nresponse = 1)
mars33 <- earth(c ~ a1+a2+a3+a4+a5, data = test)
# Print model summary
print(mars33)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars33, nresponse = 1)
mars44 <- earth(c ~ b1+b2+b3+b4+b5, data = test)
# Print model summary
print(mars44)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars44, nresponse = 1)
mars55 <- earth(c ~ d1+d2+d3+d4+d5, data = test)
# Print model summary
print(mars55)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars55, nresponse = 1)
mars66 <- earth(c ~ a, data = test)
# Print model summary
print(mars66)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars66, nresponse = 1)
mars77 <- earth(c ~ b, data = test)
# Print model summary
print(mars77)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars77, nresponse = 1)
mars88 <- earth(c ~ d, data = test)
# Print model summary
print(mars88)
#illustrates the model selection plot that graphs the GCV based on the number of terms retained in the model (x-axis) 
#which are constructed from a certain number of original predictors (right-hand y-axis)
plot(mars88, nresponse = 1)
