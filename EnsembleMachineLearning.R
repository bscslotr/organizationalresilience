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
library(naivebayes)
library(kernlab)
library(caretEnsemble)
library(randomForest)
library(AppliedPredictiveModeling)
library(writexl)
#Downloading the Data
library(readxl)
mdatacs <- read_excel("C:/Users/bau/Desktop/mdatacs.xlsx")
str(mdata)
# Names and dimentions of data
names(mdata)
dim(mdata)
# Summary of the dataset
summary(mdata)
#EXTRA
mdata$c <- round(mdata$c) # Round off the column to integer
mdata$c <- as.factor(mdata$c)
str(mdata$c)
levels(mdata$c) <- c("StrngD", "SmwhtD", "SmwhtA", "StrngA")
#View(mdata)
#Train and Test Split
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(mdatacs), size = floor(.70*nrow(mdatacs)), replace = F)
training <- mdatacs[sample, ]
testing  <- mdatacs[-sample, ]
#Train control Method -  Cross Validation
ctrl <- trainControl(method="repeatedcv", number=5, repeats=10, savePredictions=TRUE, classProbs=TRUE)
# C's on C, Acc = 0.9
fit.cv <- train(c~c1+c2+c3+c4+c5+c6+c7+c8+c9, data = training, method = "rf", trControl =ctrl)
#fit1cv <- randomForest(c ~ c1+c2+c3+c4+c5+c6+c7+c8+c9, data = training, mtry = 5, importance = TRUE)
#fit.cv <- randomForest(c~b1+b2+b3+b4+b5, data = training, trControl =ctrl)
print(fit.cv)
plot(fit.cv)
fit.cv$results
pred <- predict(fit.cv,testing)
#pred <- as.factor(pred)
#testing$c <- as.factor(testing$c)
confusionMatrix(pred,testing$c)
print(varImp(fit.cv)) #Variable Importance
plot(varImp(fit.cv))
rfp <- table(pred,testing$c)
acc_rf <- sum(diag(rfp)) / sum(rfp)*100
#Naive Bayes
model <- train(c~c1+c2+c3+c4+c5+c6+c7+c8+c9, data = training, method= "nb",trControl = ctrl) 
plot(model) 
p <- predict(model, training, type = 'prob')
head(cbind(p, training))
p1 <- predict(model, testing)
tab1 <- table(p1, testing$c)
acc_nb <- sum(diag(tab1)) / sum(tab1)*100
#SVM
news_classifier <- train(c ~c1+c2+c3+c4+c5+c6+c7+c8+c9, data= training, trControl =ctrl, method="svmPoly")
plot(news_classifier)
news_predictions <- predict(news_classifier, testing)
p11 <- table(news_predictions,testing$c)
acc_svm <- sum(diag(p11))/sum(p11)*100
predDF <- data.frame(pred, p1, news_predictions, c=testing$c, c1=testing$c1,c2=testing$c2,c3=testing$c3,c4=testing$c4,c5=testing$c5,c6=testing$c6,c7=testing$c7,c8=testing$c8,c9=testing$c9,a1=testing$a1, a2=testing$a2, a3=testing$a3, a4=testing$a4, a5=testing$a5,b1=testing$b1, b2=testing$b2, b3=testing$b3, b4=testing$b4, b5=testing$b5,d1=testing$d1, d2=testing$d2, d3=testing$d3, d4=testing$d4, d5=testing$d5)
modelStack <- train(c~ c1+c2+c3+c4+c5+c6+c7+c8+c9,data = predDF, trControl=ctrl, method="knn")
combPred <- predict(modelStack,testing)
tab2 <- table(combPred,testing$c)
acc_comb <- sum(diag(tab2)) / sum(tab2)*100
#PLOTS
transparentTheme(trans = .9)
featurePlot(x = mdata[,55:63], 
            y = mdata$c,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(9, 1), 
            auto.key = list(columns = 3))

models <- caretList(c ~c1+c2+c3+c4+c5+c6+c7+c8+c9,
                    data= training,
                    trControl = ctrl,
                    tuneList = list(
                      rf_train = caretModelSpec(method = "rf", tuneLength = 10),
                      nb_train = caretModelSpec(method = "nb", tuneLength = 10),
                      svm_train = caretModelSpec(method = "svmPoly", tuneLength = 10)))
                      
preds <- predict(models,testing)                     
                      
ens <- caretEnsemble(models)

