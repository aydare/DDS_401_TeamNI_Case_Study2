#load packages and data
#install.packages("randomForest")
library(randomForest)
library(randomForestExplainer)
library(RCurl)
library(ggplot2)

jobURL <- getURL("https://raw.githubusercontent.com/mjwolfe91/DDS_401_TeamNI_Case_Study2/master/Data/CaseStudy2-data.csv")
jobDF <- read.csv(text=jobURL, header=TRUE)

#stratified Age groups
str(jobDF)
jobDF$AgeGroup <- cut(jobDF$X.U.FEFF.Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf))
levels(jobDF$AgeGroup) <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

#create random forest for attrition
set.seed(71)
jobs.rf <- randomForest(Attrition ~ ., data=jobDF, importance=TRUE, proximity=TRUE)
print(jobs.rf)

#check variable importance
round(importance(jobs.rf),2)


#do MDS on 1 - proximity:
explain_forest(jobs.rf, data=jobDF)

#test regression on proximity
str(jobs.rf$importance)

#plot most important features
varImpPlot(jobs.rf)
importanceOrder <- order(-jobs.rf$importance)
names <- rownames(jobs.rf$importance)
for (name in names)
    partialPlot(jobs.rf, jobDF, eval(name), main=name, xlab=name)

#multinom logistic model based on top 3 most important "yes" features
jobs.glm <- glm(Attrition ~ MonthlyIncome + StockOptionLevel + OverTime, data=jobDF, family=binomial(link='logit'))
summary(jobs.glm)

fitted.results <- predict(jobs.glm,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))