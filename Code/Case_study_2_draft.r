#load packages and data
#install.packages("randomForest")
library(randomForest)
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
jobs.mds <- cmdscale(1 - jobs.rf$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(jobDF[,1:4], jobs.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(jobDF$Attrition)],
      main="Job Attrition Data Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(jobs.mds$GOF)

#test regression on proximity
str(jobs.rf$importance)

#plot most important features
varImpPlot(jobs.rf)
importanceOrder <- order(-jobs.rf$importance)
names <- rownames(jobs.rf$importance)
for (name in names)
    partialPlot(jobs.rf, jobDF, eval(name), main=name, xlab=name)