# loading packages
library(ggplot2)
library(factoextra)
library(stringr)
library(tidyr)
library(gridExtra)
library(FunCluster)
library(rpart)
library(caret)
library(rattle)

#loading the data 
papers<-read.csv("C:/Users/lewis/Downloads/fedPapers85.csv")
str(papers)
#summary of the authors 
summary(papers$author)
#creating a new column with an abbreviation of the author name 
papers$owner<- ifelse(papers$author== 'HM', 'HM', ifelse(papers$author== 'Jay', 'J', ifelse(papers$author== 'Madison', 'M', ifelse(papers$author== 'dispt', 'D', ifelse(papers$author=='Hamilton', 'H',NA)))))
#splitting the file name and file number 
papers<- extract(papers, filename, into= c("Name", "Num"), "([^(]+)\\s*[^0-9]+([0-9].).")
#Combine the author name with the file number 
papers$file<-paste(papers$owner, "-", papers$Num)
rownames(papers)<- papers$file
#Drop unnecessary columns 
papers<- papers[c(-(ncol(papers)-1))]
papers<- papers[c(-(ncol(papers)))]
papers<- papers[c(-2,-3)]
#Authored by Jay and Hamilton/Madison
n<- papers[papers$author!='Jay',]
papers<- n[n$author!='HM',]
#View the dataset 
View(papers)
#dropping unused levels 
papers<- droplevels(papers)
#Review the data top 5
head(papers,5)
#Decision Tree; m=test and l= train 
m<- papers[papers$author=='dispt',]
l<-papers[papers$author!= 'dispt',]
#Dropping the unused levels 
m<-droplevels(m)
l<- droplevels(l)
#Training the model with l dataset
dt_model<-train(author~., data= l, metric= 'Accuracy', method= 'rpart')
dt_predict<-predict(dt_model, newdata= m, na.action=na.omit, type= 'prob')
head(dt_predict, 11)
#printing the final model 
print(dt_model)
#Plotting the final model 
fancyRpartPlot(dt_model$finalModel)
#Model Prediction 
raw_dt_predict<- predict(dt_model, newdata = m, type = 'raw')
print(raw_dt_predict)
#Modeling Tuning 
dt_model_preprune <- train(author ~ ., data = l, method = "rpart",
                           metric = "Accuracy",
                           tuneLength = 8,
                           control = rpart.control(minsplit = 50, minbucket = 20, maxdepth = 6))
print(dt_model_preprune$finalModel)
#New Model 
fancyRpartPlot(dt_model_preprune$finalModel)