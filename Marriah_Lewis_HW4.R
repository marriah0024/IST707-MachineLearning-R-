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
#Euclidean distance calculation and visualization
distance<-get_dist(papers)
fviz_dist(distance, gradient = list(low='#BB0099', mid= 'white', high= '#FC6a2e'))
#K-means 
set.seed(42)
clus<-kmeans(papers[c(-1)], centers = 5)
#created a cluster table 
t(table(papers[,1], clus$cluster))
#Rename author column and plot the cluster; more cleaning needed 
fviz_cluster(clus, data = papers[c(-1)])
# How many clusters are needed?
set.seed(123)
q<-function(k){
  return(kmeans(papers[c(-1)],k, nstart = 30)$tot.withins)
}
k_values<-1:10
q_values<-purrr::map_dbl(k_values, q)
plot(x = k_values, y= q_values,
     type = 'b', frame= F,
     xlab = 'Number of clusters (k)',
     ylab = 'Total within clusters')

#Try 4 clusters 
set.seed(48)
four<-kmeans(papers[c(-1)], centers = 4, nstart = 30, iter.max = 100)
tab<-t(table(papers[,1], four$cluster))
#Plotting the four clusters 
fviz_cluster(four, data = papers[c(-1)])
#Cluster Growth 
k2<-kmeans(papers[c(-1)], centers = 2, nstart = 30)
k3<-kmeans(papers[c(-1)], centers = 3, nstart = 30)
k4<- kmeans(papers[c(-1)], centers = 4, nstart = 30)
k5<- kmeans(papers[c(-1)], centers = 5, nstart = 30)
k6<- kmeans(papers[c(-1)], centers=6, nstart= 30)
k7<- kmeans(papers[c(-1)], centers = 7, nstart = 30)
#plotting the clusters 
plot2<- fviz_cluster(k2, geom = "point", data = papers[c(-1)])+ ggtitle('k=2')
plot3<- fviz_cluster(k3, geom = "point", data = papers[c(-1)])+ ggtitle('k=3')
plot4<- fviz_cluster(k4, geom = "point", data = papers[c(-1)])+ ggtitle('k=4')
plot5<- fviz_cluster(k5, geom = "point", data = papers[c(-1)])+ ggtitle('k=5')
plot6<- fviz_cluster(k6, geom = "point", data = papers[c(-1)])+ ggtitle('k=6')
plot7<- fviz_cluster(k7, geom = "point", data = papers[c(-1)])+ ggtitle('k=7')
grid.arrange(plot2, plot3, plot4, plot5, plot6, plot7, nrow=3)

#Hierarchical clustering 
hac<-hclust(dist(papers[c(-1)],method = 'euclidean'), method = 'ward.D2')
#plot the hierarchical clustering 
plot.new()
plot(hac, main = 'Dendogram using HAC', xlab = rect.hclust(hac, k=4))

#Decision Tree m=test and l= train  
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



