#Load dataset 
install.packages('arules')
install.packages('arulesViz')
library(arules)
library(arulesViz)
library(dplyr)
data<- read.csv("~/IST707/dataset/bankdata_all.csv")
str(data)
#Data Cleaning (separating income and age into 3 levels)
data$income.bracket <- cut(data$income, 3, labels = c("low", "med", "high"))
data$age.group <- cut(data$age, 3)
#drop id, age, and income columns
data <- subset(data, select=-c(id))
data<- subset(data, select= -c(age, income))
data$children <- as.factor(data$children)
#Factorizing 
data<- mutate_if(data, is.character, as.factor)
str(data)
#Association Rules
rules<- apriori(data=data, parameter = list(support=0.005, confidence= .8))
#Towards the bottom of the image, it created 139113 rules which would take a considerable amount of time to sort through, hence newrule1 created. 
#Try again with a new rule 
newrule1 <- apriori(data = data, parameter = list(minlen=2, maxlen=100, supp = 0.05, conf=0.8), appearance = list(default="lhs", rhs=c("pep=NO", "pep=YES")))
#sort the rules by lift and list the top 5 
inspect(head(sort(newrule1, by="lift", decreasing = T), 5))
#Rule 2 
newrule2 <- apriori(data = data, parameter = list(minlen=1, maxlen=100, supp = 0.1, conf=0.9), appearance = list(default="lhs", rhs=c("pep=NO", "pep=YES")))
inspect(head(sort(newrule2, by="lift", decreasing = T), 5))
#Rule 3 
newrule3 <- apriori(data = data, parameter = list(minlen=1, maxlen=100, supp = 0.05, conf=0.8), appearance = list(default="lhs", rhs=c("pep=NO", "pep=YES")))
inspect(head(sort(newrule3, by="count", decreasing = T), 5))