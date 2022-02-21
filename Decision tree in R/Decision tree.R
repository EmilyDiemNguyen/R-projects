install.packages('party')
library(party)

data <- read.csv(file="C:/Users/emily/OneDrive - St. Clair College/Desktop/semester 3/DAB 303 - Marketing Analytics/Week 12/Book1.csv")
View(data)
head(data)
str(data)
# No command can revert from categorical to numeric, need to convert to factor first
data$Age <- as.factor(data$Age)
data$City <- as.factor(data$City)
data$Gender <- as.factor(data$Gender)
data$Education <- as.factor(data$Education)
data$Profile <- as.factor(data$Profile)
str(data)
# keep target variable as categorical only
data$Age <- as.numeric(data$Age)
data$City <- as.numeric(data$City)
data$Gender <- as.numeric(data$Gender)
data$Education <- as.numeric(data$Education)
str(data)
View(data)

install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
data_ctree <- rpart(Profile ~ Age + City + Gender + Education, data = data, minsplit=  1,method = "class")

print(data_ctree)

rpart.plot(data_ctree)

####################################

install.packages('party')

library(party)

tree <- ctree(Profile ~ Age + City + Gender + Education, data = data)

print(tree)

plot(tree, type = 'simple')
