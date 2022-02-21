#### 1. read the csv file in the folder

data1 <- read.csv(file="C:/Users/emily/OneDrive - St. Clair College/Desktop/semester 3/DAB 303 - Marketing Analytics/Week 12/USDA_Macronutrients.csv")
data2 <- read.csv(file="C:/Users/emily/OneDrive - St. Clair College/Desktop/semester 3/DAB 303 - Marketing Analytics/Week 12/USDA_Micronutrients.csv")

str(data1)
str(data2)

nrow(data1)
ncol(data1)
colnames(data1)
colnames(data2)

#### 2. merge the data frames using the variable "ID". Name the merged data frame "USDA"

data <- merge(data1, data2, by="ID")
head(data)
str(data)
nrow(data)
ncol(data)
colnames(data)
#### 3. Check the datatypes of the attributes. Delete the commas in the sodium and potassium records. Assign sodium and potassium as numeric data types

typeof(data)
sapply(data, class)
data$Sodium <- gsub("[,]","",data$Sodium)
data$Sodium <- as.numeric(data$Sodium)

data$Potassium <- gsub("[,]","",data$Potassium)
data$Potassium <- as.numeric(data$Potassium)
sapply(data, class)

sum(is.na(data))
sum(is.na(data$Sodium))
sum(is.na(data$Potassium))

#### 4. Remove records (rows) with missing values in more than 4 attributes (columns). How many records remain in the data frame?

data <- data[rowSums(is.na(data)) < 5,]
nrow(data)

#### 5. For records with missing values for Sugar, Vitamin E and Vitamin D, replace missing values with mean value the respective variable.

sum(is.na(data$Sugar))
mean(data$Sugar, na.rm = T)
SugarMean = mean(data$Sugar,na.rm = T)
SugarMean
data[is.na(data$Sugar), "Sugar"] = SugarMean
sum(is.na(data$Sugar))

sum(is.na(data$VitaminE))
mean(data$VitaminE, na.rm = T)
VitaminEMean = mean(data$VitaminE,na.rm = T)
VitaminEMean
data[is.na(data$VitaminE), "VitaminE"] = VitaminEMean
sum(is.na(data$VitaminE))

sum(is.na(data$VitaminD))
mean(data$VitaminD, na.rm = T)
VitaminDMean = mean(data$VitaminD,na.rm = T)
VitaminDMean
data[is.na(data$VitaminD), "VitaminD"] = VitaminDMean
sum(is.na(data$VitaminD))

sum(is.na(data))

#### 6. with a single line of code, remove all remaining records with missing values. Name the data frame "USDAclean". How many records remain in the data frame.

USDAclean <- na.omit(data)
sum(is.na(USDAclean))
nrow(USDAclean)
str(USDAclean)

#### 7. Which food has the highest sodium level?

USDAclean$Description[USDAclean$Sodium==max(USDAclean$Sodium)]
USDAclean[USDAclean$Sodium == max(USDAclean$Sodium), "Description"]

#### 8. Create a histogram of Vitamin C distribution in foods.
# Numeric variables : create histogram
# Categorical variables: frequency distribution, box plot

hist(USDAclean$VitaminC,
     col = "red",
     xlab = "Vitamin C",
     main = "histogram of Vitamin C distribution in foods",
     xlim = c(0,100), breaks = 100)
# OR
hist(USDAclean$VitaminC,
     col = 'cyan',
     xlab = "Vitamin C",
     main = "Histogram of Vitamin C distribution in Foods",
     xlim = c(0,100),
     breaks = 100)

#### 9. Create a box plot to illustrate the distribution of values for Total Fat, Protein and Carbohydrate
#use box plot to check data set has out liners or not

boxplot(USDAclean$TotalFat, USDAclean$Protein, USDAclean$Carbohydrate,
        col = rainbow(3),
        horizontal = TRUE,
        outline = F)
# OR

boxplot(USDAclean$TotalFat,USDAclean$Protein,USDAclean$Carbohydrate,
        col = rainbow(3),
        horizontal = TRUE,
        outline=T)

#### 10. Create a scatter plot to illustrate the relationship between a food's Total Fat content and its Calorie content. (6 points)

library(ggplot2)
plot1 <- ggplot(USDAclean, aes(x=TotalFat,y=Calories,colour="red")) + geom_point(shape=1)+ggtitle("Relation: between a food's TotalFat content and its Calorie content")
plot1


#### 11. Add a variable to the data frame that takes value 1 if the food has higher sodium than average, 0 otherwise. Call this variable High Sodium. Do the same for High Calories, High Protein, High Sugar, and High Fat. How many foods have both high sodium and high fat? (8 points)

# HighSodium
USDAclean$HighSodium[USDAclean$Sodium > mean(USDAclean$Sodium)] <- 1
USDAclean$HighSodium[USDAclean$Sodium <= mean(USDAclean$Sodium)] <- 0
# High Calories
USDAclean$HighCalories[USDAclean$Calories > mean(USDAclean$Calories)] <- 1
USDAclean$HighCalories[USDAclean$Calories <= mean(USDAclean$Calories)] <- 0
# High Protein
USDAclean$HighProtein[USDAclean$Protein > mean(USDAclean$Protein)] <- 1
USDAclean$HighProtein[USDAclean$Protein <= mean(USDAclean$Protein)] <- 0
# High Sugar
USDAclean$HighSugar[USDAclean$Sugar > mean(USDAclean$Sugar)] <- 1
USDAclean$HighSugar[USDAclean$Sugar <= mean(USDAclean$Sugar)] <- 0
# High Fat
USDAclean$HighTotalFat[USDAclean$TotalFat > mean(USDAclean$TotalFat)] <- 1
USDAclean$HighTotalFat[USDAclean$TotalFat <= mean(USDAclean$TotalFat)] <- 0
# Foods have High Sodium and High Fat"
# How many foods have both high sodium and high fat?
sum((USDAclean$HighSodium == 1) & (USDAclean$HighTotalFat == 1))
unique(USDAclean$HighTotalFat)


#### 12. Calculate the average amount of iron, for high and low protein foods. (8 points)

# average amount of iron, for high in foods
mean(USDAclean[USDAclean$HighProtein == 1,]$Iron)
# average amount of iron, for  low protein foods
mean(USDAclean[USDAclean$HighProtein == 0,]$Iron)


#### 13. Create a script for a "HealthCheck" program to detect unhealthy foods. Use the algorithm flowchart below as a basis for this script. (8 points)

install.packages("jpeg")
library(jpeg)
require(jpeg)
img<-readJPEG("HealthCheck.jpg")
plot(1:4, ty = 'n', ann = F, xaxt = 'n', yaxt = 'n')
rasterImage(img,1,1,4,4)




HealthCheck <- function(sodium, sugar, fat){
  ifelse (sodium==0, "Pass", ifelse (sugar==0, "Pass", ifelse
                                     (fat==0, "Pass", "Fail")))
}


#### 14. Add a new variable called HealthCheck to the data frame using the output of the function. (8 points)

USDAclean$HealthCheck <- HealthCheck(USDAclean$HighSodium, 
                                     USDAclean$HighSugar, USDAclean$HighTotalFat)


#### 15. How many foods in the USDAclean data frame fail the HealthCheck? (8 points)

sum(USDAclean$HealthCheck=="Fail")


#### 16. Save your final data frame as "USDAclean_ [your last name]." (3 points)

USDAclean_seharawat <- USDAclean
USDAclean_seharawat <- write.csv(USDAclean, file =
                                   "USDAclean_seharawat.csv")


#Find Q1, Q3 and Interquartile range for a values in TotalFat

Q1 <- quantile(USDAclean$TotalFat, 0.25)
Q1
Q3 <- quantile(USDAclean$TotalFat, 0.75)
Q3

IQR <- IQR(USDAclean$TotalFat)
IQR

Q3-Q1

no_outliers <- subset(USDAclean, USDAclean$TotalFat > (Q1 - 1.5*IQR) & USDAclean$TotalFat < (Q3 + 1.5*IQR))
dim(no_outliers)

#Find Q1, Q3 and Interquartile range for a values in Potassium

PotassiumQ1 <- quantile(USDAclean$Potassium, 0.25)
PotassiumQ1
PotassiumQ3 <- quantile(USDAclean$Potassium, 0.75)
PotassiumQ3

PotassiumIQR <- IQR(USDAclean$Potassium)
PotassiumIQR

PotassiumQ3-PotassiumQ1

no_outliers_Potassium <- subset(USDAclean, (USDAclean$Potassium > (PotassiumQ1 - 1.5*PotassiumIQR)) & 
                                  ( USDAclean$Potassium < (PotassiumQ3 + 1.5*PotassiumIQR)))
dim(no_outliers_Potassium)

boxplot(no_outliers_Potassium$Potassium,
        col = 'yellow',
        horizontal = TRUE)

#Find Q1, Q3 and Interquartile range for a values in Protein

ProteinQ1 <- quantile(USDAclean$Protein, 0.25)
ProteinQ1
ProteinQ3 <- quantile(USDAclean$Protein, 0.75)
ProteinQ3

ProteinIQR <- IQR(USDAclean$Protein)
ProteinIQR

ProteinQ3-ProteinQ1

no_outliers_Protein <- subset(USDAclean, USDAclean$Protein > (ProteinQ1 - 1.5*ProteinIQR) & USDAclean$Protein < (ProteinQ3 + 1.5*ProteinIQR))
dim(no_outliers_Protein)

boxplot(no_outliers_Protein$Protein,
        col = 'purple',
        horizontal = TRUE,
        outline = TRUE)

#Find Q1, Q3 and Interquartile range for a values in Carbohydrate

CarbohydrateQ1 <- quantile(USDAclean$Carbohydrate, 0.25)
CarbohydrateQ1
CarbohydrateQ3 <- quantile(USDAclean$Carbohydrate, 0.75)
CarbohydrateQ3

CarbohydrateIQR <- IQR(USDAclean$Carbohydrate)
CarbohydrateIQR

CarbohydrateQ3-CarbohydrateQ1

no_outliers_Carbohydrate <- subset(USDAclean, USDAclean$Carbohydrate > (CarbohydrateQ1 - 1.5*CarbohydrateIQR) & USDAclean$Carbohydrate < (CarbohydrateQ3 + 1.5*CarbohydrateIQR))
dim(no_outliers_Carbohydrate)

boxplot(no_outliers_Protein$Carbohydrate,
        col = 'cyan',
        horizontal = TRUE,
        outline = TRUE)

This is the end of Assignment 1


########################
# TASK - Check NA, Replace NA with Mean, Check Outlier, Remove Outlier using IQR & Z-Score

View(iris)

str(iris)

sum(is.na(iris$Sepal.Length))
sum(is.na(iris$Sepal.Width))
sum(is.na(iris$Petal.Length))
sum(is.na(iris$Petal.Width))

boxplot(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width,
        col = rainbow(4),
        horizontal = TRUE)

SW_Q1 = quantile(iris$Sepal.Width, 0.25)
SW_Q1
SW_Q3 = quantile(iris$Sepal.Width, 0.75)
SW_Q3
SW_IQR = IQR(iris$Sepal.Width)
SW_IQR

SW_Q3 - SW_Q1

no_outliers_SW <- subset(iris, iris$Sepal.Width > (SW_Q1 - 1.5*SW_IQR) & iris$Sepal.Width < (SW_Q3 + 1.5*SW_IQR))
dim(no_outliers_SW)

boxplot(no_outliers_SW$Sepal.Width,
        col = 'green',
        horizontal = TRUE)

# Using z-score
df = iris[-5]
View(df)
z_scores <- as.data.frame(sapply(df, function(df) (abs(df-mean(df))/sd(df))))
head(z_scores)
iris_no_outliers <- z_scores[!rowSums(z_scores>3),]
iris_no_outliers

dim(iris_no_outliers)
boxplot(iris_no_outliers$Sepal.Length, iris_no_outliers$Sepal.Width, iris_no_outliers$Petal.Length, iris_no_outliers$Petal.Width,
        col = rainbow(4),
        horizontal = TRUE)
