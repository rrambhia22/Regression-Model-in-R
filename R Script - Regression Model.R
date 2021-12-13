#MODULE 5 - R PRACTICE


#installing the packages
install.packages("broom")
install.packages("corrplot")
install.packages("gtsummary")


#importing libraries
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(scales)
library(psych)
library(MASS)
library(ggpubr)
library(broom)
library(corrplot)
library(gtsummary)


#reading csv file
dataset_fish <- read.csv("Fish.csv")
dataset_fish


#describing the dataset
colnames(dataset_fish)

start_records <- head(dataset_fish,10)
start_records

end_records <- tail(dataset_fish,10)
end_records

dim(dataset_fish)

str(dataset_fish)

summary_dataset <- summary(dataset_fish)
summary_dataset

classtype <- sapply(dataset_fish, class)
classtype



#descriptive analysis

#weight
min(dataset_fish$Weight)
max(dataset_fish$Weight)
mean(dataset_fish$Weight)
median(dataset_fish$Weight)
mode(dataset_fish$Weight)
sd(dataset_fish$Weight)
range(dataset_fish$Weight)

#height
min(dataset_fish$Height)
max(dataset_fish$Height)
mean(dataset_fish$Height)
median(dataset_fish$Height)
mode(dataset_fish$Height)
sd(dataset_fish$Height)
range(dataset_fish$Height)

#width
min(dataset_fish$Width)
max(dataset_fish$Width)
mean(dataset_fish$Width)
median(dataset_fish$Width)
mode(dataset_fish$Width)
sd(dataset_fish$Width)
range(dataset_fish$Width)



#frequency table
unique_species <- unique(dataset_fish$ï..Species)
unique_species

count_species <- count(dataset_fish$ï..Species)
count_species

table_species <- table(dataset_fish$ï..Species)
table_species



#creating new subset of dataset
dataset_fish_new <- data.frame(dataset_fish$Weight, dataset_fish$Length1, dataset_fish$Length2, dataset_fish$Length3, 
                               dataset_fish$Height, dataset_fish$Width)
dataset_fish_new



#visualizations

#graph 1 : Frequency of the Species
barplot(table_species, col = "darkblue", main = "Frequency of the Species", 
        xlab = "Species", ylab = "Frequency")


#graph 2 : Boxplot of attributes
par(mfrow=c(2,2))
boxplot(dataset_fish$Weight, col = "green", main = "Weight of the Fish")
boxplot(dataset_fish$Height, col = "yellow", main = "Height of the Fish")
boxplot(dataset_fish$Width, col = "orange", main = "Width of the Fish")


#graph 3 : Histogram of Weight of the Fish
hist (dataset_fish$Weight, 
      xlab ='Weight of the Fish', 
      ylab='Frequency', 
      col='cadetblue',
      main='Histogram of Weight of the Fish',
      col.main ="black")


#graph 4 : Histogram of Height of the Fish
hist (dataset_fish$Height, 
      xlab ='Height of the Fish', 
      ylab='Frequency', 
      col='lightblue',
      main='Histogram of Height of the Fish',
      col.main ="black")


#graph 5 : Histogram of Width of the Fish
hist (dataset_fish$Width, 
      xlab ='Width of the Fish', 
      ylab='Frequency', 
      col='blue',
      main='Histogram of Width of the Fish',
      col.main ="black")


#graph 6 : Density plot of the attributes
par(mfrow=c(2, 2)) 
plot(density(dataset_fish$Weight), main = "Density Plot : Weigth of the Fish", ylab = "Frequency")
polygon(density(dataset_fish$Weight), col = "lightblue")
plot(density(dataset_fish$Height), main = "Density Plot : Height of the Fish", ylab = "Frequency")
polygon(density(dataset_fish$Height), col = "pink")
plot(density(dataset_fish$Width), main = "Density Plot : Width of the Fish", ylab = "Frequency")
polygon(density(dataset_fish$Width), col = "orange")




#relationship between weight and height of the fish

#correlation
correlation_value1 <- cor(dataset_fish$Weight, dataset_fish$Height)
correlation_value1

#linear regression model & plot
linearregression_model1 <- lm(Weight ~ Height, data = dataset_fish)
linearregression_model1
summary(linearregression_model1)

ggplot(dataset_fish, aes(x=Height, y=Weight, fill=Height)) + geom_point(stat = "identity", col = "darkblue") + 
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE)+
  labs(
    x = "Height",
    title = "Relationship between Weight and Height of the Fish") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))



#relationship between weight and width of the fish

#correlation
correlation_value2 <- cor(dataset_fish$Weight, dataset_fish$Width)
correlation_value2

#linear regression model & plot
linearregression_model2 <- lm(Weight ~ Width, data = dataset_fish)
linearregression_model2
summary(linearregression_model2)


ggplot(dataset_fish, aes(x=Width, y=Weight, fill=Width)) + geom_point(stat = "identity", col = "darkblue") + 
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE)+
  labs(
    x = "Width",
    title = "Relationship between Weight and Width of the Fish") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))



#correlation table
dataset_fish_new.cor = cor(dataset_fish_new)
dataset_fish_new.cor

#correlation chart
corrplot(dataset_fish_new.cor)


#regression table 1
tbl_regression(linearregression_model1)

#regression table 2
tbl_regression(linearregression_model2)


