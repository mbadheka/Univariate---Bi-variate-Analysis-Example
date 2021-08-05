## This individual project is based on real-world analysis of Singapore Airbnb collected 
## on 29 August 2019 downloaded from kaggle
## For discovering important information, informing conclusions and supporting decision making.


cat("\14") ## Clear Console

rm(list = ls()) ## Clear Memory

setwd("C:/Users/malhar/Desktop/IMAN") ## Set working directory


##-------------------------------------------------------
##TASK 1: Data Collection
##-------------------------------------------------------
## The dataset used in this Project is downloaded from Kaggle website.
##  file was gotten from insideairbnb.com to kaggle 
## "listing.csv " contains data in various formats like Text and Numbers.
## We have downloaded the data for 7907 sales record.


## reading table

t<-read.csv("book1.csv",na.strings = "")

##---------------------------------------------
##TASK 2: Data Cleaning and Transformation
##---------------------------------------------
names(t)<-c("id","name of hotel","host_id","host_name","neighbourhood_group","neighbourhood","latitude","longitude","room_type","price","minimum_nights","num_reviewer","last_review_date","review_p/m","avaiblity")

## Handling Missing Data
t<-na.omit(t)


##---------------------------------------------
##TASK 3: Univariate Analysis
##---------------------------------------------
## 1. Frequency 
table(t$`neighbourhood_group`)
table(t$`neighbourhood`)

# table(t$`review_p/m`)

## 2. percentage table
x1<-100*(table(t$`neighbourhood_group`)/sum(table(t$`neighbourhood_group`)))


## 3. Location (mean, median, etc.)
## Mean
mean(as.integer(t$`price`))
mean(as.integer(t$`num_reviewer`))

## Medain 
median(as.integer(t$`price`))
median(as.integer(t$`num_reviewer`))


## 4.	Spread (minimum, maximum, range, quartiles, standard deviation, etc.)

## min values
min(as.integer(t$`price`))
min(as.integer(t$`num_reviewer`))

max(as.integer(t$`price`))
max(as.integer(t$`num_reviewer`))

range(as.integer(t$`price`))
range(as.integer(t$`num_reviewer`))

quantile(t$`price`)
quantile(t$`num_reviewer`)

var(as.integer(t$`price`))
var(as.integer(t$`num_reviewer`))

sd(as.integer(t$`price`))
sd(as.integer(t$`num_reviewer`))


## 5.	Shape (symmetry, sharpness, etc.)
## density function

par(mfrow = c(2,2), mar=c(4, 4, 2,1))
plot(density(t$price), xlab = "price" , main= "Density function of price")
plot(density(t$num_reviewer), xlab = "num_reviewer" , main= "Density function of num_reviewer")

## Install Package e1071 for skewness and kurtosis
install.packages("moments")
library(moments)


skewness(as.integer(t$`price`))
skewness(as.integer(t$`num_reviewer`))

kurtosis(as.integer(t$`price`))
kurtosis(as.integer(t$`num_reviewer`))

## 6.	Bar chart
par(mfrow = c(2,1), mar=c(4, 4, 2, 1))
barplot(t$`price`,col = "red", xlab = "price", ylab = "Frequency",xlim = c(0,500),main = "Barplot of price")
barplot(t$`num_reviewer`,col = "red", xlab = "num_reviewer", ylab = "Frequency",main = "Barplot of num_reviewer")


## 7. histogram 
par(mfrow = c(2,1), mar=c(4, 4, 2, 1))
hist(t$`price`,col = "Blue", xlab = "price", ylab = "Frequency", main = "histogram of price")
hist(t$`num_reviewer`,col = "Blue", xlab = "num_reviewer", ylab = "Frequency",main = "histogram of num_reviewer")

## 8 pie chart

par(mfrow = c(1,2))

pie(x1,main = "neighbourhood_group",init.angle = 100,radius = 0.8, cex = 0.45)
pie(table(t$room_type),main = "neighbourhood_group",init.angle = 100,radius = 0.8, cex = 0.45)


## 9.boxplots
par(mfrow = c(2,1), mar=c(4, 4, 2, 1))
boxplot(t$`price`,col = "Blue", xlab = "minimum_nights", ylab = "Frequency", main = "box plot of minimum_nights", outline = FALSE)
boxplot(t$`num_reviewer`,col = "Blue", xlab = "num_reviewer", ylab = "Frequency",main = "boxplot of num_reviewer",outline = FALSE)

##-------------------------------------
##TASK 4: Bivariate Analysis
##-------------------------------------

p<-rnorm(length(t$price),mean = 127.1123,sd=131.1054)
r<-rnorm(length(t$num_reviewer),mean =39.97397, sd= 55.48952)

## 1. Frequency
table(t$neighbourhood_group, t$`neighbourhood`) 

## 2. Percentage
100*(table(t$neighbourhood_group, t$`neighbourhood`)/sum(table(t$neighbourhood_group, t$`neighbourhood`)))


## 3. Correlation
cor(t$`price`, t$`num_reviewer`)
cor(t$`num_reviewer`, t$`review_p/m`)

## 4. covarience 
cov(t$`price`, t$`num_reviewer`)
cov(t$`num_reviewer`, t$`review_p/m`)

##scatter plot 
plot(t$price,t$num_reviewer,xlim = c(0,500),xlab = "price",ylab = "num_reviewer")
plot(t$price,r,xlim = c(0,500) ,ylim = c(0,150),xlab = "price",ylab = "num_reviewer")


## 5. Linear Regression
##calculating linear model

par(mfrow = c(2, 1))
model<-lm(p ~ t$num_reviewer, data=t)
plot(t$num_reviewer ,model$fitted.values) #t$num_reviewer,


##plotting linear model
model <- lm(p ~ poly(t$num_reviewer))
plot(t$num_reviewer,model$fitted.values)
lines(t$num_reviewer,model$fitted.values)

## 7. Bar plot based on Item Type

par(mfrow = c(1, 2))
barplot(table( p,  as.integer(t$`num_reviewer`)),xlab = "price", main = "num_reviewer", col = "black")

## 8. Scatter plot for price and reviewer 

par(mfrow = c(1,3), mar=c(4, 4, 2,1))
model<-lm(r ~ t$`num_reviewer`, data=t)
plot(r,t$num_reviewer, xlab="price", ylab="num_reviewer",xlim = c(0,140),ylim = c(0,140) ,col=c("red", "green"))
lines(t$num_reviewer,model$fitted.values, col=c("black"))


