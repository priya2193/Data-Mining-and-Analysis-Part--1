#################
##Fisrt R Program 09/18/2017
#################


rm(list= ls())

#Set Working Directory
setwd("/Users/priyamurthy/Documents/R progamming STA545")

#install.packages('RMySQL', repos='http://cran.us.r-project.org')


#install some packages
#install.packages("DAAG")
#install.packages("lattice")
#	install.packages("MASS")

#library("DAAG")
#library("lattice")
#library("MASS")


source("https://bioconductor.org/biocLite.R")
biocLite("geneplotter")


########################
##Histograms and Density Plots
########################

#install.packages("ISLR")
#install.packages("dplyr")
library("ISLR")
library(dplyr)
?auto
dim(Auto)
names(Auto)

write.table(Auto, file= "Auto_data.txt", sep = "\t", col.names = names(Auto))
#temp <- read.delim("Auto_data.txt", sep = "\t", header = TRUE)
temp1 <- saveRDS(temp, "Auto_data.rds")
temp1 <- readRDS("Auto_data.rds")
## PLOTTING HISTOGRAMS
par(mfrow = c(1,4))
hist(Auto$mpg, main = "Histogram - ISLR - Auto" , xlab = "Miles per gallon")


##Density Function
dens <- density(Auto$mpg)
xlim <- range(dens$x)
ylim <- range(dens$y)
quartz()
hist(Auto$mpg, probability = T, xlim = xlim , ylim = ylim , main = "Density Function - ISLR - Auto" , xlab = "Miles per gallon")
lines(dens)

saveeps(density_plots)
	
##Stem and Leaf Plots
stem(Auto$mpg)

#BoxPlot

boxplot(Auto$mpg~Auto$year, main = "BoxPlot - ISLR - Mpg- Year")
boxplot(Auto$mpg~Auto$origin, main = "BoxPlot - ISLR - Mpg- Origin")
boxplot(Auto$mpg~Auto$cylinders, main = "BoxPlot - ISLR - Mpg vs Cylinders")


#Cleaning Data
CleanData = as.data.frame(Auto)
CleanData <- subset(Auto, Auto$mpg < 38)
boxplot(CleanData$mpg~CleanData$origin, main = "BoxPlot - ISLR - Mpg- Origin")

CleanData <- subset(CleanData, !(CleanData$year == 78 & CleanData$mpg > 40))
boxplot(CleanData$mpg~CleanData$year, main = "BoxPlot - ISLR - Mpg- Year")


CleanData <- subset(CleanData, !(CleanData$cylinders == 4 & CleanData$mpg > 40))

CleanData <- subset(CleanData, !(CleanData$cylinders == 6 & CleanData$mpg >= 25))
CleanData <- subset(CleanData, !(CleanData$cylinders == 8 & CleanData$mpg > 20))
boxplot(CleanData$mpg~CleanData$cylinders, main = "BoxPlot - ISLR - Mpg- Cylinders")


#############
#Patterns in Bivariate data
#############
#Scatter Plot
############

?attach
#Weight vs mpg
attach(Auto)
plot(weight, mpg, main="Scatterplot - ISLR - Auto - wt vs mpg",xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)

#CleanData <- subset(CleanData, CleanData$mpg < 46)

plot(CleanData$weight,CleanData$mpg, main="Scatterplot - ISLR - Auto - wt vs mpg",xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)

# Cylinders vs mpg
attach(Auto)
plot(cylinders, mpg, main="Scatterplot - ISLR - Auto - cylinders vs mpg",xlab="Cylinders ", ylab="Miles Per Gallon ", pch=19)

# Displacement vs mpg
attach(Auto)
plot(displacement, mpg, main="Scatterplot - ISLR - Auto - Displacement vs mpg",xlab="Displacement ", ylab="Miles Per Gallon ", pch=19)
attach(CleanData)
plot(displacement, mpg, main="Scatterplot - ISLR - Auto - Displacement vs mpg",xlab="Displacement ", ylab="Miles Per Gallon ", pch=19)

# Horsepower vs mpg
attach(Auto)
plot(horsepower, mpg, main="Scatterplot - ISLR - Auto - Horsepower vs mpg",xlab="Horsepower ", ylab="Miles Per Gallon ", pch=19)

# Acceleration vs mpg
attach(Auto)
plot(acceleration, mpg, main="Scatterplot - ISLR - Auto - Acceleration vs mpg",xlab="Acceleration", ylab="Miles Per Gallon ", pch=19)

# Year vs mpg
attach(Auto)
plot(year, mpg, main="Scatterplot - ISLR - Auto - Year vs mpg",xlab="Year", ylab="Miles Per Gallon ", pch=19)

# Origin vs mpg
attach(Auto)
plot(origin, mpg, main="Scatterplot - ISLR - Auto - Origin vs mpg",xlab="Origin", ylab="Miles Per Gallon ", pch=19)

# Name vs mpg
attach(Auto)
plot(name, mpg, main="Scatterplot - ISLR - Auto - Name vs mpg",xlab="Name ", ylab="Miles Per Gallon ", pch=19)

#Removing col = names from dataset
CleanData = subset(CleanData, select = -c(name))
names(CleanData)
###Scatter Plot Matrix
pairs(~mpg+cylinders+displacement+horsepower+weight+acceleration+year+origin+name,data = Auto,
   main = "Scatterplot Matrix")

plot(~.,data = CleanData)


##Saving as .RData file
write.table(CleanData, file= "Auto_CleanData.txt", sep = "\t", col.names = names(CleanData))
save(CleanData, file= "Auto_Clean_Data.txt")
save(CleanData, file = "Auto_CleanData.rdata")


?save
