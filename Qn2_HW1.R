
library("ISLR")
rm(list= ls())

#Set Working Directory
setwd("/Users/priyamurthy/Documents/R progamming STA545")

Clean_Dataset <-load("/Users/priyamurthy/Documents/R progamming STA545/Auto_CleanData.rdata")
head(CleanData)
result<-lm(CleanData$mpg~.,data=CleanData)
print(summary(result))


##Using * and : to fit linear regression models with interaction effects
attach(CleanData)
output = lm(mpg ~ displacement:weight, data =CleanData)
summary(output)

output1 = lm(mpg ~ displacement:cylinders+displacement:weight+acceleration:horsepower, data=CleanData)
summary(output1)

output2 = lm(mpg ~. -cylinders-acceleration+year:origin+displacement:weight+
             displacement:weight+acceleration:horsepower+acceleration:weight, data=CleanData)
summary(output2)

output3 = lm(mpg ~ (.)*(.), data=CleanData)
summary(output3)