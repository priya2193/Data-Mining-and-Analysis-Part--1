### lm function - creates relationship between the predictor and response variable
## mpg of Auto data set is the response variable 
result<-lm(Auto$mpg~. -name,data=Auto)
print(summary(result))
