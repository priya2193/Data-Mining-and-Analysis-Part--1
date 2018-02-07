##########################
##Qn3 :Compare the classification performance of linear regression and k-nearest neighbor classification on the zipcode data. 
#####################

rm(list = ls())
##Set Working Directory
setwd("/Users/priyamurthy/Documents/R progamming STA545")

##Load Library
#install.packages("ElemStatLearn")
    
#install.packages("ggplot2")
library("ggplot2")

##Write dataset into zip_train from zip.train
write.table(zip.train, file= "zip_train_data.txt", sep = "\t")
zip_train <- read.delim("zip_train_data.txt", sep = "\t", header = TRUE)
zip_train <- subset(zip_train, zip_train[,1]==2 | zip_train[,1]==3);
zip_train = as.data.frame(zip_train)

##Write dataset into zip_test from zip.test
write.table(zip.test, file= "zip_test_data.txt", sep = "\t")
zip_test <- read.delim("zip_test_data.txt", sep = "\t", header = TRUE)
zip_test <- subset(zip_test, zip_test[,1]==2 | zip_test[,1]==3);
zip_test = as.data.frame(zip_test)
print(zip_test)
head(zip_test)


##Linear Regresssion using lm() funciton
result_V1 <- lm( V1 ~ . , data= zip_train); 
result_sum = summary(result_V1)
print(result_sum)
a <-  mean(result_sum$residuals^2)
accuracy <- (1 - a)*100
result_sum$coefficients
result_sum$adj.r.squared
##Computer confidence interval
confint(result_V1)

pred <- predict.lm(result_V1, zip_test);
pred_res <-summary(pred)
print(summary(pred))
#a1 <-  mean(pred_res$residuals^2)
accuracy <- (1 - a)*100
print(pred)
table(pred)


#pred <- predict.lm(result_V1, zip_train);

#####
# Compute a Confidence interval for specific values of the 	predictors....
#predict(fit, newdata = data.frame(lstat = c(5,10,15)))

#predict(fit, newdata = data.frame(lstat = c(5,10,15)), interval = "confidence")


##KNN
################################
## Try different values of k
################################
require(class)
knnplot <- function(train, test,k){
	 KNN <- knn(train, test, train$V1, k)
	 return(KNN)
}
###############################################
## Try differnt values of k, and save
###############################################
#filer <- paste("k", c(1,3,5,7,9,11, 13,15), ".png", sep="")
k=1
error_test = 0
error_train =0
    for (i in c(1,3,5,7,9,11, 13,15)){
      #quartz()
      test_predict <- knnplot(zip_train,zip_test, i)
      train_predict <- knnplot(zip_train,zip_train,i)
      
      #converting to data frame
      test_predict_df <- as.data.frame(test_predict)
      train_predict_df <- as.data.frame(train_predict)
      
      knn_test <- test_predict_df$test_predict_df[1:364]
      knn_train <- train_predict_df$train_predict_df[1:1389]
      
      ##Computing training and test error
      error_test[k] <- sum(zip_test$V1!= test_predict_df)/nrow(zip_test)
      error_train[k] <- sum(zip_train$V1!= train_predict_df)/nrow(zip_train)
      Accuracy_Test = (1- error_test[k])*100
      #print(Accuracy_Test)
      
      Accuracy_Train = (1- error_train[k])*100
      print(Accuracy_Train)
      
        k=k+1;
      #ggsave(filename = filer[i], plot = p, height = 5, width = 5)
    }


print(error_test)
print(error_train)
