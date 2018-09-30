library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)

df <- read.csv('student-mat.csv',sep=';')
head(df)

#Lets check summary
summary(df)

#check if any null value is present
any(is.na(df))

#check structure of data
str(df)

# lets check correlation between variables
#1. only for numeric columns
num.cols <- sapply(df,is.numeric)
#filter
cor.data <- cor(df[ ,num.cols])
print(cor.data)
#corrplot only takes numeric data
print(corrplot(cor.data, method='color'))

#exploring corrgram
corrgram(df, order=TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel=panel.txt)

print(ggplot(df,aes(x=G3))+geom_histogram(bins=20, alpha = 0.5, fill='blue'))


#Split data into train and test set
# to split data use caTools package
library(caTools)

#Set A Seed
set.seed(101)

#split up the sample
sample <- sample.split(df$G3,SplitRatio = 0.7)
#70% train data and 30% test data
train <- subset(df,sample==TRUE)
test <- subset(df, sample==FALSE)

#Train and build model
model <- lm(G3 ~ ., train) #use all features

#print residual 
res <- residuals(model)
class(res)
res <- as.data.frame(res)
head(res)
ggplot(res, aes(x=res))+geom_histogram(fill='blue',alpha=0.5)

plot(model) #this is to check various plots of model (advanced topic)
#Predictions
G3.predictions <- predict(model, test)

results <- cbind(G3.predictions,test$G3)
colnames(results)<-c('predicted','actual')
results <- as.data.frame(results)
print(head(results))

#take care of negative values
to_zero <- function(x){
  if(x<0){
    return (0)
  }
  else{
    return(x)
  }
}

#Apply zero function
results$predicted <- sapply(results$predicted,to_zero)

#MEAN squared error
mse <- mean((results$actual-results$predicted)^2)
print('MSE')
print(mse)

#RMSE
print('RMSE')
print(mse^0.5)

SSE <- sum ((results$predicted-results$actual)^2)
SST <- sum((mean(df$G3)-results$actual)^2)
R2 <- 1-SSE/SST
print('R2')
print(R2)
#Interpret the model
print(summary(model))
