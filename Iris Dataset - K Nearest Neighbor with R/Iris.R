library(ISLR)

df <- iris
head(df)

# Scale the data
stand.features <- scale(df[1:4])
print(var(stand.features))

final.data <- cbind(stand.features,iris[5])

#Train test split data
library(caTools)
set.seed(101)
sample <- sample.split(final.data$Species,SplitRatio = 0.7)

train <- subset(final.data, sample==TRUE)
test <- subset(final.data, sample==FALSE)

##KNN
library(class)
predicted.species <- knn(train[1:4], test[1:4], train$Species,k=1)
print(predicted.species)

print(mean(test$Species != predicted.species))

#Choose a K value
predicted.species <- NULL
error.rate <- NULL

for(i in 1:10){
  set.seed(101)
  predicted.species <- knn(train[1:4], test[1:4], train$Species,k=i)
  error.rate[i] <- mean(test$Species != predicted.species)
}

# Plot this for elbow method
library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)

pl <- ggplot(error.df, aes(x=k.values, y=error.rate))+geom_point()
pl <- pl+geom_line(lty = 'dotted',color = 'red')
print(pl)
