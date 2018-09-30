library(ISLR)
str(Caravan)
head(Caravan)
summary(Caravan$Purchase)

#check if missing values present
any(is.na(Caravan))


print(var(Caravan[,1]))
print(var(Caravan[,2]))

#separate purchase variable
purchase <- Caravan$Purchase


#Standardize data because of varied variance
Standardized.Caravan <- scale(Caravan[,-86])

print(var(Standardized.Caravan[,1]))
print(var(Standardized.Caravan[,2]))

#split train and test data by simple indexing 
test.index <- 1:1000
test.data <- Standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]
train.data <- Standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]

###############
#KNN MODEL

library(class)
set.seed(101)
predicted.purchase <- knn(train.data,test.data,train.purchase,k=5)
print(head(predicted.purchase))

misclass.err <- mean(test.purchase != predicted.purchase)
print(misclass.err)

#####
#Choosing A K value

predicted.purchase <- NULL
error.rate <- NULL

for(i in 1:20){
  set.seed(101)
  predicted.purchase <- knn(train.data,test.data,train.purchase,k=i)
  error.rate[i] <- mean(test.purchase != predicted.purchase)
  
}

print(error.rate)

######
## Visulaize K ELBOW Method
######

library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate,k.values)

print(error.df)

ggplot(error.df,aes(x=k.values,y=error.rate))+geom_point()+geom_line(color='red',lty='dotted')
 
