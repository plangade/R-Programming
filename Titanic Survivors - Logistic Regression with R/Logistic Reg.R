#Exploring titanic data with logistic regression

df.train <- read.csv('titanic_train.csv')
head(df.train)
str(df.train)

# import library Amelia to check missingmap
suppressWarnings(suppressMessages(library(Amelia)))

missmap(df.train, main="Missing Map", col=c('yellow','black') ,legend=FALSE)

#explore data through visualization
suppressWarnings(suppressMessages(library(ggplot2)))
ggplot(df.train,aes(Survived))+geom_bar()
ggplot(df.train,aes(Pclass))+geom_bar(aes(fill=factor(Pclass)))
ggplot(df.train,aes(Sex))+geom_bar(aes(fill=factor(Sex)))
ggplot(df.train,aes(Age))+geom_histogram(bins=20,alpha=0.5,fill='blue')
ggplot(df.train, aes(SibSp))+geom_bar()
ggplot(df.train,aes(Fare))+geom_histogram(fill='green',color='black', alpha=0.5)

#data cleaning for NA or NULL values
pl <- ggplot(df.train,aes(Pclass,Age))
pl <- pl + geom_boxplot(aes(group=Pclass,fill=factor(Pclass), alpha=0.4))
print(pl)
pl <- pl+scale_y_continuous(breaks=seq(min(0),max(80),by=2))
print(pl)

#imputation of Age based on class
impute_age <- function(age, class){
  out <- age
  for(i in 1:length(age)){
    if(is.na(age[i])){
      if(1==class[i]){
        out[i] <- 37
      }else if(2 == class[i]){
        out[i] <- 29
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

fixed.ages <- impute_age(df.train$Age,df.train$Pclass)

df.train$Age <- fixed.ages

missmap(df.train, main="Imputation Check", col=c('yellow','black'),legend=FALSE)


suppressWarnings(suppressMessages(library(dplyr)))
df.train <- select(df.train, -PassengerId, -Name,-Ticket,-Cabin)
head(df.train)

df.train$Survived <- factor(df.train$Survived)
df.train$SibSp <- factor(df.train$SibSp)
df.train$Parch <- factor(df.train$Parch)
df.train$Pclass <- factor(df.train$Pclass)

str(df.train)

#build model
log.model <- glm(Survived ~ ., family=binomial(link = 'logit'), data=df.train)
summary(log.model)


#predict on same data
suppressWarnings(suppressMessages(library(caTools)))
set.seed(101)
split <- sample.split(df.train$Survived,SplitRatio = 0.7)
final.train <- subset(df.train,split == TRUE)
final.test <- subset(df.train, split == FALSE)
final.log.model <- glm(final.train$Survived ~., family = binomial(link='logit'), data=final.train)
summary(final.log.model)

fitted.probabilities<-predict(final.log.model, final.test, type='response')
fitted.results <-ifelse( fitted.probabilities>0.5, 1,0)
misclassError <- mean(fitted.results != final.test$Survived)
accuracy <- 1-misclassError
print(accuracy)
table(final.test$Survived, fitted.probabilities>0.5)
#create confusion matrix



#predict model on test data
#read test data 
df.test <- read.csv('titanic_test.csv')
head(df.test)
str(df.test)
missmap(df.test,main = 'Missing Test Data', col=c('yellow', 'black'),legend=FALSE)

#clean test data
fixed.test.age <- impute_age(df.test$Age,df.test$Pclass)
df.test$Age <- fixed.test.age
missmap(df.test,main = 'Fixed Test Data', col=c('yellow', 'black'),legend=FALSE)

df.test <- select(df.test,-PassengerId,-Name,-Ticket,-Cabin)
df.test$SibSp <- factor(df.test$SibSp)
df.test<-df.test[!(df.test$Parch==9),] # temporary fix

df.test$Parch <- factor(df.test$Parch)
df.test$Pclass <- factor(df.test$Pclass)

test.probabilities <- predict(log.model,df.test, type='response')
test.results <- ifelse(test.probabilities>0.5,1,0)
df.test$Survived <- test.results

  