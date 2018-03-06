titanic<-read.csv('titanic.csv')
dim(titanic)
meanage<-sum(na.omit(titanic$Age))/length(na.omit(titanic$Age))
titanic$Age[is.na(titanic$Age)]<-meanage
titanic$Age<-round(titanic$Age)
#Create a new variable (vector) called Age Category and assign a category to every passenger in the dataset.
titanic$AgeCat[titanic$Age>=0&titanic$Age<=16]<-"0-16"
titanic$AgeCat[titanic$Age>=17&titanic$Age<=32]<-"17-32"
titanic$AgeCat[titanic$Age>=33&titanic$Age<=48]<-"33-48"
titanic$AgeCat[titanic$Age>=49&titanic$Age<=64]<-"49-64"
titanic$AgeCat[titanic$Age>=65]<-"65 and Above"
#Replace the integer value of 0 and 1 in the survivor variable (vector) with a meaningful labels
titanic$Survived[titanic$Survived==0]<-"Not Survived"
titanic$Survived[titanic$Survived==1]<-"Survived"
#Convert the integer and character vectors to factor variables 
titanic$Pclass<-factor(titanic$Pclass)
titanic$AgeCat<-factor(titanic$AgeCat)
titanic$Survived<-factor(titanic$Survived)
titanic$Embarked<-as.character(titanic$Embarked)
titanic$Embarked[titanic$Embarked=="S"]<-"Southammpton"
titanic$Embarked[titanic$Embarked=="C"]<-"Cherbourg"
titanic$Embarked[titanic$Embarked=="Q"]<-"Queenstown"
titanic$Embarked<-factor(titanic$Embarked)
#Remove the other redundant variables such as Ticket and Cabin from the titanic data frame
titanic<-titanic[c(-9,-11)]
View(titanic)
write.csv (titanic, file = "titanicNew.csv")

######################## DECISION TREE #######################

#Decision tree works best for the categorical variables hence we will convert two variables SibSp and Parch into categorical variables and add the variables (vectors) to the data.
decision_tree<-titanic
SibSpCat= ifelse(decision_tree$SibSp >= 3, ">=3","<3")
decision_tree<-data.frame(decision_tree,SibSpCat)
decision_tree$SibSpCat<-as.factor(decision_tree$SibSpCat)
ParchCat= ifelse(decision_tree$Parch >= 3, ">=3","<3")
decision_tree<-data.frame(decision_tree,ParchCat)
decision_tree$ParchCat<-as.factor(decision_tree$ParchCat)

#separate data into “training data” and “testing data”.
set.seed(1)
test = sample(1:nrow(decision_tree),nrow(decision_tree)/3)
train = -test
training_data = decision_tree[train,]
testing_data = decision_tree[test,]
testing_survived = decision_tree$Survived[test]

#To build decision tree install rpart and rattle package

library(RGtk2)
library(RGtk2Extras)
library(RColorBrewer)
library(rpart.plot)
library(rpart)
library(rattle)

#build the decison tree
tree_model=rpart(Survived~Pclass + Sex + AgeCat + Embarked + SibSpCat + ParchCat,
                 data = training_data, method="class", control=rpart.control(minsplit = 10,cp=0.00))
fancyRpartPlot(tree_model,sub="decision_tree")
tree_predict=predict(tree_model,testing_data,type="class")

mean(tree_predict != testing_survived)


###test code to interactively trim nodes from decision tree
fit <- rpart(Survived ~ Pclass + Sex + AgeCat + SibSpCat + ParchCat + Fare + Embarked,
             data=training_data,
             method="class",
             control=rpart.control( minsplit = 5, cp=0.00 ))
fancyRpartPlot(fit,sub="decision_tree")
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)

###########################  K Means Clustering #################################

#clustering requires continuous values, to convert the above 3 categorical variables into continuous

titanicNew<-read.csv("titanicNew.csv")
titanicUpdated<-titanicNew

SurvivedNum<-ifelse(titanicUpdated$Survived=="Not Survived",0,1)
titanicUpdated <-data.frame(titanicUpdated,SurvivedNum)
SexN<-ifelse(titanicUpdated $Sex=="male",1,0)
titanicUpdated <-data.frame(titanicUpdated, SexN)
EmbarkedN<-ifelse(titanicUpdated$Embarked=="Southampton",1,ifelse(titanicUpdated $Embarked=="Cherbourg",2,0))
titanicUpdated <-data.frame(titanicUpdated, EmbarkedN)

write.csv(titanicUpdated,file = "titanicUpdated.csv")


titanic.scaled<-scale(data.frame(titanic$Age,titanic$Parch,titanic$SibSp,titanic$Fare))
colnames(titanic.scaled)
totwss<-vector()
btwss<-vector()
for(i in 2:15)
{
  set.seed(1234)
  temp<-kmeans(titanic.scaled,centers = i)
  totwss[i]<-temp$tot.withinss
  btwss[i]<-temp$betweenss
}
plot(totwss,xlab="number of clusters", type = "b", ylab = "Total within sum of square")
plot(btwss,xlab="number of clusters", type = "b", ylab = "Total between sum of square")
