suppressWarnings(suppressMessages(library(caTools)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(Amelia)))


# Read File and check head, target variable is income
adult <- read.csv("adult_sal.csv")
head(adult)
adult <- select(adult,-X)
str(adult)
summary(adult)

# Data Cleaning and Feature Engineering
table(adult$type_employer) # to get frequency of each 

#group employer type for unemployed, gov, self-emp
group_emp <- function(job){
  job<- as.character(job)
  if(job=='Never-worked'|job=='Without-pay'){
    return('unemployed')
  }else if(job=='Self-emp-inc'|job=='Self-emp-not-inc'){
    return('Self-emp')
  }else if(job == 'Local-gov'|job == 'State-gov'){
    return('SL-gov')
  }else{
    return(job)
  }
}

adult$type_employer <-sapply(adult$type_employer,group_emp)
table(adult$type_employer)

# feature engineering for Marital Status
table(adult$marital)
# Reduce this to three groups:
  #Married
  #Not-Married
  #Never-Married
group_marital <- function(mar){
  mar <- as.character(mar)
  if(mar=='Divorced'|mar == 'Separated' | mar == 'Widowed'){
    return('Not-married')
  }else if(mar == 'Never-married'){
    return('Never-married')
  }else{
    return('Married')
  }
}

adult$marital <- sapply(adult$marital,group_marital)
table(adult$marital)

# Country column data clean
table(adult$country)
# Grouping the countries according to continent
levels(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctr){
  ctr <- as.character(ctr)
  if(ctr %in% Asia){
    return('Asia')
  }else if(ctr %in% North.America){
    return('North.America')
  }else if(ctr %in% Europe){
    return('Europe')
  }else if( ctr %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')
  }
}

adult$country <- sapply(adult$country,group_country)
table(adult$country)

# check education
table(adult$education)

group_education <- function(edu){
  edu <- as.character(edu)
  if(edu == '1st-4th'| edu == '5th-6th' | edu == '7th-8th' | edu == '9th' | edu == '10th'){
    return('Elementary-Middle-School')  
  }else if(edu == '11th'| edu == '12th'){
    return('High-School')
  }else{
    return(edu)
  }
}

adult$education <- sapply(adult$education,group_education)
table(adult$education)

# lets check missing data
adult[adult=='?']<-NA

# checking the structure of data frame again to verify 
str(adult)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$marital <- sapply(adult$marital,factor)
adult$country <- sapply(adult$country,factor)
adult$education <- factor(adult$education)
str(adult)


missmap(adult,y.at=c(1),y.labels=c(''),col=c('yellow','black'))
# Drop Missing Data
adult<-na.omit(adult)
missmap(adult,y.at=c(1),y.labels=c(''),col=c('yellow','black'))


# Exploratory Data Analysis
ggplot(adult,aes(age))+geom_histogram(aes(fill=income),color='black',binwidth=1)+theme_bw()
ggplot(adult,aes(hr_per_week))+geom_histogram(color='black')+theme_bw()


adult<-rename(adult,region=country)
head(adult)

ggplot(adult, aes(region))+geom_bar(aes(fill=income),color='black')+theme_bw()


###############################
### LOGISTIC REGRESSION MODEL #
###############################

#Train-test split 
set.seed(101)
sample <- sample.split(adult$income,SplitRatio = 0.7)
#Train
train<-subset(adult,sample==T)
#test
test<-subset(adult,sample==F)

model <- glm(income ~ ., family=binomial(link='logit'), data=train)
summary(model)

new.step.model <-step(model)
summary(new.step.model)

test$predicted.income <- predict(model, newdata = test, type='response')
table(test$income,test$predicted.income>0.5)


accuracy <- (6372+1423)/(6372+1423+872+548)
accuracy
recall <- 6372/(6372+548)
recall
precision <- 6372/(6372+872)
precision
