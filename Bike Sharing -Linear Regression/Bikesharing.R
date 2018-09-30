library(ggplot2)
library(dplyr)

bike.share <- read.csv('bikeshare.csv')
head(bike.share)

# The target variable is count as I need to predict the total count of bikes rented during each hour
#Lets check scatter plot for count Vs temp

countVsTemp <- ggplot(bike.share, aes(x=temp,y=count))+geom_point(alpha=0.3, aes(color=temp))+theme_bw()
print(countVsTemp)

# Now lets check count Vs datetime with temp as color gradient.
# Converting datetime with POSIXct
bike.share$datetime <- as.POSIXct(bike.share$datetime)
head(bike.share)
countVsdatetime <- ggplot(bike.share,aes(x=datetime,y=count))+geom_point(alpha=0.3, aes(color=temp))
countVsdatetime <- countVsdatetime + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()
print(countVsdatetime)

#Lets find out correlation between temp and count
cor(bike.share[,c('temp','count')])

#Lets check relation with season for count variable
ggplot(bike.share,aes(x=bike.share$season,y=bike.share$count))+geom_boxplot(aes(color = factor(bike.share$season)))+theme_bw()

# It is observed that the season 1(summer) has few rentals comapared to season4(winter)
# but it can be saaid that the increase in count is not due to seasonality but due to increase in number of reantals

# extracting hour from datetime for exploratory purpose
bike.share$hour <- sapply(bike.share$datetime, function(x){format(x,"%H")})
head(bike.share)

# check distribution of count Vs Hour for workingday
pl <- ggplot(filter(bike.share,workingday==1), aes(hour,count))
pl <- pl + geom_point(position=position_jitter(w=1, h=0),alpha=0.3,aes(color=temp))
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl <- pl+theme_bw()
print(pl)
# check distribution of count Vs Hour for non-workingday
pl1 <- ggplot(filter(bike.share,workingday==0), aes(hour,count))
pl1 <- pl1 + geom_point(position=position_jitter(w=1, h=0),alpha=0.3,aes(color=temp))
pl1 <- pl1 + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl1 <- pl1+theme_bw()
print(pl1)

# I have noticed that working days have peak activity during the morning (~8am) and right after work gets out (~5pm), with some lunchtime activity. 
#While the non-work days have a steady rise and fall for the afternoon

#Build the model
temp.model <- lm(count ~ temp, bike.share)
summary(temp.model)

#predict count of rentals if temp=25
temp.test <- data.frame(temp=c(25))
predict(temp.model,temp.test)

# change hor column to numeric values
bike.share$hour <- sapply(bike.share$hour, as.numeric)

#Finally building model based on variable selection
model <- lm(count ~ . -casual-registered-datetime-atemp,bike.share )
summary(model)
