# suppress warnings
options(warning=FALSE)

library(ggplot2)
library(data.table)
library(ggthemes)
library(plotly)


#Import the ggplot2 data.table libraries and use fread to load the csv file 'Economist_Assignment_Data.csv' 
#df <- read.csv('Economist_Assignment_Data.csv')
df <- fread('Economist_Assignment_Data.csv',drop=1)

#Check the head of df
head(df)
str(df)

#create scatter plot
pl <- ggplot(df, aes(x=CPI,y=HDI,color=Region))+geom_point(shape = 1, size = 3)
print(pl)

#Add trend line
pl2 <- pl + geom_smooth(aes(group=1), method='lm',formula = y~log(x),se=FALSE,color='red')
print(pl2)

#Adding label for subset countries
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

pl3 <- pl2 + geom_text(aes(label=Country), color = 'gray20', data=subset(df,Country %in% pointsToLabel,check_overlap=TRUE))
print(pl3)

#Adding theme
pl4 <- pl3+theme_bw()
print(pl4)
pl5 <- pl4 + 
  scale_x_continuous(name="Corruption Perceptions Index,2011(10=least corrupt)",limits=c(0.9,10.5),breaks=1:10) +
  scale_y_continuous(name='Human Development Index, 2011(1=Best)',limits=c(0.2,1.0)) + 
  ggtitle('Corruption and Human Development')

print(pl5)

#Adding theme from ggthemes
pl6 <- pl5 + theme_economist_white()
print(pl6)

#interactive visualization using plotly
print(ggplotly(pl6))
