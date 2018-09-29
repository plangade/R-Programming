library(ggplot2)
head(txhousing)

pl<-ggplot(txhousing, aes(x=sales,y=volume))
pl2<- pl+geom_point(color='blue',alpha=0.3)
pl2 <- pl2 + geom_smooth(color='red')
print(pl2)
