library(ggplot2)
head(mpg)

pl<-ggplot(mpg,aes(x=hwy))+geom_histogram(bins=20,fill='red',alpha=0.5)
print(pl)

pl1<-ggplot(mpg, aes(x=manufacturer))
pl2 <- pl1+geom_bar(aes(fill=factor(cyl)))
print(pl2)
