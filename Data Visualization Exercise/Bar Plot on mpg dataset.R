library(ggplot2)
pl<-ggplot(mpg, aes(x=manufacturer))
pl2 <- pl+geom_bar(aes(fill=factor(cyl)))
print(pl2)
