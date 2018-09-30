df1 <- read.csv('winequality-red.csv',sep=';')
df2 <- read.csv2('winequality-white.csv', sep = ';')

head(df1)
head(df2)

df1$label <- sapply(df1$pH,function(x){'red'})
df2$label <- sapply(df2$pH,function(x){'white'})

wine <- rbind(df1, df2)
head(wine)
tail(wine)
print(str(wine))

wine$residual.sugar <- as.numeric(wine$residual.sugar)
wine$citric.acid <- as.numeric(wine$citric.acid)
wine$alcohol <- as.numeric(wine$alcohol)
##EDA
library(ggplot2)
pl <- ggplot(wine, aes(residual.sugar))
pl <- pl + geom_histogram(aes(fill=label),color='black') 
pl <- pl + scale_fill_manual(values=c('red','white'))+theme_bw()
print(pl)

pl <- ggplot(wine, aes(citric.acid))
pl <- pl + geom_histogram(aes(fill=label),color='black',bins=50) 
pl <- pl + scale_fill_manual(values=c('red','white'))+theme_bw()
print(pl)
 
pl <- ggplot(wine, aes(alcohol))
pl <- pl + geom_histogram(aes(fill=label),color='black',bins=50) 
pl <- pl + scale_fill_manual(values=c('red','white'))+theme_bw()
print(pl)
                             
pl <- ggplot(wine, aes(x=citric.acid,y=residual.sugar))
pl <- pl + geom_point(aes(color=label),alpha=0.2) 
pl <- pl + scale_fill_manual(values=c('#ae4554','#faf7ea'))+theme_dark()
print(pl)

pl <- ggplot(wine, aes(x=volatile.acidity, y=residual.sugar))
pl <- pl + geom_point(aes(color=label),alpha=0.2) 
pl <- pl + scale_fill_manual(values=c('#ae4554','#faf7ea'))+theme_dark()
print(pl)

clus.data <- wine[,1:12]
head(clus.data)

##Build model
wine.cluster <- kmeans(wine[1:12],2)
print(wine.cluster$centers)


