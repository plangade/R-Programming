# Create a vector. 
x <- c(12,7,3,4.2,18,2,54,-21,8,-5)
# Find Mean.
result.mean <- mean(x)
print(result.mean)

#sort 
x<-sort(x)
print(x)
# Find Mean by applying trim option
result.mean1 <-  mean(x,trim = 0.3)
print(result.mean1)

y<-c(12,7,3,4.2,18,2,54,-21,8,-5,NA)
result.mean2 <- mean(y)
print(result.mean2)

# Find mean dropping NA values.
result.mean3 <-  mean(y,na.rm = TRUE)
print(result.mean3)


####### MEDIAN 

median.result<-median(x, na.rm=FALSE)
print(median.result)


####### Mode
getmode<-function(v){
  print(v)
  uniqv <- unique(v)
  #print(uniqv)
  #z<-match(v, uniqv)
  #print(z)
  #a<-tabulate(z)
  #print(a)
  #print(which.max(a))
  print(uniqv[which.max(tabulate(match(v,uniqv)))])
}

# Create the vector with numbers.
v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)

# Calculate the mode using the user function.
result <- getmode(v)
print(result)

# Create the vector with characters.
charv <- c("o","it","the","it","it")

# Calculate the mode using the user function.
result <- getmode(charv)
print(result)
