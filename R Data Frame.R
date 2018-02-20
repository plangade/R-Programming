# Data frame example
emp.data<-data.frame(
  emp.id=c(1:5),
  emp.name=c("Priyanka", "John", "Allen", "Raksha", "Steve"),
  emp.salary=c(1000, 1500, 500, 2000, 3000),
  start_date=as.Date(c("2012-01-01","2011-01-01","2013-01-01","2010-01-01","2009-01-01")),
  stringsAsFactors = FALSE
)
print(emp.data)

# get the structure of the data frame
str(emp.data)

#statistical summary and nature of the data
summary(emp.data)


#Extract specific column
result<-data.frame(emp.data$emp.name, emp.data$emp.salary)
print(result)

#Extract first 2 rows
result1<-emp.data[1:2,]
print(result1)

result2<-emp.data[1:2,2:4]
print(result2)

#extract 3rd and 5th row with 2nd and 4th column
result3<-emp.data[c(3,5),c(2,4)]
print(result3)

# Add column
emp.data$Dept<-c("IT", "Finance","Admin","HR", "Operations")
v<-emp.data
print(v)
print(emp.data)


# Create the second data frame
emp.newdata <- 	data.frame(
  emp.id = c (6:8), 
  emp.name = c("Rasmi","Pranab","Tusar"),
  emp.salary = c(578.0,722.5,632.8), 
  start_date = as.Date(c("2013-05-21","2013-07-30","2014-06-17")),
  Dept = c("IT","Operations","Fianance"),
  stringsAsFactors = FALSE
)

# Bind the two data frames.
emp.finaldata <- rbind(emp.data,emp.newdata)
print(emp.finaldata)
