list1<-list(c(2,5,3),21.3,sin)
print(list1)
M<-matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=FALSE)
print(M)
a<-array(c('green','yellow'),dim=c(3,2,3))
print(a)
apple_colors<-c('green','green','red','yellow','red','red','yellow')
factor_apple<-factor(apple_colors)
print(factor_apple)
print(nlevels(factor_apple))
BMI <- 	data.frame(
  gender = c("Male", "Male","Female"), 
  height = c(152, 171.5, 165), 
  weight = c(81,93, 78),
  Age = c(42,38,26)
)
print(BMI)
ls()
print(ls())
N<-t(M)
print(N)
print(M %*% N)


#Loop repeat
v<-c("Hi")
cnt<-2
repeat{
  print(v)
  cnt<-cnt+1
  if(cnt>5){
    break
  }
}


v <- letters[1:4]
for ( i in v) {
  print(i)
}


a <- "Hello"
b <- 'How'
c <- "are you? "

print(paste(a,b,c))

print(paste(a,b,c, sep = "-"))

print(paste(a,b,c,collapse = ""))

x<- seq(5,10, by= 0.4)
x
s<- c('red','green', 6, TRUE)
s
list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2),
                  list("green",12.3))
list_data
list_data[4]<-"Text"
list_data
list_data[4]<-NULL
list_data
list_data[2]<-NULL
list_data
list_data[4]<-"Text"
list_data
list_data[3]<-"Updated Text"
list_data
# Elements are arranged sequentially by row.
M <- matrix(c(3:14), nrow = 4, byrow = TRUE)
print(M)

# Elements are arranged sequentially by column.
N <- matrix(c(3:14), nrow = 4, byrow = FALSE)
print(N)

# Define the column and row names.
rownames = c("row1", "row2", "row3", "row4")
colnames = c("col1", "col2", "col3")

P <- matrix(c(3:14), nrow = 4, byrow = TRUE, dimnames = list(rownames,colnames))
print(P)


#facor data object
data <- c("East","West","East","North","North","East","West","West","West","East","North")
print(data)
print(is.factor(data))
factor_data = factor(data)
print(factor_data)
print(is.factor(factor_data))
# Create the vectors for data frame.
height <- c(132,151,162,139,166,147,122)
weight <- c(48,49,66,53,67,52,40)
gender <- c("male","male","female","female","male","female","male")
# Create the data frame.
input_data <- data.frame(height,weight,gender)
print(input_data)
# Test if the gender column is a factor.
print(is.factor(input_data$gender))
# Print the gender column so see the levels.
print(input_data$gender)
## First control, then treatment:
gl(2, 8, labels = c("Control", "Treat"))
## 20 alternating 1s and 2s
gl(2, 1, 20)
## alternating pairs of 1s and 2s
gl(2, 2, 20)
