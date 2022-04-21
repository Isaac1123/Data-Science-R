#######Chapter 3.4#######

#Exercise 2
######################
#Computer Software
#Programming Languages
#Provides constructive criticism
#Verbalizes ideas clearly and concisely
#Empowers workers by delegating some responsibilities
######################

#Exercise 3
#a)
#install.packages("neuralnet") #Install the library
#b)
library(neuralnet) #Read the library documentation from CRAN
#c)
??neuralnet #Find the function from the library document

#Exercise 8

vector <- c(10,11,9,15,19,52,19,7,10,22,28,40,6,99,33,35,26,5,87,91,0,12,16,81,200)
matrix <- matrix(vector, nrow = 5, byrow = TRUE)
diagonal <- diag(x=matrix)
transformmatrix <- function(x){
  my_mean <- mean(x)
  my_median <- median(x)
  return(c(my_mean, my_median))
}#closing the function

#a)
transformmatrix(diagonal) #Use UDF to show the mean/median of diagonal

#b)
vector2 <- c(1:9)
matrix2 <- matrix(vector2, nrow=3, byrow= TRUE)
diagonal2 <- diag(matrix2)
transformmatrix(diagonal2) #Use UDF to show the mean/median of diagonal2

#######Chapter 4.4#######

#Exercise 10

library(MASS)
air_qual <- airquality

clean_na <- function(x, col_idx= c(1,2,3)){
  new_df <- x[-which(is.na(x[,col_idx]))]
  return(new_df)
}#closing the i_loop

clean_na(air_qual, col_idx = 2) #Use UDF to clean up the NA in air_qual




