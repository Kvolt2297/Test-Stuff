# 
# how to make a function

lots.of.integers <- c(3,6,7,3,5,7,5)
mean(lots.of.integers)
## [1] 5.142857

# function definition
mean.thor1 <- function(list.of.values){
  # show the data that come into the 
  print(list.of.values)
  # get the sum of all values
  sum.temp <- sum(list.of.values)
  # get the sample size of the vector
  length.val.temp <- length(list.of.values)
  # calculate the mean
  new.mean <- sum.temp / length.val.temp
  return(new.mean)
}

mean.thor(lots.of.integers)
# very short version of the function.
mean.short <- function(list.of.values){ print(list.of.values);sum(list.of.values) / length(list.of.values) }
mean.short(c(2,3,5))
#


