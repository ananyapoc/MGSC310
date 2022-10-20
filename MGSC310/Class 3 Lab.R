### In Class Exercise 1 ###
y <- c(3,2,15,-1,22,1,9,17,5)

#length of y
length(y)

#display the first and last values
y[c(1,9)]

#find the last value for a vector of any length
y[length(y)]

#display the values that are greater than the mean of y
y[y > mean(y)]

#how many elements of y are greater than 10
sum(y > 10)

#are all the values positive?
all(y > 0)

#are any of the values equal to the mean?
any(y == mean(y))

#are any of the values equal to the median?
any(y == median(y))

