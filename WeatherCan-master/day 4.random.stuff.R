#
# examples how to structure a file properly
#
# library needed
#   none
#
# index
# 1 data types
# 2 cool functions
#

# ------------------------------------------------------------------------------
# --------------  1 data types -------------------------------------------------       
d

d


d
dd

# ------------------------------------------------------------------------------


# ---------------- 2 cool functions---------------------------------------------
# drawing from normal distribution

# define parameters
mean.cricket <- 5.2    # taken from Veen et al 2018
sd.cricket <- 1.32
sample.size <- 12
col.thor <- c("blue", "purple")   # colour for the figures
str(col.thor)
class(col.thor)

# indexing
list.files()    # show all the files in the working directory
getwd()
m <- read.csv("mammals.csv", stringsAsFactors = FALSE )
head(m) # show first couple....
str(m)
nrow(m) # number of rows
ncol(m) # number of columns
m[1]

# bla blah
#
#
d <- rnorm(sample.size, mean.cricket, sd.cricket ) # draw from a normal distribution
hist(d)               # make a histogram of the data
?rnorm                # look up the help function

# drawing from binomial distribution
qnorm(sample.size, mean.cricket, sd.cricket)


# making a plot
plot(mean.cricket, sd.cricket, col = col.thor[1])
points(5,1.1, col = col.thor[2])

