#
#
# To find correlations for each year 
#    average all countries per year
#
#
# 
#

# to get all the years for which there is data
years.tot <- unique(all.candy$Year)

# store how many countries there is data for
n.countries.data <- c(NA, length(years.tot))  

# make objects to describe where in the data frame the data is
# the column numbers of all.candy which contain frequencies
col.freq <- c(4:9)



# initialize a data frame to store the correlation coeeficients
col.NA <- rep(NA, length(years.tot))
data.correlations <- cbind.data.frame(year = years.tot, 
                                      anx.vs.depr = col.NA)
## NOTE ## 
# this loop only gives correlation information for anxiety and depression, from
#  1990 to 2017. This could be expanded to include all correlations (SEE BELOW)

global.store.year <- data.correlations

for(i in 1:length(years.tot)){
  
  # make a subset of year i
  all.candy.t <- all.candy[ all.candy$Year == years.tot[i],]
  
  unique(all.candy.t$Year) # printing this for a quick check to see if it works, 
  #  should return list of 1990 to 2017 if whole loop
  #  runs
  
  # how many countries do we have data for in year i?
  n.countries.data[i] <- length(unique(all.candy.t$Entity))
  print(unique(n.countries.data))    # this confirms we have data for all 201 
  #  countries for each year
  
  
  # start your analysis
  # correlation for all of the six variables and store in object cor.by.year
  cor.by.year <- cor(all.candy.t[,col.freq])
  
  # TEMP just one of the correclations
  # just anxiety and depression
  cor.by.year.ad <- cor(all.candy.t[,c(7,9)])
  
  
  # to extract just one value
  anxt.vs.depr <- cor.by.year.ad[1,2]
  
  # to store the correlation value in the data frame in the row corresponding to
  #   it's year
  global.store.year[i,2] <- c(anxt.vs.depr)
  
  
  
}

# to see what the resulting data frame looks like
global.store.year



#
#
#
#
#