##
#
#
#  GOOD CLEANING
# "C:/folder/folder/Desktop/file.csv"
# read csv
ment.and.sub <- read.csv("prevalence-by-mental-and-substance-use-disorder-c.csv",
                         header = TRUE, sep = ",",stringsAsFactors = FALSE)


# to get a list of all the countries stored as country.num
country.num <- unique(ment.and.sub$Entity)

# to check how many unique enities there is data for
how.many.entities <- length(country.num)

# this data includes entities which are not countries. most notably, the region
#  "World", which likely is the data from all countries combined, is removed

# to find the rows which contain data for "World"
world.rows <- which(ment.and.sub$Entity == "World")

# to remove the rows which contain data for "World"
new.ment.and.sub <- ment.and.sub[-c(world.rows),]

# to check there are no rows left with data for "World"
world.rows.gone <- which(new.ment.and.sub$Entity == "World")
world.rows.gone

# instead of going through this proccess for every entity to be removed, look at
#  the list of entities to see which are to be removed (now without world data)
rm.ent <- unique(new.ment.and.sub$Entity)
rm.ent

# going through this list to get all entities which are not countries, but
#   instead are regions, to make a vector of all the entities for which to
#   remove data, gives the vector
dont.want <- c("Andean Latin America", "Central Asia", "Central Europe", 
               "Central Europe, Eastern Europe, and Central Asia", 
               "Central Latin America", "Central Sub-Saharan Africa", 
               "Eastern Europe", "Eastern Sub-Saharan Africa", "High SDI", 
               "High-income", "High-income Asia Pacific", "High-middle SDI", 
               "Latin America and Caribbean", "Low SDI", "Low-middle SDI", 
               "Middle SDI", "North Africa and Middle East", "North America", 
               "Oceania", "South Asia", "Southeast Asia", 
               "Southeast Asia, East Asia, and Oceania", 
               "Southern Latin America", "Southern Sub-Saharan Africa",
               "Sub-Saharan Africa", "Tropical Latin America", 
               "United Kingdom", "Western Europe", "Western Sub-Saharan Africa")

# the length of this vector indicates how many entities are being removed
length(dont.want)
## 29

# to make a loop which removes all rows from the data frame new.ment.and.sub 
#   which contain data for regions which are to be ignored

# first make copy called d.t
d.t <- new.ment.and.sub

# let's go through Entity and exclude all entries in dont.want
for(i in 1:length(dont.want)){
  
  # changes d.t to only contain rows with entities which are not listed in 
  #  dont.want
  d.t <- d.t[d.t$Entity != dont.want[i],]
  
}

# go and check if it is indeed gone
gone <- unique(d.t$Entity)  # gone is a vector of the 201 countries we want to 
#   to keep as Entities.

# making a copy of the cleaned data frame with a better name
#  name the data frame to indicate it has all the countries and all the years
all.candy <- d.t

#
#
#
#
