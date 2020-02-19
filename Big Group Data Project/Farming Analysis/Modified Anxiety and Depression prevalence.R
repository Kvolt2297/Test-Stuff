##
#
#
#
# Big Data Project - Kirill's Work
#
#
#
##
#Installing and loading libraries---------------------------------------------
install.packages("data.table")

install.packages("dplyr")

install.packages("formattable")

install.packages("tidyr")

install.packages("corrplot")


#Loading Libraries
library(formattable)

library(data.table)

library(dplyr)

library(tidyr)

library(corrplot)

#First Tests-----------------------------------------------

#Taking away the "World" out of country
mental.global <- mental.global[-c(6357:6384),]

#Taking away Wesetrn Sub-Saharn Africa
which(mental.global$Entity == "Western Sub-Saharan Africa")
mental.global <- mental.global[-c(6329:6356),]
#Western Europe
which(mental.global$Entity == "Western Europe")
mental.global <- mental.global[-c(6301:6328),]

#United States Virgin Islands
delete.row<-which(mental.global$Entity == "United States Virgin Islands")
mental.global <- mental.global[-c(delete.row),]
country

#Attempting Implementing Correlation Coefficient

#We will pick Canada, 2017 data for Anxiety and Depression

which (mental.global$Entity == "Canada")
#980. I know this is the 2017, because it has to be the last row.

#Taking the info from the 980 row (Canada 2017)
mental.global[980,]

#Canada, 2017, Anxiety = 5.178218, Depression 3.988792
#
#
#Working on correlation coeficient

# ormula: r(x & y)  = S(x & y) / S(x) * S(y)
  
#S(x) and S(y) is sample variation. S(x & y) is sample covarience
#
#
#Covarience sees how variable x and y in data set are lineary related
#
# There is a covarience function, called "cov()" 
cov(mental.global[980, 7], mental.global[980, 9])    #Trying covarience function
#Got NA

#Using all the years, to see if it work. EDIT: it does work.
cov.mental<-cov(mental.global[953:980, 7], mental.global[953:980, 9]) 
#cov = 0.007637826

#NOTE: It seems we need a set of multiple datas... we might need to use all 
#years for that

#Correlation Coeficient
cor.mental.Canada<-cor(mental.global[953:980, 7], mental.global[953:980, 9])
cor.mental
#0.4238462

plot(c(cor.mental.Canada, cor.mental.US), col = c("Red", "Blue"))

which (mental.global$Entity == "United States")
cor.mental.US<-cor(mental.global[6077:6104, 7], mental.global[6077:6104, 9]) 


#Making a rough Graph for US and Canada Anxiety & Depression Rates
plot(mental.global[c(6077:6104),c(3,7)], col = "Blue", xlim = c(1990, 2017),
     ylim = c(0,7), ylab = "Prevalence (%)", pch = 15, 
    # legend(1, 5, legend = c("US", "Canada"), col = c("blue", "Red"), 
           # pch = c(15, 2), fill = c("red", "blue")),
     main = "US and Canada Anxiety and Depression Rates") #US anxiety

#getting an error of "Error in title(...) : invalid graphics parameter" from 
#legend. I take it out for now.
points(mental.global[c(6077:6104),c(3,9)], col = "Red", pch = 15) #US depression
points(mental.global[c(953:980),c(3,7)], col = "Purple", pch = 2) #Canada anxiety
points(mental.global[c(953:980),c(3,9)], col = "Green", pch = 2) #Canada depression



#Getting only data that we need (country, year, anxiety and depression)
test<-data.frame(mental.global.2017[ ,c(1,3,7,9)])
test

#Running Global Correlation
cor(mental.global.2017[,10], mental.global.2017[,9])  #Depression/Alcohol 
# -0.01630034 

#First attempts to get correlation coeficient for all of data in year 20177
i <- 1
for(i in length(mental.global.2017[,4:10])) {
  test.cor<-rep(cor(length(mental.global.2017$Entity[i]), 
                    mental.global.2017[, c(i+3)]), 7)
  print(test.cor) 
}
#Lindsay's Work---------------------------------------------------

#renamed mental.global.2017

#removed non numerical data from table

Global.ment.sub = subset(mental.global.2017, select = -c(Entity,Year,Code))

# ran correlation for numerical data 

correlation.ment=cor(Global.ment.sub)
correlation.ment # This is where the table is at



#Display data in corrolation plot
cor<- corrplot(correlation.ment)
corrplot.mixed(correlation.ment)


#Start of Farmer's Coding----------------------------------------------
#Running it through multiple years
Global.ment.years = subset(mental.global, select = -c(Entity,Code))


# Finding correlation from 1990 and onwards by selecting each year
# EDIT: Start of farmer's coding

#1990
#Selecting only year 1990 and assigning it to an object
prevalence.1990 <- Global.ment.years[Global.ment.years$Year == "1990",]
#Running correlation on the object, only from 4th to 10th row (where data for 
#mental disorders are located). Saving in in new correlation.1990 object.
correlation.1990 <- cor(prevalence.1990[, 4:10])
#1991
#Same process repeated until year 2017
prevalence.1991 <- Global.ment.years[Global.ment.years$Year == "1991",]
correlation.1991 = cor(prevalence.1991[, 4:10])
#1992
prevalence.1992 <- Global.ment.years[Global.ment.years$Year == "1992",]
correlation.1992 <- cor(prevalence.1992[, 4:10])
#1993
prevalence.1993 <- Global.ment.years[Global.ment.years$Year == "1993",]
correlation.1993 <- cor(prevalence.1993[, 4:10])
#1994
prevalence.1994 <- Global.ment.years[Global.ment.years$Year == "1994",]
correlation.1994 <- cor(prevalence.1994[, 4:10])
#1995
prevalence.1995 <- Global.ment.years[Global.ment.years$Year == "1995",]
correlation.1995 <- cor(prevalence.1995[, 4:10])
#1996
prevalence.1996 <- Global.ment.years[Global.ment.years$Year == "1996",]
correlation.1996 <- cor(prevalence.1996[, 4:10])
#1997 
prevalence.1997 <- Global.ment.years[Global.ment.years$Year == "1997",]
correlation.1997 <- cor(prevalence.1997[, 4:10])
#1998
prevalence.1998 <- Global.ment.years[Global.ment.years$Year == "1998",]
correlation.1998 <- cor(prevalence.1998[, 4:10])
#1999
prevalence.1999 <- Global.ment.years[Global.ment.years$Year == "1999",]
correlation.1999 <- cor(prevalence.1999[, 4:10])
#2000
prevalence.2000 <- Global.ment.years[Global.ment.years$Year == "2000",]
correlation.2000 <- cor(prevalence.2000[, 4:10])
#2001
prevalence.2001 <- Global.ment.years[Global.ment.years$Year == "2001",]
correlation.2001 <- cor(prevalence.2001[, 4:10])
#2002
prevalence.2002 <- Global.ment.years[Global.ment.years$Year == "2002",]
correlation.2002 <- cor(prevalence.2002[, 4:10])
#2003
prevalence.2003 <- Global.ment.years[Global.ment.years$Year == "2003",]
correlation.2003 <- cor(prevalence.2003[, 4:10])
#2004
prevalence.2004 <- Global.ment.years[Global.ment.years$Year == "2004",]
correlation.2004 <- cor(prevalence.2004[, 4:10])
#2005
prevalence.2005 <- Global.ment.years[Global.ment.years$Year == "2005",]
correlation.2005 <- cor(prevalence.2005[, 4:10])
#2006
prevalence.2006 <- Global.ment.years[Global.ment.years$Year == "2006",]
correlation.2006 <- cor(prevalence.2006[, 4:10])
#2007
prevalence.2007 <- Global.ment.years[Global.ment.years$Year == "2007",]
correlation.2007 <- cor(prevalence.2007[, 4:10])
#2008
prevalence.2008 <- Global.ment.years[Global.ment.years$Year == "2008",]
correlation.2008 <- cor(prevalence.2008[, 4:10])
#2009
prevalence.2009 <- Global.ment.years[Global.ment.years$Year == "2009",]
correlation.2009 <- cor(prevalence.2009[, 4:10])
#2010
prevalence.2010 <- Global.ment.years[Global.ment.years$Year == "2010",]
correlation.2010 <- cor(prevalence.2010[, 4:10])
#2011
prevalence.2011 <- Global.ment.years[Global.ment.years$Year == "2011",]
correlation.2011 <- cor(prevalence.2011[, 4:10])
#2012
prevalence.2012 <- Global.ment.years[Global.ment.years$Year == "2012",]
correlation.2012 <- cor(prevalence.2012[, 4:10])
#2013
prevalence.2013 <- Global.ment.years[Global.ment.years$Year == "2013",]
correlation.2013 <- cor(prevalence.2013[, 4:10])
#2014 
prevalence.2014 <- Global.ment.years[Global.ment.years$Year == "2014",]
correlation.2014 <- cor(prevalence.2014[, 4:10])
#2015
prevalence.2015 <- Global.ment.years[Global.ment.years$Year == "2015",]
correlation.2015 <- cor(prevalence.2015[, 4:10])
#2016
prevalence.2016 <- Global.ment.years[Global.ment.years$Year == "2016",]
correlation.2016 <- cor(prevalence.2016[, 4:10])
#2017
prevalence.2017 <- Global.ment.years[Global.ment.years$Year == "2017",]
correlation.2017 <- cor(prevalence.2017[, 4:10])

#Creating DataFrames Table for 2010 to 2017---------------------------------

#Taking off the top half values from the table, as they repeart the bottom half
upper.2010 <- round(correlation.2010, 3) #Rounding the numbers to 3 s.f.
upper.2010[upper.tri(correlation.2010)] <-"" #Replacing all of the upper values
#with empty gaps.
upper.2010 <- as.data.frame(upper.2010) #Forming a dataframe from this.

#Repeat the same process for the next years

upper.2011 <- round(correlation.2011, 3)
upper.2011[upper.tri(correlation.2011)] <-""
upper.2011 <- as.data.frame(upper.2011) 

upper.2012 <- round(correlation.2012, 3)
upper.2012[upper.tri(correlation.2012)] <-""
upper.2012 <- as.data.frame(upper.2012) 

upper.2013 <- round(correlation.2013, 3)
upper.2013[upper.tri(correlation.2013)] <-""
upper.2013 <- as.data.frame(upper.2013) 

upper.2014 <- round(correlation.2014, 3)
upper.2014[upper.tri(correlation.2014)] <-""
upper.2014 <- as.data.frame(upper.2014) 

upper.2015 <- round(correlation.2015, 3)
upper.2015[upper.tri(correlation.2015)] <-""
upper.2015 <- as.data.frame(upper.2015) 

upper.2016 <- round(correlation.2016, 3)
upper.2016[upper.tri(correlation.2016)] <-""
upper.2016 <- as.data.frame(upper.2016) 

upper.2017 <- round(correlation.ment, 3)
upper.2017[upper.tri(correlation.ment)] <-""
upper.2017 <- as.data.frame(upper.2017) 


#Changing Names on the Tables From 2010 to 2017--------------------------------

#Giving names for each column and row. All positions of names in columns and 
#rows are the same. 
naming.label <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                  "Anxiety", "Drug Use Disorder", "Depression", 
                  "Alcohol Use Disorder")


renaming<-function(x) {
  rownames(objects(x)) <- naming.label
  colnames(objects(x)) <- naming.label
                        }


#Assigning the name to correlation coefficient from 2010 to 2017 data

  renaming(upper.2010)
  rownames(upper.2010) <- naming.label #Renaming the rows in upper.2010
  colnames(upper.2010) <- naming.label #Renaming the columns in upper.2010
  
  rownames(upper.2011) <- naming.label
  colnames(upper.2011) <- naming.label
  
  rownames(upper.2012) <- naming.label
  colnames(upper.2012) <- naming.label
  
  rownames(upper.2013) <- naming.label
  colnames(upper.2013) <- naming.label
  
  rownames(upper.2014) <- naming.label
  colnames(upper.2014) <- naming.label
  
  rownames(upper.2014) <- naming.label
  colnames(upper.2014) <- naming.label
  
  rownames(upper.2015) <- naming.label
  colnames(upper.2015) <- naming.label
  
  rownames(upper.2016) <- naming.label
  colnames(upper.2016) <- naming.label

  rownames(upper.2017) <- naming.label
  colnames(upper.2017) <- naming.label

#Visualizing Tables 2010-2017------------------------------------------------

#Creating form.table function to create nicer looking tables 
form.table<-function(x) {               #Creating a way to input an object 
              formattable(x,            #into the fucntion
#Making sure that text is right aligned and columns come from the (x)               
              align = c("l", rep("r", NCOL(x))),
              list( formatter(style =  
                ~ style(font.weight = "bold")) #make top font bold
              ))
}
#Running the function for the rest of the data.frames that I have. 
form.table(upper.2010)
form.table(upper.2011)
form.table(upper.2012)
form.table(upper.2013)
form.table(upper.2014)
form.table(upper.2015)
form.table(upper.2016)
form.table(upper.2017)         
         



