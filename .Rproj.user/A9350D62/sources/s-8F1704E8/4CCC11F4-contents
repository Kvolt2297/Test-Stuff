#Source
# https://ourworldindata.org/mental-health#depression
depression.prev <- read.csv(file.choose(), stringsAsFactors = FALSE)
anxiety.prev <- read.csv(file.choose(), stringsAsFactors = FALSE)

head(depression.prev)
head(anxiety.prev)

mental.global <- read.csv(file.choose(), stringsAsFactors = FALSE)

which(mental.global$Entity == "Russia")

which(mental.global$Depression.... > 6.5)
#2244-2253
mental.global$Entity[2244:2253]
#Greenland
mental.global$Year[2244:2253]
#1993-2002
mat.mental<- as.matrix(mental.global)
nrow(mat.mental)
str(mat.mental)

lm(mental.global$Depression....[mental.global$Entity("World")] 
     ~ mental.global$Anxiety.disorders.... [mental.global$Entity("World")])

country <- unique(mental.global$Entity)
country


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

# Formula: r(x & y)  = S(x & y) / S(x) * S(y)
  
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


#Making a rough Graph
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

#line of best fit? 


#Getting only data that we need (country, year, anxiety and depression)
test<-data.frame(mental.global.2017[ ,c(1,3,7,9)])
test

#Running Global Correlation
cor(mental.global.2017[,10], mental.global.2017[,9])  #Depression/Alcohol 
# -0.01630034 

i <- 1
for(i in 1:7) {
  test.cor<-rep(cor(mental.global.2017[,i], mental.global.2017[, c(i+3)]) 7)
  print(test.cor) 
  
length(mental.global.2017[,4:10])

#Lindsay's Work

#renamed mental.global.2017

#removed non numerical data from table

Global.ment.sub = subset(mental.global.2017, select = -c(Entity,Year,Code))

# ran correlation for numerical data 

correlation.ment=cor(Global.ment.sub)
correlation.ment # This is where the table is at

#Installed Corrplot Packages 
install.packages("corrplot")
library(corrplot)
#Install dplyr package
install.packages("dplyr")
library(dplyr)

#Install xtable
install.packages("xtable")
library(xtable)
#Display data in corrolation plot
cor<- corrplot(correlation.ment)
corrplot.mixed(correlation.ment)



#Running it through multiple years
Global.ment.years = subset(mental.global, select = -c(Entity,Code))


#Deleting all rows except for year 1990 and finding correlation
#1990
prevalence.1990 = Global.ment.years[Global.ment.years$Year == "1990",]
correlation.1990 = cor(prevalence.1990[, 2:8])
#1991
prevalence.1991 = Global.ment.years[Global.ment.years$Year == "1991",]
correlation.1991 = cor(prevalence.1991[, 2:8])
#1992
prevalence.1992 = Global.ment.years[Global.ment.years$Year == "1992",]
correlation.1992 = cor(prevalence.1992[, 2:8])
#1993
prevalence.1993 = Global.ment.years[Global.ment.years$Year == "1993",]
correlation.1993 = cor(prevalence.1993[, 2:8])
#1994
prevalence.1994 = Global.ment.years[Global.ment.years$Year == "1994",]
correlation.1994 = cor(prevalence.1994[, 2:8])
#1995
prevalence.1995 = Global.ment.years[Global.ment.years$Year == "1995",]
correlation.1995 = cor(prevalence.1995[, 2:8])
#1996
prevalence.1996 = Global.ment.years[Global.ment.years$Year == "1996",]
correlation.1996 = cor(prevalence.1996[, 2:8])
#1997 
prevalence.1997 = Global.ment.years[Global.ment.years$Year == "1997",]
correlation.1997 = cor(prevalence.1997[, 2:8])
#1998
prevalence.1998 = Global.ment.years[Global.ment.years$Year == "1998",]
correlation.1998 = cor(prevalence.1998[, 2:8])
#1999
prevalence.1999 = Global.ment.years[Global.ment.years$Year == "1999",]
correlation.1999 = cor(prevalence.1999[, 2:8])
#2000
prevalence.2000 = Global.ment.years[Global.ment.years$Year == "2000",]
correlation.2000 = cor(prevalence.2000[, 2:8])
#2001
prevalence.2001 = Global.ment.years[Global.ment.years$Year == "2001",]
correlation.2001 = cor(prevalence.2001[, 2:8])
#2002
prevalence.2002 = Global.ment.years[Global.ment.years$Year == "2002",]
correlation.2002 = cor(prevalence.2002[, 2:8])
#2003
prevalence.2003 = Global.ment.years[Global.ment.years$Year == "2003",]
correlation.2003 = cor(prevalence.2003[, 2:8])
#2004
prevalence.2004 = Global.ment.years[Global.ment.years$Year == "2004",]
correlation.2004 = cor(prevalence.2004[, 2:8])
#2005
prevalence.2005 = Global.ment.years[Global.ment.years$Year == "2005",]
correlation.2005 = cor(prevalence.2005[, 2:8])
#2006
prevalence.2006 = Global.ment.years[Global.ment.years$Year == "2006",]
correlation.2006 = cor(prevalence.2006[, 2:8])
#2007
prevalence.2007 = Global.ment.years[Global.ment.years$Year == "2007",]
correlation.2007 = cor(prevalence.2007[, 2:8])
#2008
prevalence.2008 = Global.ment.years[Global.ment.years$Year == "2008",]
correlation.2008 = cor(prevalence.2008[, 2:8])
#2009
prevalence.2009 = Global.ment.years[Global.ment.years$Year == "2009",]
correlation.2009 = cor(prevalence.2009[, 2:8])
#2010
prevalence.2010 = Global.ment.years[Global.ment.years$Year == "2010",]
correlation.2010 = cor(prevalence.2010[, 2:8])
#2011
prevalence.2011 = Global.ment.years[Global.ment.years$Year == "2011",]
correlation.2011 = cor(prevalence.2011[, 2:8])
#2012
prevalence.2012 = Global.ment.years[Global.ment.years$Year == "2012",]
correlation.2012 = cor(prevalence.2012[, 2:8])
#2013
prevalence.2013 = Global.ment.years[Global.ment.years$Year == "2013",]
correlation.2013 = cor(prevalence.2013[, 2:8])
#2014 
prevalence.2014 = Global.ment.years[Global.ment.years$Year == "2014",]
correlation.2014 = cor(prevalence.2014[, 2:8])
#2015
prevalence.2015 = Global.ment.years[Global.ment.years$Year == "2015",]
correlation.2015 = cor(prevalence.2015[, 2:8])
#2016
prevalence.2016 = Global.ment.years[Global.ment.years$Year == "2016",]
correlation.2016 = cor(prevalence.2016[, 2:8])
#2017
prevalence.2017 = Global.ment.years[Global.ment.years$Year == "2017",]
correlation.2017 = cor(prevalence.2017[, 2:8])

#Automating? 
for(i in 1:length(Global.ment.years$Year) {
  if (Global.ment.years[c(i=>28, ),]) {
    
    
  }
}
#Creating DataFrames Table for 2010 to 2017

upper.2010 <- round(correlation.2010, 3)
upper.2010[upper.tri(correlation.2010)] <-""
upper.2010 <- as.data.frame(upper.2010) 

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


#Changing Names on the Table From 2010 to 2017
rownames(upper.2010) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2010) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2011) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2011) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2012) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2012) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2013) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2013) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2014) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2014) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2015) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2015) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2016) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2016) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2017) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2017) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")

#Merging Data Frames

merge.test<-merge(upper.2010, upper.2011, by = c("Schizophrenia", "Bipolar", "Eating Disorders", 
                                                 "Anxiety", "Drug Use Disorder", "Depression", 
                                                 "Alcohol Use Disorder"))

all_models <- rbind_list(
  upper.2010 %>% mutate(Year = 2010),
  upper.2011 %>% mutate(Year = 2011),
  upper.2012 %>% mutate(Year = 2012),
  upper.2013 %>% mutate(Year = 2013),
  upper.2014 %>% mutate(Year = 2014),
  upper.2015 %>% mutate(Year = 2015),
  upper.2016 %>% mutate(Year = 2016),
  upper.2017 %>% mutate(Year = 2017)
  )

#Visualize the Table. Taken from:
# https://www.littlemissdata.com/blog/prettytables
install.packages("data.table")

install.packages("dplyr")

install.packages("formattable")

install.packages("tidyr")

#Load the libraries

library(data.table)

library(dplyr)

library(formattable)

library(tidyr)

#Creating Tables 2010-2017

formattable(upper.2010, 
            align = c("l", rep("r", NCOL(upper.2010))), 
            list("Correlation 2010" = formatter("span", style = 
                                ~ style(color = "greay", font.weight = "bold"))
            ))
formattable(upper.2011, 
            align = c("l", rep("r", NCOL(upper.2011))), 
            list("Correlation 2010" = formatter("span", style = 
                                ~ style(color = "greay", font.weight = "bold"))
            ))

formattable(upper.2012, 
            align = c("l", rep("r", NCOL(upper.2012))), 
            list("Correlation 2010" = formatter("span", style = 
                                ~ style(color = "greay", font.weight = "bold"))
            ))
formattable(upper.2013, 
            align = c("l", rep("r", NCOL(upper.2013))), 
            list("Correlation 2010" = formatter("span", style = 
                                ~ style(color = "greay", font.weight = "bold"))
            ))
formattable(upper.2014, 
            align = c("l", rep("r", NCOL(upper.2014))), 
            list("Correlation 2010" = formatter("span", style =
                                ~ style(color = "greay", font.weight = "bold"))
            ))

formattable(upper.2015, 
            align = c("l", rep("r", NCOL(upper.2015))), 
            list("Correlation 2010" = formatter("span", style = 
                                ~ style(color = "greay", font.weight = "bold"))
            ))

formattable(upper.2016, 
            align = c("l", rep("r", NCOL(upper.2016))), 
            list("Correlation 2010" = formatter("span", style =
                                ~ style(color = "greay", font.weight = "bold"))
            ))

formattable(upper.2017, 
            align = c("l", rep("r", NCOL(upper.2017))),
            list("Correlation 2017" = formatter("span", style = 
                                ~ style(color = "greay", font.weight = "bold"))
            ))
        
         
            

#Creating a table for correlation with schizophrenia from 2010 - 2017
Year.2010 <- correlation.2010[2:7, 1]
Year.2011 <- correlation.2011[2:7, 1]
Year.2012 <- correlation.2012[2:7, 1]
Year.2013 <- correlation.2013[2:7, 1]
Year.2014 <- correlation.2014[2:7, 1]
Year.2015 <- correlation.2015[2:7, 1]
Year.2016 <- correlation.2016[2:7, 1]
Year.2017 <- correlation.2017[2:7, 1]
schiz.mental<- cbind(Year.2010, Year.2011, Year.2012, Year.2013, 
                     Year.2014, Year.2015, Year.2016, Year.2017) 
schiz.mental<- round(schiz.mental, 3)

#Creating a table for correlation with anxiety from 2010 - 2017
Year.2010 <- correlation.2010[c(1:3, 5:7), 4]
Year.2011 <- correlation.2011[c(1:3, 5:7), 4]
Year.2012 <- correlation.2012[c(1:3, 5:7), 4]
Year.2013 <- correlation.2013[c(1:3, 5:7), 4]
Year.2014 <- correlation.2014[c(1:3, 5:7), 4]
Year.2015 <- correlation.2015[c(1:3, 5:7), 4]
Year.2016 <- correlation.2016[c(1:3, 5:7), 4]
Year.2017 <- correlation.2017[c(1:3, 5:7), 4]
anx.mental<- cbind(Year.2010, Year.2011, Year.2012, Year.2013, 
                   Year.2014, Year.2015, Year.2016, Year.2017) 
anx.mental<- round(anx.mental, 3)

#Creating a table for correlation with depressionfrom 2010 - 2017
Year.2010 <- correlation.2010[c(1:5, 7), 6]
Year.2011 <- correlation.2011[c(1:5, 7), 6]
Year.2012 <- correlation.2012[c(1:5, 7), 6]
Year.2013 <- correlation.2013[c(1:5, 7), 6]
Year.2014 <- correlation.2014[c(1:5, 7), 6]
Year.2015 <- correlation.2015[c(1:5, 7), 6]
Year.2016 <- correlation.2016[c(1:5, 7), 6]
Year.2017 <- correlation.2017[c(1:5, 7), 6]
depr.mental<- cbind(Year.2010, Year.2011, Year.2012, Year.2013, 
                    Year.2014, Year.2015, Year.2016, Year.2017) 
depr.mental<- round(depr.mental, 3)
Year.2010

length(Global.ment.years["2010", ])

#Experimentation

for (i in Global.ment.years["2010",]) {
  
  print(Global.ment.years[,c(2:7)])
  print(i)
}

formattable(upper.2017, 
            align = c("l",rep("r", NCOL(upper.2017) - 1)),
            list(formatter("span", style = ~ style(color = "grey", 
                                  font.weight = "bold")), 
                 )
data.table(upper.2017)

install.packages("expss")
library(expss)
        
        hardcore <- cro(upper.2017$Schizophrenia, upper.2017$Anxiety)
?cro
      
          
          
          
          #Running it through multiple years
          Global.ment.years = subset(mental.global, select = -c(Entity,Code))
          
          
          ### meee ####
          
          all.country <- d.t
          all.country
          
          years <- unique(all.country$Year)
          years
          yearss <- c(years)
          yearss
          
          #reset i
          i <- 1
          ind <- 1
          
          for(i in 1:28){
            ind <- 1
            for(ind in 1:201){
              if(all.country$Year[ind] == yearss[i]){
                corr.tttt <- cor(all.country[all.country$Year == yearss[i],c(4:9)])
                
              }
              
              ind <- ind + 1 
            }
          }
          
          corr.t
          corr.tt
          all.country
          i
          corr.tttt
          
          warnings()
          
          
          for(ind in 1:201){
            if(any(all.country$Year == 2017)){
              corr.ttt <- cor(all.country[,c(4:9)])
            }
          }
          any(all.country$Year == 2000)
          corr.ttt
          
          for(ind in 1:201){
            if((all.country$Year == 2017)){
              corr.ttt <- cor(all.country[,c(4:9)])
            }
          }
          
          ## THIS IS THE LATEST/BEST VERSION, EXCHANGE 2017 FOR YEARS[i] FOR ALL YEARS
          
          for(i in 1:28){
            ind <- 1
            for(ind in 1:201){
              if(all.country$Year[ind] == 2017){
                cortt <- cor(all.country[all.country$Year == yearss[i],c(4:10)])
                
              }
              
              ind <- ind + 1 
            }
          }
          
          cortt
          head(all.country)
          
          
          

          

          #### ####
          #
          #
          #
          #
          #
          #
          #
          © 2020 GitHub, Inc.