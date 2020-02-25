#Main File
#
# Volodko Kirill
#
#
# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"
# Copyright (C) 2019 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# 
# R is free software and comes with ABSOLUTELY NO WARRANTY.
# You are welcome to redistribute it under certain conditions.
# Type 'license()' or 'licence()' for distribution details.
# 
# Natural language support but running in an English locale
# 
# R is a collaborative project with many contributors.
# Type 'contributors()' for more information and
# 'citation()' on how to cite R or R packages in publications.
# 
# Type 'demo()' for some demos, 'help()' for on-line help, or
# 'help.start()' for an HTML browser interface to help.
# Type 'q()' to quit R.




#----------------------------------------------------------
#Sorting Out Folders-----------------------------------------------------------
#
setwd("C:/Users/Kirill (Kvolt)/Desktop/RStudio Files/Personal Project")
working.dir <- getwd()

#Create folders Names
output.folder.names <- c("tables", "data")

#Loop to creat the folders
for(i in 1:length(output.folder.names)) 
  if(file.exists(output.folder.names[i]) == FALSE) #If the folders are not 
    #present, create the folder
    dir.create(output.folder.names[i])


#Set pathing for the folders.
# path.graphs <- path.figures <- paste(working.dir, "/", 
#                                      output.folder.names[1], "/", sep = "")

path.tables <- paste(working.dir, "/", output.folder.names[1], "/", 
                     sep = "")

path.data <- paste(working.dir, "/", output.folder.names[2], "/", sep = "")

#----------------------------------------------------------
#Number of participats in the experiment---------------------------------
#
#How many participants in total in the experiment (equally divided into 4 
#groups)
#
total.participants <- 67  #total amount of participants in the experiment
n.participants <- round(total.participants/4, 0)  
#how any participants in each group. round it off to 0 s.f. 
#Assumption is that I always have equal amount of participants in each group.
#I can't have half a participatns.
#----------------------------------------------------------
#Source Code------------------------------------------------------------------
source("Raw Data.R")
source("Descriptives.R")
source("Data Emulation.R")
source("ANOVA Values.R")
source("Final Table.R")
