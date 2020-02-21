#
#
# 
# Main, to set up the workflow 
#   making folders
#
# Version of R
#                  _                           
# platform       x86_64-apple-darwin15.6.0   
# arch           x86_64                      
# os             darwin15.6.0                
# system         x86_64, darwin15.6.0        
# status                                     
# major          3                           
# minor          6.0                         
# year           2019                        
# month          04                          
# day            26                          
# svn rev        76424                       
# language       R                           
# version.string R version 3.6.0 (2019-04-26)
# nickname       Planting of a Tree  
#
#
#  Packages used
install.packages("corrplot")
install.packages("data.table")
install.packages("dplyr")
install.packages("formattable")
install.packages("tidyr")
install.packages("xtable")

#  Libraries used 
library("corrplot")
library("data.table")
library("dplyr")
library("formattable")
library("tidyr")
library("xtable")
#
#
#
#
# 
work.dir <- getwd()
# 
#
#
make.folder.names <- c("data", "figures", "data.output")


for(i in 1:length(make.folder.names))
  if(file.exists(make.folder.names[i]) == FALSE)
    dir.create(make.folder.names[i])

path.data <- paste(work.dir, "/", make.folder.names[1], "/", sep = "")
path.figures <- paste(work.dir, "/", make.folder.names[2], "/", sep = "")
path.data.output <- paste(work.dir, "/", make.folder.names[3], "/", sep = "")
#
# 
#
#
#
#
#
# To run everything
source("cleaning.R")
source("correlation.R")
source("manualcorrelations.R")
source("makingfigures.R")
#
#
#