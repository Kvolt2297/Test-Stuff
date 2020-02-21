#
#
# =============================================================================
# ==== General ================================================================
# =============================================================================
#
# the main.R file is at the centre of my projects. Start here!
# 1) which libraries are needed
# 2) make folder structure and create paths to folders
# 3) define 'global' variables; variables used throughout the code
# 4) run R scripts with e.g. custum functions functions 
#
#
# typically start with the version of R it works on
# 
R.version.string
#  "R version 3.5.1 (2018-07-02)"
# this can be expanded by using
# sessionInfo()

# *****************************************************************************
# NOTES
# • 
#
#
# good coding does not go beyond 80 characters on a line... (not me!)
# https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
# Set this in RStudio
#Tools -> Global Options -> Code -> Display
# [ ] Show margin
# Margin column [80]
# *****************************************************************************

# I like to set the working dir so I can always retrace where files should be 
# in case the code fails
working.dir <- getwd()


# =============================================================================
# ==== 1) libraries ===========================================================
# =============================================================================
# libraries contain specific functions and they come in many shapaes an forms
# library("name of package")

# if you don't have the library yet, install it! Either through the 'Tools' menu
# or install.packages("name of package")
# install package
install.packages("weathercan")

# load package
library("weathercan")

# =============================================================================
# === 2) folder management ====================================================
# =============================================================================
# names of folders for output data (figures,data output,data)
# store names of the folders in an object
output.folder.names <- c("figures", "data.output", "data")
# and make the folders if they don't exit yet. No need to understand this now
for(i in 1:length(output.folder.names)) 
  if(file.exists(output.folder.names[i]) == FALSE) 
    dir.create(output.folder.names[i])

# you need to store in an object the file path to these folders so we can 
# read from them and write to them.
# again no need to understand the code
path.figures <- paste(working.dir, "/", output.folder.names[1], "/", sep = "")
path.data.output <- paste(working.dir, "/", output.folder.names[2], "/", 
                          sep = "")
path.data <- paste(working.dir, "/", output.folder.names[3], "/", sep = "")

# test saving file to output folder
write.csv(output.folder.names, paste(path.data.output, "uselessnames.csv"),
          row.names = FALSE)

# =============================================================================
# === 3) global variables =====================================================
# =============================================================================
# define variables here that you will use throughout the code
# for example, number of iteratons of a permutation, colours used, etc

# colours
col.t <- c("#FF0000", "#80FF00")   # hexadecimal colour code for red and lime
alpha <- 90                        # sets the transparancy
col.trans <- paste(col.t, alpha, sep = "")


# =============================================================================
# === 4) run custom code ======================================================
# =============================================================================
# if you have custum R scripts with e.g. functions for data manipulations
# or to make figures, run this here so they are in the working memory
# example to run R script called figures
# source("figures.R")


# ==== end of main =============================================================





