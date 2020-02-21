#
#
#
#
#  MAKING TABLES

# store all the names as a vector we can loop through
table.names <- c("form.table(upper.2010)",
                 "form.table(upper.2011)",
                 "form.table(upper.2012)",
                 "form.table(upper.2013)",
                 "form.table(upper.2014)",
                 "form.table(upper.2015)",
                 "form.table(upper.2016)",
                 "form.table(upper.2017)"

### DOESN'T WORK
# # loop through all the table names
# for(i in 1:length(table.names)){
#   
# }


# make tables and store them (manually)
pdf(paste(path.figures,"table.2010.pdf", sep = ""))
form.table(upper.2010)
dev.off()

pdf(paste(path.figures,"table.2011.pdf", sep = ""))
form.table(upper.2011)
dev.off()

pdf(paste(path.figures,"table.2012.pdf", sep = ""))
form.table(upper.2012)
dev.off()

pdf(paste(path.figures,"table.2013.pdf", sep = ""))
form.table(upper.2013)
dev.off()

pdf(paste(path.figures,"table.2014.pdf", sep = ""))
form.table(upper.2014)
dev.off()

pdf(paste(path.figures,"table.2015.pdf", sep = ""))
form.table(upper.2015)
dev.off()

pdf(paste(path.figures,"table.2016.pdf", sep = ""))
form.table(upper.2016)
dev.off()

pdf(paste(path.figures,"table.2017.pdf", sep = ""))
form.table(upper.2017)
dev.off()


# to make the correlation table 
pdf(paste(path.figures,"corralation.plot.pdf", sep = ""))
corrplot(correlation.ment)
dev.off()

# to make a mixed correlation table (should have clearer variable names)
pdf(paste(path.figures,"corralation.plot.mixed.pdf", sep = ""))
corrplot.mixed(correlation.ment)
dev.off()


#