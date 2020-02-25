#
#
#Volodko Kirill
#
#

#Making a difference in Mean and ANOVA results table---------------------------

final.table <- cbind.data.frame(Group = rep(names.groups, 1),
                                "Mean Before Experiment" = rep(NA, 4), 
                                "Mean After TSST" = rep(NA, 4),
                                "Mean After Jokes" = rep(NA, 4),
                                "F-value" = rep(NA, 4),
                                "Significance" = rep(NA, 4))

#Filling up The Final Table----------------------------------------------------

#Means from the First Phase

final.table$`Mean Before Experiment`[is.na #if there is an NA in those 
                                     #rows/columns
                                     
                                     (final.table$`Mean Before Experiment`)] <- 
  s.descr.table$Mean[s.descr.table$Group %in% final.table$Group]
#Replace them with the data from that columns, from all of these 
#rows that match with the rows of the other data.frame.

#Means from the Second Phase (ONLY TWO GROUPS)

final.table$`Mean After TSST`[is.na 
                              (final.table$`Mean After TSST`)] <- 
  m.descr.table$Mean[m.descr.table$Group %in% final.table$Group] 
final.table$`Mean After TSST`[3:4] <- NA #Get's rid off repeated values.

#Means from the Third Phase
final.table$`Mean After Jokes`[is.na 
                               (final.table$`Mean After Jokes`)] <- 
  f.descr.table$Mean[f.descr.table$Group %in% final.table$Group]

#Putting the F-Values to the Table
final.table$`F-value`[is.na 
                      (final.table$`F-value`)] <- 
  F.Values$`F-Values`[F.Values$Group %in% final.table$Group]

#Putting the Significance Values to the Table
final.table$Significance[is.na 
                         (final.table$Significance)] <- 
  Sig.Values$Significace[Sig.Values$Group %in% final.table$Group]

#Exporting the Final Table-----------------------------------------------------

write.table(final.table, 
            file = paste(working.dir, "tables/Difference in Means.csv", 
                         sep = "/"), row.names=FALSE, sep = ",")