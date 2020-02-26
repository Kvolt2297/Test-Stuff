#
#
#Volodko Kirill
#
#

#Making a difference in Mean and ANOVA results table---------------------------

final.table <- cbind.data.frame(Group = rep(names.groups, 1),
                                "Mean Before Experiment" = rep(NA, 4), 
                                "Mean After TSST" = rep(NA, 4),
                                "Mean After Jokes" = rep(NA, 4))
                               

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


#Means from the Third Phase
final.table$`Mean After Jokes`[is.na 
                               (final.table$`Mean After Jokes`)] <- 
  f.descr.table$Mean[f.descr.table$Group %in% final.table$Group]

#This Used to be only for one way anova. 
# #Putting the F-Values to the Table
# final.table$`F-value`[is.na 
#                       (final.table$`F-value`)] <- 
#   F.Values$`F-Values`[F.Values$Group %in% final.table$Group]
# 
# #Putting the Significance Values to the Table
# final.table$Significance[is.na 
#                          (final.table$Significance)] <- 
#   Sig.Values$Significace[Sig.Values$Group %in% final.table$Group]

#Table for the ANOVA Results = F-Values---------------------------------------
anova.f.table <- cbind.data.frame(Group = rep(anova.names, 1),
                                F.Value.Start= rep(NA, 3), 
                                F.Value.Middle = rep(NA, 3),
                                F.Value.Finish = rep(NA, 3))
#F,Value Start
anova.f.table$F.Value.Start[is.na 
                               (anova.f.table$F.Value.Start)] <- 
  F.Values.s$`F-Value.Start`[F.Values.s$Group %in% anova.f.table$Group]

#F,Value Middle
anova.f.table$F.Value.Middle[is.na 
                                (anova.f.table$F.Value.Middle)] <- 
  F.Values.m$`F-Value.Middle`[F.Values.m$Group %in% anova.f.table$Group]

#F,Value Start
anova.f.table$F.Value.Finish[is.na 
                                (anova.f.table$F.Value.Finish)] <- 
  F.Values.f$`F-Value.Finish`[F.Values.f$Group %in% anova.f.table$Group]

#Table for the ANOVA Results = "Pr(>F)"---------------------------------------
anova.sg.table <- cbind.data.frame(Group = rep(anova.names, 1),
                                      "Pr(>F).Start"  = rep(NA, 3), 
                                      "Pr(>F).Middle" = rep(NA, 3),
                                      "Pr(>F).Finish" = rep(NA, 3))
#F,Value Start
anova.sg.table$`Pr(>F).Start`[is.na 
                                (anova.sg.table$`Pr(>F).Start`)] <- 
  Sig.Values.s$`Pr(>F).Start`[Sig.Values.s$Group %in% anova.sg.table$Group]

#F,Value Middle
anova.sg.table$`Pr(>F).Middle`[is.na 
                              (anova.sg.table$`Pr(>F).Middle`)] <- 
  Sig.Values.m$`Pr(>F).Middle`[Sig.Values.m$Group %in% anova.sg.table$Group]

#F,Value Start
anova.sg.table$`Pr(>F).Finish`[is.na 
                              (anova.sg.table$`Pr(>F).Finish`)] <- 
  Sig.Values.f$`Pr(>F).Finish`[Sig.Values.f$Group %in% anova.sg.table$Group]

#Exporting the Final Table-----------------------------------------------------

#Exporting difference in Means
write.table(final.table, 
            file = paste(working.dir, "tables/Difference in Means.csv", 
                         sep = "/"), row.names=FALSE, sep = ",")
#Exporting F-Vakues
write.table(anova.f.table, 
            file = paste(working.dir, "tables/F Values.csv", 
                         sep = "/"), row.names=FALSE, sep = ",")
#Exporing Pr(>F)
write.table(anova.sg.table, 
            file = paste(working.dir, "tables/Significance.csv", 
                         sep = "/"), row.names=FALSE, sep = ",")


