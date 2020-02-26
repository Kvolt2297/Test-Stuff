#
#
# Volodko Kirill
#
#

#Data Emulation------------------

stress.level <- c("High", "Low")
humor.type <- c("Neutral", "Disparaging")
  

#Creating an empty storage for 2x2 variables.
a.storage <- cbind.data.frame(stress.type = rep(stress.level, 
                                                rep(n.participants*2, 2)), 
                              humor.type  = rep(humor.type, 
                                                rep(n.participants, 2)),
                              start = rep(NA, 4*n.participants), 
                              middle = rep(NA, 4*n.participants),
                              finish = rep(NA, 4*n.participants))

#
#Emulating data through rnorm---------------------------------------------------                                                    
i < 1
for(i in 1:4){
  # enter for loop
  
  start.stop <- c( ((i-1) * n.participants + 1), i * n.participants)
  
  # First phase for every group has all the similar data.
  
  a.storage$start[start.stop[1]:start.stop[2]] <- round(rnorm(n.participants, 
                                        s.descr.table$Mean[i],
                                        s.descr.table$Standard.Deviation[i]),0)                            
  #Secobd phase 
  
  a.storage$middle[start.stop[1]:start.stop[2]] <- round(rnorm(n.participants, 
                                        m.descr.table$Mean[i],
                                        m.descr.table$Standard.Deviation[i]),0)
  
  #Third phase is for all, but every group has diffirent values
  
  a.storage$finish[start.stop[1]:start.stop[2]] <- round(rnorm(n.participants, 
                                        f.descr.table$Mean[i],
                                        f.descr.table$Standard.Deviation[i]),0)
}
#Exporting rnorm data-------------------------------

#Normal Distribution Data
write.table(a.storage, 
            file = paste(working.dir, "data/Normal Distribution.csv", 
                         sep = "/"),
            row.names=FALSE, sep = ",")
