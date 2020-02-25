#
#
#
#
# Making Raw Data Table
#
#
#
#


#Making function for random data points for each of the group-----------------

stress.level <- function(x, y) {
  round(runif((n.participants), min = x, max = y), 0)  #runif selects random 
  #numbers from a certain range
}
#Testing the function
stress.level(1, 10)

#Stress before the experiment begins (this invollves all of the participants)

#storage <- vector("numeric", 100) < Attempted to make a storage area with 
#100 empty spaces and fill it with NA

#Creating names for the group. Each of the names are followed respectively.
names.groups <- c("Stressed Neutral", "Stress Disparaging", "No Stress Neutral", 
                  "No Stress Disparaging")
#Storage for 4 groupsof n.participants. Fill it with NA's
storage <- cbind.data.frame(group = rep(names.groups, rep(n.participants,4)), 
                            start = rep(NA, 4*n.participants), 
                            middle = rep(NA, 4*n.participants),
                            finish = rep(NA, 4*n.participants)
)
head(storage) #Check if it works

#Giving each group stress level. I can change estimated values from here if I 
#want to change it in a loop.

#Stress at the beggining of the experiment
stress.start <- stress.level (0, 2)

#Stress after going through Treir Social Stress Test
stress.TSST.neutral <- stress.level(4.5, 8)
stress.TSST.disparaging <- stress.level(4.5, 8)

#Assigning stress level after hearing jokes in four different groups
stress.after.neutral <- stress.level(3, 6)
stress.after.disparaging <- stress.level(2, 5)

stress.free.after.neutral <- stress.level(0,2)
stress.free.after.disparaging <- stress.level(0,2)

#Credits to Thor Veen for assisting me with this code
for(i in 1:4){
  #Enter for loop
  # i <- 1 #Reset loop
  start.stop <- c( ((i-1) * n.participants + 1), i * n.participants)
  
  #Storing the data into the 1st columns
  storage$start[start.stop[1]:start.stop[2]] <- stress.start
  
  #Storing the data into the 2nd column - ONLY STRESSED GROUP. That's first 
  #half of the participants
  while (i <= 2) { #Telling the code to continue running if i is below or equals
    #to 2
    storage$middle[start.stop[1]:start.stop[2]] <- stress.level(3,7)
    break }
  #stop running this code if I is bigger than 2. }
  
  storage$finish[start.stop[1]:start.stop[2]] <- stress.level(3,6) 
  
  #Storing the data into the 3rd column - after reading jokes. I predict that each
  #group would have different stress ranges. Thus I need to assign unique stress
  #levels for four groups. 
  while (i == 1) { #First group - Stressed Neutral
    storage$finish[start.stop[1]:start.stop[2]] <- stress.after.neutral
    break }
  
  while (i == 2) { #First group - Stressed Disparaging
    storage$finish[start.stop[1]:start.stop[2]] <- stress.after.disparaging
    break
  }
  
  while (i == 3) { #First group - No Stress Neutral
    storage$finish[start.stop[1]:start.stop[2]] <- stress.free.after.neutral
    break
  }
  
  while (i == 4) { #First group - No Stress Disparaging
    storage$finish[start.stop[1]:start.stop[2]] <- stress.free.after.disparaging
    break
  }
} #Provides me with stress levels for each row and column!  

#
#Exporting Raw Data---------------------------------- 
write.table(storage, 
            file = paste(working.dir, "data/RawDataSimulation.csv", sep = "/"),
            row.names=FALSE, sep = ",")

#tried to use name indexing to make R look at the name of the row and give me 
#collumn values from that row. But since the name repeats itself 25 times, 
# I get incorrect number of dimensions error
#rownames(storage)
#storage$start["Stressed neutral", ]

head(storage) #Check jhow it works.
stress.start <- stress.level (0, 2)

