#
#
#
#Personal Project
#
#
#
#

#Simulating the data
#I will need 3 different columns for 4 tables. 
#Columns = Stress before the experiment, stress after TSST and stress after
#reading jokes.

#Tables = No stress/Neutral, No stress/disparaging, stress/neutral
#stress/disparaging

#Placeholder group to put in values in the future
experiment <- c(NA)

#Making the groups. 25 participants for each (100 in total).
total.participants <- 100  #total amount of participants in the experiment



n.participants <- 25 #how any participants in each group

#Assigning the participants to each of the group
part.stress.neutral <- n.participants
part.stress.disparaging <- n.participants 
part.stress.free.neutral <- n.participants 
part.stress.free.disparaging <- n.participants 

testing <- data.frame(c(1:25),stress.TSST.neutral, stress.TSST.disparaging)

row(part.stress.neutral)

testing <- rbind (part.stress.neutral, part.stress.free.disparaging, 
      part.stress.free.neutral, part.stress.free.disparaging)


      cbind(stress.TSST.neutral, stress.TSST.disparaging)



#Making function for random data points for each of the group.
#Likert scale only works with integers. Our Likert scale is from 
#0 (not stressed at all) to 10 (overhmelmingly stressed). 

stress.level <- function(x, y) {
round(runif((n.participants), min = x, max = y), 0)  #runif selects random 
  #numbers from a certain range
}

#Testing the function
stress.level(1, 10)

#Stress before the experiment begins (this invollves all of the participants)

#storage <- vector("numeric", 100) < Attempted to make a storage area with 
#100 empty spaces and fill it with NA
names.groups <- c("Stressed Neutral", "Stress Disparaging", "No Stress Neutral", 
                  "No Stress Disparaging")
#Storage for 4 groupsof n.participants. Fill it with NA's
storage <- cbind.data.frame(group = rep(names.groups, rep(n.participants,4)), 
                 start = rep(NA, 4*n.participants), 
                 middle = rep(NA, 4*n.participants),
                 finish = rep(NA, 4*n.participants)
                 )

head(storage)
for(i in 1:4){
  #Enter for loop
  # i <- 1 #Reset loop
  start.stop <- c( ((i-1) * n.participants + 1), i * n.participants)
                   
  #Storing the data into the 1st columns
  storage$start[start.stop[1]:start.stop[2]] <- stress.level(0,2)
  
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
while (i <= 1) { #First group - Stressed Neutral
  storage$finish[start.stop[1]:start.stop[2]] <- stress.level(3,6)
  break }

while (i == 2) { #First group - Stressed Disparaging
  storage$finish[start.stop[1]:start.stop[2]] <- stress.level(3,5)
  break
}

while (i == 3) { #First group - No Stress Neutral
  storage$finish[start.stop[1]:start.stop[2]] <- stress.level(1,4)
  break
}

if (i == 4) { #First group - No Stress Disparaging
  storage$finish[start.stop[1]:start.stop[2]] <- stress.level(1,4)
  break
}
} #Provides me with stress levels for each row and column!  

 
#tried to use name indexing to make R look at the name of the row and give me 
#collumn values from that row. But since the name repeats itself 25 times, 
# I get incorrect number of dimensions error
#rownames(storage)
#storage$start["Stressed neutral", ]


start.stop
#After the TSST test (stress condition only, which is half of them)

#Farmers way
stress.TSST.neutral <- stress.level(4, 8)
stress.TSST.disparaging <- stress.level(4, 8)

#Assigning stress level after hearing jokes in four different groups
stress.after.neutral <- stress.level(3, 7)
stress.after.disparaging <- stress.level(2, 6)

stress.free.after.neutral <- stress.level(1,3)
stress.free.after.disparaging <- stress.level(1,3)

