#Kirill Volodko
#
#
#Personal Project
#
#
#
#
#Plainning--------------------------------------------------------------------
#Simulating the data
#I will need 3 different columns for 4 tables. 
#Columns = Stress before the experiment, stress after TSST and stress after
#reading jokes.

#Tables = No stress/Neutral, No stress/disparaging, stress/neutral
#stress/disparaging

#Making the groups. 25 participants for each (100 in total).
total.participants <- 100  #total amount of participants in the experiment
n.participants <- total.participants/4 #how any participants in each group

#Failed Attempts------------------------------------------------------------
#My attempts at manually inputting participant number into specific groups and 
#then placing binding them into rows... it doesn't work like that 
#
#Assigning the participants to each of the group 
# part.stress.neutral <- n.participants
# part.stress.disparaging <- n.participants 
# part.stress.free.neutral <- n.participants 
# part.stress.free.disparaging <- n.participants 
# 
# testing <- data.frame(c(1:25),stress.TSST.neutral, stress.TSST.disparaging)
# 
# row(part.stress.neutral)
# 
# testing <- rbind (part.stress.neutral, part.stress.free.disparaging, 
#       part.stress.free.neutral, part.stress.free.disparaging)
# 
# 
#       cbind(stress.TSST.neutral, stress.TSST.disparaging)


#Making a table for raw data------------------------------------------------

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
stress.TSST.neutral <- stress.level(4, 8)
stress.TSST.disparaging <- stress.level(4, 8)

#Assigning stress level after hearing jokes in four different groups
stress.after.neutral <- stress.level(3, 7)
stress.after.disparaging <- stress.level(2, 6)

stress.free.after.neutral <- stress.level(1,3)
stress.free.after.disparaging <- stress.level(1,3)

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
while (i <= 1) { #First group - Stressed Neutral
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

if (i == 4) { #First group - No Stress Disparaging
  storage$finish[start.stop[1]:start.stop[2]] <- stress.free.after.disparaging
  break
}
} #Provides me with stress levels for each row and column!  

#tried to use name indexing to make R look at the name of the row and give me 
#collumn values from that row. But since the name repeats itself 25 times, 
# I get incorrect number of dimensions error
#rownames(storage)
#storage$start["Stressed neutral", ]
head(storage)
tail(storage)
stress.start <- stress.level (0, 2)


#Working on Descriptives---------------------------------------------------

#Manual checking of descripitve data 
mean(storage$start) # 1.08
sd(storage$start) #0.631
length(storage$start) #100

#Making an empty table to fill
descr.name <- c("Size", "Mean", "Standard Deviation")
descr.table <- cbind.data.frame(group = rep(descr.name, 1),
                                Size = rep(NA, 3), Mean = rep(NA, 3),
                                Standard.Deviation = rep(NA, 3)
                                )

#Calculating mean, sd and frequency of the the stress with a function. 
descriptives <- function(x) {

  Size <- length(x)
  Mean <- mean(x, na.rm = TRUE) #Since we have NA's, we need to exclude them 
                                  #from calculation
  Standard_Deviation <- round(sd(x, na.rm = TRUE), 2) 
  
 discriptive.info <-cbind.data.frame(
                   Size, 
                   Mean, 
                   Standard_Deviation)
}

#Checking if the function worked or not

#EDIT: I came to realization that I will need descriptive statistic for every
#phase for EVERY group. I need to figure out a smart way to this process.
start.descr<-descriptives(storage$start)

#Descriptive calculation for every phase and group (Farmer's way)-------------
#Calculating Stress Before The Experiment
#After reading neutral jokes after TSST
s.descr.stress.neutral <- descriptives(storage$start[1:25])
print(f.descr.free.neutral)

#After reading disparaging jokes after TSST
s.descr.stress.disparaging <- descriptives(storage$start[26:50])
print(f.descr.free.disparaging)

#After reading neutral jokes in without stress tests
s.descr.free.neutral<- descriptives(storage$start[51:75])
print(f.descr.free.neutral)

#After reading disparaging jokes without stress tests
s.descr.free.disparaging <- descriptives(storage$start[76:100])
print(f.descr.free.disparaging)




#Finding descriptives for middle column. There are missing values that I have to
#deal with. Since there are two seprate groups, I need to figure out how to 
#divide them apart. The function does that automatically!

m.descr.stress.neutral<-descriptives(storage$middle[1:25])
print(descr.stress.neutral)

m.descr.stress.disparaging<-descriptives(storage$middle[26:50])
print(descr.stress.disparaging)

#Finding descriptives for the last column. I need to have descriptives for each
#row in the last phase. 
#Calculating values based on their location in the data set.

#Maybe I can create function for what I have done below?

#After reading neutral jokes after TSST
f.descr.stress.neutral <- descriptives(storage$finish[1:25])
print(f.descr.free.neutral)

#After reading disparaging jokes after TSST
f.descr.stress.disparaging <- descriptives(storage$finish[26:50])
print(f.descr.free.disparaging)

#After reading neutral jokes in without stress tests
f.descr.free.neutral<- descriptives(storage$finish[51:75])
print(f.descr.free.neutral)

#After reading disparaging jokes without stress tests
f.descr.free.disparaging <- descriptives(storage$finish[76:100])
print(f.descr.free.disparaging)

#Creating a loop to craft a descriptive table for every group------------------
for(i in 1:4){
  
  start.stop <- c( ((i-1) * n.participants + 1), i * n.participants)
  # sim for the start dates
  storage$start[start.stop[1]:start.stop[2]] 
  <- rnorm(start.descr$Size, start.descr$Mean, start.descr$Standard_Deviation)
  
  # sim for the end values
  storage$end[start.stop[1]:start.stop[2]] 
  <- rnorm(n.participants, mean.gr.end[i], sd.gr.end[i])
}

#Experimenting witg write.table function
write.table(storage, file ="Test Stuff")



