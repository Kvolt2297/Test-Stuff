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

#Sorting Out Folders-----------------------------------------------------------

working.dir <- getwd()

#Create folders Names
output.folder.names <- c("graphs", "tables", "data")

#Loop to creat the folders
for(i in 1:length(output.folder.names)) 
  if(file.exists(output.folder.names[i]) == FALSE) #If the folders are not 
    #present, create the folder
    dir.create(output.folder.names[i])

#Set pathing for the folders.
path.graphs <- path.figures <- paste(working.dir, "/", 
                               output.folder.names[1], "/", sep = "")

path.tables <- paste(working.dir, "/", output.folder.names[2], "/", 
                          sep = "")

path.data <- paste(working.dir, "/", output.folder.names[3], "/", sep = "")





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

#Making the groups. 25 participants for each (100 in total).
total.participants <- 100  #total amount of participants in the experiment
n.participants <- round(total.participants/4, 0) 
#how any participants in each group. round it off to 0 s.f.


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

while (i == 4) { #First group - No Stress Disparaging
  storage$finish[start.stop[1]:start.stop[2]] <- stress.free.after.disparaging
  break
}
} #Provides me with stress levels for each row and column!  

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



#Working on Descriptives---------------------------------------------------

#Manual checking of descripitve data 
mean(storage$start) # 1.08
sd(storage$start) #0.631
length(storage$start) #100

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

start.descr<-descriptives(storage$start) #testing the descriptive function.
#The values match with my manual input. It means the descriptives() works


#Descriptive calculation for every phase and group (Farmer's way)-------------

#Finding descriptives before The Experiment-----------
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

#Finding descriptives for middle column---------------------------------------- 
#There are missing values that I have to
#deal with. Since there are two seprate groups, I need to figure out how to 
#divide them apart.

m.descr.stress.neutral<-descriptives(storage$middle[1:25])
print(descr.stress.neutral)

m.descr.stress.disparaging<-descriptives(storage$middle[26:50])
print(descr.stress.disparaging)

#Finding descriptives for the last column.------------------------------------- 

#I need to have descriptives for each row in the last phase. 
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

#Making an empty tables to fill

#Create a descriptive stable to for every group in the every phase 
descr.name <- c("Stressed Neutral", "Stress Disparaging", "No Stress Neutral", 
                "No Stress Disparaging")

#Phase start
s.descr.table <- cbind.data.frame(Group = rep(descr.name, 1),
                                 Size = rep(NA, 4), Mean = rep(NA, 4),
                                 Standard.Deviation = rep(NA, 4)
)
#Phase Middle 
m.descr.table <- cbind.data.frame(Group = descr.name[1:2],
                                Size = rep(NA, 2), Mean = rep(NA, 2),
                                Standard.Deviation = rep(NA, 2)
)
#Phase Final
f.descr.table <- cbind.data.frame(Group = rep(descr.name, 1),
                                Size = rep(NA, 4), Mean = rep(NA, 4),
                                Standard.Deviation = rep(NA, 4)
)

#Experimenting with combining the data frames [FAILED]-------------------------

#Did not work out. It is due that descr.table has more columns than other datas
# testdata <- rbind(descr.table,  
#                 s.descr.stress.neutral, 
#                 s.descr.stress.disparaging, 
#                 s.descr.free.neutral, 
#                 s.descr.stress.disparaging)

#Attempts at Making Automated Descriptive Table for All [FAILED]---------------
# testdata<-merge.data.frame(descr.table$Size, s.descr.free.disparaging$Size)
# testdata<-cbind.data.frame(descr.table$Size, s.descr.free.disparaging$Size )
# 
# phase1<-c(s.descr.stress.neutral, 
#        s.descr.stress.disparaging, 
#        s.descr.free.neutral, 
#        s.descr.free.disparaging)
# 
# phase2 <- c(m.descr.stress.neutral, 
#             m.descr.stress.disparaging)
# 
# phase3 <- c(f.descr.stress.neutral, 
#             f.descr.stress.disparaging, 
#             f.descr.free.neutral, 
#             f.descr.free.disparaging)
# i <- 1
#  for(i in 1:4){
#   start.stop <- i
#   descr.table$Size[start.stop[1]:start.stop[1]] <- phase1[i*3 + 1]
# 
#   descr.table$Mean[start.stop[1]:start.stop[1]] <- phase1[i*3 + 2]
#   
#   descr.table$Standard.Deviation[start.stop[1]:start.stop[1]] <- phase1[i*3 + 3]
#  }
# 
# 
# i <- 0
# for(i in 0:4){
#   start.stop <- i
#   descr.table$Size[start.stop[i]] <- phase3[i*3 + 1]
#   
#   descr.table$Mean[start.stop[i]] <- phase3[i*3 + 2]
#   
#   descr.table$Standard.Deviation[start.stop[i]] <- phase3[i*3 + 3]
# }
# start.stop <- c( ((i-1) * n.participants + 1), i * n.participants)

#Storing the data into the 1st columns
storage$start[start.stop[1]:start.stop[2]] <- stress.start




#Manual Labor on assigning the Phase Start values-----------------------------
#Size
# s.descr.table[1,2] = s.descr.stress.neutral[1,1]
# s.descr.table[2,2] = s.descr.stress.disparaging[1,1]
# s.descr.table[3,2] = s.descr.free.disparaging[1,1]
# s.descr.table[4,2] = s.descr.free.disparaging[1,1]
# #Mean
# s.descr.table[1,3] = s.descr.stress.neutral[1,2]
# s.descr.table[2,3] = s.descr.stress.disparaging[1,2]
# s.descr.table[3,3] = s.descr.free.disparaging[1,2]
# s.descr.table[4,3] = s.descr.free.disparaging[1,2]
# 
# #SD 
# s.descr.table[1,4] = s.descr.stress.neutral[1,3]
# s.descr.table[2,4] = s.descr.stress.disparaging[1,3]
# s.descr.table[3,4] = s.descr.free.disparaging[1,3]
# s.descr.table[4,4] = s.descr.free.disparaging[1,3]

#Looping the descriptive process--------------------------------------------
#
#Phase 1 has all of the groups (size, mean and sd are all equal to each other)
phase1 <-rbind(s.descr.stress.neutral, 
          s.descr.stress.disparaging, 
          s.descr.free.neutral, 
          s.descr.free.disparaging)
#Phase 2 is only applicable to stress groups. So only 2 groups would apply here
phase2 <- rbind(m.descr.stress.neutral, 
                m.descr.stress.disparaging)

#Last phase involves all of the groups (mean and sd are different in 
#every group)
phase3 <- rbind(f.descr.stress.neutral, 
              f.descr.stress.disparaging, 
              f.descr.free.neutral, 
              f.descr.free.disparaging)

for(i in 1:3){
s.descr.table[1:4, i+1] = phase1[1:4, i] #Start
m.descr.table[1:2, i+1] = phase2[1:2, i] #Middle
f.descr.table[1:4, i+1] = phase3[1:4, i] #Final
}

#check if they work. Their values should match
head(phase2) 
head(m.descr.table) #It works 

#Exporting Descriptives to table folder----------------------------------------

#Descriptives at the start of the experiment
write.table(s.descr.table, 
           file = paste(working.dir, "tables/Before.Exp.csv", sep = "/"),
           row.names=FALSE, sep = ",")
#Descriptives after TSST (only stress groups)
write.table(m.descr.table, 
           file = paste(working.dir, "tables/TSST.levels.csv", sep = "/"),
           row.names=FALSE, sep = ",")
#Descriptives after reading jokes
write.table(f.descr.table, 
          file = paste(working.dir, "tables/Jokes.levels.csv", sep = "/"),
          row.names=FALSE, sep = ",")


#Data Emulation------------------
d.storage <- cbind.data.frame(group = rep(names.groups, rep(n.participants,4)), 
                              start = rep(NA, 4*n.participants), 
                              middle = rep(NA, 4*n.participants),
                              finish = rep(NA, 4*n.participants)
)

#Emulating data through rnorm---------------------------------------------------                                                    
for(i in 1:4){
# enter for loop

start.stop <- c( ((i-1) * n.participants + 1), i * n.participants)
# First phase for every group has all the similar data.
d.storage$start[start.stop[1]:start.stop[2]] <- round(rnorm(n.participants, 
                                        s.descr.table$Mean[i],
                                        s.descr.table$Standard.Deviation[i]),2)
#Secobd phase only for the stressed groups
while(i <= 2) {
d.storage$middle[start.stop[1]:start.stop[2]] <- round(rnorm(n.participants, 
                                        m.descr.table$Mean[i],
                                        m.descr.table$Standard.Deviation[i]),2)
break }

#Third phase is for all, but every group has diffirent values
d.storage$finish[start.stop[1]:start.stop[2]] <- round(rnorm(n.participants, 
                                        f.descr.table$Mean[i],
                                        f.descr.table$Standard.Deviation[i]),2)
}




#Running Anova I need to learn how to run ANOVA with 3 variables---------------


testdata<-stack(storage) #stacks data differently. 
anova.result <- aov(values ~ ind, data = testdata)
summary(anova.result)

#Separating each group data for the upcoming analysis.------------------------- 

#Note: I am using original storage data, as if I am working with raw data.

storage.stress.n = storage[1:25, 2:4] #Stress Neutral
storage.stress.d = storage[26:50, 2:4] #Stress Disparaging
storage.free.n = storage[51:75, c(2,4)] #No stress Neutral. Deleted the middle.
storage.free.d = storage[76:100, c(2,4)] #No stress Disparaging. No middle.

#Calculating ANOVA for each group manually-----------------------------------
#
#Anova for Stress Neutral
anova.stress.n<-stack(storage.stress.n) #Stacking Values
anova.result.sn <- aov(values ~ ind, data = anova.stress.n)
summary(anova.result.sn)
#F(2, 72) = 177, p <.05
#
#Anova for Stress Disparaging
anova.stress.d<-stack(storage.stress.d)
anova.result.sd <- aov(values ~ ind, data = anova.stress.d)
summary(anova.result.sd)
#F(2, 72) = 88, p <.05
#
anova.free.n<-stack(storage.free.n)
anova.result.fn <- aov(values ~ ind, data = anova.free.n)
summary(anova.result.fn)
#F(1, 48) = 0.225, p <. 0.616
#
anova.free.d<-stack(storage.free.d)
anova.result.fd <- aov(values ~ ind, data = anova.free.d)
summary(anova.result.fd)
#F(1, 48) = 0, p < 1

t.test(storage.free.n$start, storage.free.n$finish)

#Trying to Figure Out Anova [FAILED]---------------------------------------

#Doesn't work
# a.1 <- lm(storage$start[76-100] ~ storage$finish[76:100])
# a.2 <- lm(storage$finish[1:25] ~ 1)
# what<-anova(a.1)
# aov(d.storage$start[1:25,], d.storage$middle[1:25,], d.storage$finish[1:25,])

#testdata <- stack(storage)

#Giving anova analysis only values from specific groups from stacked data

# anova.result <- aov(values ~ ind, data = testdata[c(1:25, 101:125, 201:225)])
# 
# testdata.lm <- lm(storage$start[1:25], 
#                   storage$middle[1:25], 
#                   storage$finish[1:25])

      
#Optimising ANOVA process----------------------------------------------------
#
#EDIT: For some reason, after re-testing it gives me a list of 1, instead of 13
#EDIT 2: I manage to fix it, by removing summary() from the function. Don't 
#know why it works like that
#
anova.results <- function(x) {
  y <- stack(x) #Stacking the groups 
  y <-aov(values ~ ind, data = y) #Running anova analysis
  #Print out the results in a summary form
}

#Check if it matches with the manual input. Stress Neutral Group
anova.result.sn <- anova.results(storage.stress.n)  
summary(anova.result.sn)
#Values match, The function works.

#Stress Disparaging
anova.result.sd <- anova.results(storage.stress.d)
#No Stress Neutral
anova.result.fn <- anova.results(storage.free.n)
#No Stress Disparaging
anova.result.fd <- anova.results(storage.free.d)

#Figuring out where Pr(>F) value lies in the anova.result.sn
names(anova.result.sn)
anova.result.sn$residuals

#Trying to extract values within the ANOVA and put on the table [FAILED]-------
# experiment <- cbind(c(anova.result.sn, anova.result.sd, 
#                       anova.result.fn, anova.result.fd))
# experiment <- rbind(c(anova.result.sn, anova.result.sd, 
#                       anova.result.fn, anova.result.fd))


#Found two ways to extract ANOVA values---------------------------------------

#Method One
experiment <- summary(anova.result.sn)
str(experiment)
potato <- experiment[[1]]$`Pr(>F)` #works out.
data.frame(potato)
#Method Two
summary(anova.result.sn)[[1]][["Pr(>F)"]] # Also seems to extract Pr(>F) values
summary(anova.result.sn)[[1]][["F value"]] #Extracts F-Values
