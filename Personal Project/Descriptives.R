#
# 
#Volodko Kirill
#
#
#
#Working on Creating Descriptive Table (Sample Size, Mean and SD)--------------

#Manual checking of descripitve data 
mean(storage$start) 
sd(storage$start) 
length(storage$start) 

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
#
#Finding descriptives for the first phase-------------------------------------
# s.descr.stress.neutral <- descriptives(storage$start[1:25])
# print(s.descr.free.neutral)
# 
# #After reading disparaging jokes after TSST
# s.descr.stress.disparaging <- descriptives(storage$start[26:50])
# print(s.descr.free.disparaging)
# 
# #After reading neutral jokes in without stress tests
# s.descr.free.neutral<- descriptives(storage$start[51:75])
# print(s.descr.free.neutral)
# 
# #After reading disparaging jokes without stress tests
# s.descr.free.disparaging <- descriptives(storage$start[76:100])
# print(s.descr.free.disparaging)

#
#Finding descriptives for middle column---------------------------------------- 
# 
# m.descr.stress.neutral<-descriptives(storage$middle[1:25])
# print(m.descr.stress.neutral)
# 
# m.descr.stress.disparaging<-descriptives(storage$middle[26:50])
# print(m.descr.stress.disparaging)

#
#Finding descriptives for the last column.------------------------------------- 
# #
# #After reading neutral jokes after TSST
# f.descr.stress.neutral <- descriptives(storage$finish[1:25])
# print(f.descr.stress.neutral) #Check values
# 
# #After reading disparaging jokes after TSST
# f.descr.stress.disparaging <- descriptives(storage$finish[26:50])
# print(f.descr.stress.disparaging) #Check values
# 
# #After reading neutral jokes in without stress tests
# f.descr.free.neutral<- descriptives(storage$finish[51:75])
# print(f.descr.free.neutral) #Check values
# 
# #After reading disparaging jokes without stress tests
# f.descr.free.disparaging <- descriptives(storage$finish[76:100])
# print(f.descr.free.disparaging) #Check values


#
#Loop for finding the descriptives for every group in every phase--------------
for(i in 1:4) {
  #Enter for loop
  # i <- 1 #Reset loop
  start.stop <- c( ((i-1) * n.participants + 1), i * n.participants)
  
  while(i == 1) { #Stress/Neutral Groups
    
    #Find the descriptives for the first 25 rows of the first phase.
    s.descr.stress.neutral <- descriptives(
      storage$start[start.stop[1]:start.stop[2]])
    
    #Find the descriptives for the first 25 rows of the second phase.
    m.descr.stress.neutral<-descriptives(
      storage$finish[start.stop[1]:start.stop[2]])
    
    #Find the descriptives for the first 25 rows of the last phase.
    f.descr.stress.neutral <- descriptives(
      storage$finish[start.stop[1]:start.stop[2]])
    break     } 
  
  while(i == 2) { #Stress/Disparaging Groups
    #Find the descriptives from 26-50th row of the first phase. 
    s.descr.stress.disparaging <- descriptives(
      storage$start[start.stop[1]:start.stop[2]])
    
    #Find the descriptives from 26-50th row of the second phase.
    m.descr.stress.disparaging<-descriptives(
      storage$finish[start.stop[1]:start.stop[2]])
    
    #Find the descriptives from 26-50th row of the last phase.
    f.descr.stress.disparaging  <- descriptives(
      storage$finish[start.stop[1]:start.stop[2]])
    break     } 
  
  while(i == 3) { #Stress Free/Neutral Groups
    #Find the descriptives from 51-75th row of the first phase.
    s.descr.free.neutral <- descriptives(
      storage$start[start.stop[1]:start.stop[2]])
    
    #Find the descriptives from 51-75th row of the second phase.
    m.descr.free.neutral<-descriptives(
      storage$middle[start.stop[1]:start.stop[2]])
    
    #Find the descriptives from 51-76th row of the last phase.
    f.descr.free.neutral <- descriptives(
      storage$finish[start.stop[1]:start.stop[2]])
    break     }
  
  
  while(i == 4) { #Stress Free/Disparaging Groups
    
    #Find the descriptives from 76-100th row of the first phase.
    s.descr.free.disparaging <- descriptives(
      storage$start[start.stop[1]:start.stop[2]])
    
    #Find the descriptives from 76-100th  row of the second phase.
    m.descr.free.disparaging<-descriptives(
      storage$middle[start.stop[1]:start.stop[2]])
    
    #Find the descriptives from 76-100th row of the last phase.
    f.descr.free.disparaging <- descriptives(
      storage$finish[start.stop[1]:start.stop[2]])
    break     } 
  
}





#
#Creating a loop to craft a descriptive table for every group------------------

#Making an empty tables to fill them up later on.
#Create a descriptive stable to for every group in the every phase 
descr.name <- c("Stressed Neutral", "Stress Disparaging", "No Stress Neutral", 
                "No Stress Disparaging")

#Phase Start
s.descr.table <- cbind.data.frame(Group = rep(descr.name, 1),
                                  Size = rep(NA, 4), Mean = rep(NA, 4),
                                  Standard.Deviation = rep(NA, 4)
)
#Phase Middle 
m.descr.table <- cbind.data.frame(Group = rep(descr.name, 1),
                                  Size = rep(NA, 2), Mean = rep(NA, 2),
                                  Standard.Deviation = rep(NA, 2)
)
#Phase Final
f.descr.table <- cbind.data.frame(Group = rep(descr.name, 1),
                                  Size = rep(NA, 4), Mean = rep(NA, 4),
                                  Standard.Deviation = rep(NA, 4)
)

#
#Experimenting with combining the data frames [FAILED]-------------------------

#Did not work out. It is due that descr.table has more columns than other datas
# testdata <- rbind(descr.table,  
#                 s.descr.stress.neutral, 
#                 s.descr.stress.disparaging, 
#                 s.descr.free.neutral, 
#                 s.descr.stress.disparaging)

#
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




#
#Manual Labor on assigning the Phase Start values-----------------------------

#EDIT: I came up with better method in the next section,
#
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

#
#Looping the descriptive process--------------------------------------------
#
#Phase 1 has all of the groups (size, mean and sd are all equal to each other)
phase1 <-rbind(s.descr.stress.neutral, 
               s.descr.stress.disparaging, 
               s.descr.free.neutral, 
               s.descr.free.disparaging)
#Phase 2 is only applicable to stress groups. So only 2 groups would apply here
phase2 <- rbind(m.descr.stress.neutral, 
                m.descr.stress.disparaging,
                m.descr.free.neutral,
                m.descr.free.disparaging)

#Last phase involves all of the groups (mean and sd are different in 
#every group)
phase3 <- rbind(f.descr.stress.neutral, 
                f.descr.stress.disparaging, 
                f.descr.free.neutral, 
                f.descr.free.disparaging)

for(i in 1:3){
  s.descr.table[1:4, i+1] = phase1[1:4, i] #Start
  m.descr.table[1:4, i+1] = phase2[1:4, i] #Middle
  f.descr.table[1:4, i+1] = phase3[1:4, i] #Final
}

#check if they work. Their values should match
head(phase2) 
head(m.descr.table) #It works 

#
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


#
#
#
#
