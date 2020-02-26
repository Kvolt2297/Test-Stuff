#
#
#Volodko Kirill
#
#
#Running Anova -------------------------------------------------

#Testing how I could run  ANOVA--------------------------------------

#EDIT: Since we are using 2-way ANOVA, this won't come useful for this part.
# testdata<-stack(a.storage) #stacks data differently. 
# anova.result <- aov(values ~ ind, data = testdata)
# summary(anova.result)

#
#Separating each group data for the ANOVA analysis.------------------------- 

#Note: I am using original storage data, as if I am working with raw data.

#Allocating values for each group. Helps me to keep track of them in the future
# storage.stress.n = a.storage[1:25, 2:4] #Stress Neutral
# storage.stress.d = a.storage[26:50, 2:4] #Stress Disparaging
# storage.free.n = a.storage[51:75, c(2,4)] #No stress Neutral. No middle column.
# storage.free.d = a.storage[76:100, c(2,4)] #No stress Disparaging. No middle.

#Making a loop for separating groups from data manipulation--------------------
#EDIT: This loop is not useful anymore, as it was designed for one-way ANOVA
# for(i in 1:4){
#   #enter for loop
#   start.stop <- c( ((i-1) * n.participants + 1), i * n.participants)
# # Stress Neutral
#   while(i == 1) {
#   storage.stress.n <- a.storage[start.stop[1]:start.stop[2], 1:5] 
#   
#   break
#   }
# # Stress Disparaging
#   while(i == 2){
#     storage.stress.d <- a.storage[start.stop[1]:start.stop[2], 1:5]   
#   break
#   }
# # No stress Neutral. 
#   while(i == 3){
#     storage.free.n <- a.storage[start.stop[1]:start.stop[2], c(1:5)]   
#     break
#   }
# # No stress Disparaging.   
#   while(i == 4){
#     storage.free.d <- a.storage[start.stop[1]:start.stop[2], c(5:5)]   
#     break
#   }
# } 


#DETLA STRESS LEVELS

#
#Calculating ANOVA for each group manually-----------------------------------
#

anova2.start <- aov(start~stress.type*humor.type, 
                       data = a.storage) #Using values from norm distribution.
summary(anova2.start)

anova2.middle <- aov(middle~stress.type*humor.type, 
                    data = a.storage)
summary(anova2.middle)

anova2.finish <- aov(finish~stress.type*humor.type, 
                     data = a.storage)
summary(anova2.finish)

#
#Trying to Figure Out how to run Anova [FAILED]--------------------------------

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


#
#Optimising ANOVA process----------------------------------------------------
#
#EDIT: For some reason, after re-testing it gives me a list of 1, instead of 13
#EDIT 2: I manage to fix it, by removing summary() from the function. Don't 
#know why it works like that
#
#EDIT 3: I've used this function for one-way anova. But it doesn't seem to work
#well for two way ANOVA. I dropped it for time sake.
# anova2.results <- function(x) {
#   x <-aov(x~stress.type*humor.type, 
#           data = a.storage) #Running anova analysis
# }
# 
# #Check if it matches with the manual input. 
# #Stress Neutral Group
# anova.result.sn <- anova2.results(start)  
# summary(anova.result.sn)
# #Values match, The function works.
# 
# #Assigning the anova values for the rest of the groups
# #Stress Disparaging
# anova.result.sd <- anova2.results(middle)
# 
# #No Stress Neutral
# anova.result.fn <- anova.results(storage.free.n)
# 
# #No Stress Disparaging
# anova.result.fd <- anova.results(storage.free.d)
# 
# #Figuring out where Pr(>F) value lies in the anova.result.sn
# names(anova.result.sn)
# anova.result.sn$residuals

#
#Trying to extract values within the ANOVA and put on the table [FAILED]-------
# experiment <- cbind(c(anova.result.sn, anova.result.sd, 
#                       anova.result.fn, anova.result.fd))
# experiment <- rbind(c(anova.result.sn, anova.result.sd, 
#                       anova.result.fn, anova.result.fd))


#
#Found two ways to extract ANOVA values---------------------------------------
#
#Method One---------------------------------
# experiment <- summary(anova.result.sn)
# str(experiment)
# potato <- experiment[[1]]$`Pr(>F)` #works out.


#Method Two [preffered]-------------------------
summary(anova2.start)[[1]][["Pr(>F)"]] # Also seems to extract Pr(>F) values
#This method is more prefered by me. 

#Extracts F-Values-----------------------------------------------------------

anova.names <- c("Stress Type", "Humor Type", "Stress & Humor Type")
#Creating a F.Values data-frame. Taking off any NA and rounding values by 2s.f.
F.Values.s <-data.frame(c(anova.names), na.omit(round(
  summary(anova2.start)[[1]][["F value"]], 2)))

F.Values.m <-data.frame(c(anova.names), na.omit(round(
  summary(anova2.middle)[[1]][["F value"]], 2)))

F.Values.f <-data.frame(c(anova.names), na.omit(round(
  summary(anova2.finish)[[1]][["F value"]], 2)))


#Renaming the Columns for easier use in future
colnames(F.Values.s) <- c("Group","F-Value.Start")
colnames(F.Values.m) <- c("Group","F-Value.Middle")
colnames(F.Values.f) <- c("Group","F-Value.Finish")

#Doing the same for significance values, without rounding
Sig.Values.s <-data.frame(c(anova.names), na.omit(
                            summary(anova2.start)[[1]][["Pr(>F)"]]))

Sig.Values.m <-data.frame(c(anova.names), na.omit(
                            summary(anova2.middle)[[1]][["Pr(>F)"]]))

Sig.Values.f <-data.frame(c(anova.names), na.omit(
                            summary(anova2.finish)[[1]][["Pr(>F)"]]))
#Renaming the Columns for easier use in future
colnames(Sig.Values.s) <- c("Group","Pr(>F).Start")
colnames(Sig.Values.m) <- c("Group","Pr(>F).Middle")
colnames(Sig.Values.f) <- c("Group","Pr(>F).Finish")

