#
#
#Volodko Kirill
#
#
#Running Anova -------------------------------------------------

#Testing how I could run ANOVA--------------------------------------
testdata<-stack(a.storage) #stacks data differently. 
anova.result <- aov(values ~ ind, data = testdata)
summary(anova.result)

#
#Separating each group data for the ANOVA analysis.------------------------- 

#Note: I am using original storage data, as if I am working with raw data.

#Allocating values for each group. Helps me to keep track of them in the future
storage.stress.n = a.storage[1:25, 2:4] #Stress Neutral
storage.stress.d = a.storage[26:50, 2:4] #Stress Disparaging
storage.free.n = a.storage[51:75, c(2,4)] #No stress Neutral. No middle column.
storage.free.d = a.storage[76:100, c(2,4)] #No stress Disparaging. No middle.

#
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
anova.results <- function(x) {
  y <- stack(x) #Stacking the groups 
  y <-aov(values ~ ind, data = y) #Running anova analysis
}

#Check if it matches with the manual input. 
#Stress Neutral Group
anova.result.sn <- anova.results(storage.stress.n)  
summary(anova.result.sn)
#Values match, The function works.

#Assigning the anova values for the rest of the groups
#Stress Disparaging
anova.result.sd <- anova.results(storage.stress.d)

#No Stress Neutral
anova.result.fn <- anova.results(storage.free.n)

#No Stress Disparaging
anova.result.fd <- anova.results(storage.free.d)

#Figuring out where Pr(>F) value lies in the anova.result.sn
names(anova.result.sn)
anova.result.sn$residuals

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
summary(anova.result.sn)[[1]][["Pr(>F)"]] # Also seems to extract Pr(>F) values
#This method is more prefered by me. 

#Extracts F-Values-----------------------------------------------------------
#Creating a F.Values data-frame. Taking off any NA and rounding values by 2s.f.
F.Values <-data.frame(c(names.groups), na.omit(round(c(
  summary(anova.result.sn)[[1]][["F value"]], 
  summary(anova.result.sd)[[1]][["F value"]], 
  summary(anova.result.fn)[[1]][["F value"]],
  summary(anova.result.fd)[[1]][["F value"]]), 2)))

#Renaming the Columns for easier use in future
colnames(F.Values) <- c("Group","F-Values")

#Doing the same for significance values, without rounding
Sig.Values <-data.frame(c(names.groups), na.omit(c(
  summary(anova.result.sn)[[1]][["Pr(>F)"]], 
  summary(anova.result.sd)[[1]][["Pr(>F)"]], 
  summary(anova.result.fn)[[1]][["Pr(>F)"]],
  summary(anova.result.fd)[[1]][["Pr(>F)"]]), ))

#Renaming the Columns for easier use in future
colnames(Sig.Values) <- c("Group","Significace")