#
# get weather data from Canada
# using library 'weathercan' 
# https://github.com/ropensci/weathercan
#

# this code is linked with the 'Reproducible R project' worksheet

# ---- A obtain data from multiple stations ------------------------------------
# find a station
# Squamish
st.t1 <- stations_search("Squamish", interval = "day") # 336
head(st.t1)
# Pemberton
st.t2 <- stations_search("Pemberton", interval = "day") # 46747
head(st.t2)

# download data 
# Squamish data
w.Squam <- weather_dl(station_ids = 336, start = "2017-11-01", 
                      end = "2018-04-30", interval = "day")
str(w.Squam)
# send to data folder
write.csv(w.Squam, paste(path.data,'Squamish.csv', sep = ""))

# Pemberton data
w.Pem <- weather_dl(station_ids = 536, start = "2017-11-01", 
                    end = "2018-04-30", interval = "day")
str(w.Pem)
# send to data folder
write.csv(w.Pem, paste(path.data,'Pemberton.csv', sep = ""))

# ---- B visualise data ------------------------------------
# maximum temperature in Squamish
str(w.Squam)
pdf(paste(path.figures, "max.temp.Squamish.pdf", sep = ""), width = 7, 
    height = 5)
plot(w.Squam$date, w.Squam$max_temp, pch= 19, col = col.trans[1], las = 1,
     ylab = "max temperature (Celsius)", xlab = "date")
dev.off()
list.files(path.figures)
# maximum temperature in Pemberton
pdf(paste(path.figures, "max.temp.Pemberton.pdf", sep = ""), width = 7, 
    height = 5)
plot(w.Pem$date, w.Pem$max_temp, pch= 19, col = col.trans[1], las = 1,
     ylab = "max temperature (Celsius)", xlab = "date")
dev.off()


# ---- C correlation between stations ------------------------------------
# look at the correlation between Squamish and Pemberton
pdf(paste(path.figures, "correlation.temp.pdf", sep = ""), width = 5, 
    height = 5)
plot(w.Squam$max_temp, w.Pem$max_temp, pch= 19, col = col.trans[2], las = 1,
     ylab = "Pemberton temperature (C)", xlab = "Squamish temperature (C)")
dev.off()

# let's calculate the correlation coefficient
cor(w.Pem$max_temp, w.Squam$max_temp)
# throws and errors. Check NA's
is.na(w.Pem$max_temp) # NAs present
# exclude the NAs
na.S <- which(is.na(w.Squam$max_temp))
na.P <- which(is.na(w.Pem$max_temp))
w.Squam$max_temp[29]# quick check
# which ones are missing?
missing.combined <- unique(c(na.S, na.P))

# delete the rows with missing values from both data sets
w.Squam.t <- w.Squam[-missing.combined,]
which(is.na(w.Squam.t$max_temp)) # indeed, no more missing values
w.Pem.t <- w.Pem[-missing.combined,]
which(is.na(w.Pem.t$max_temp))  # indeed, no more missing values

# redo the correlation
model.corr.temp <- cor(w.Pem.t$max_temp, w.Squam.t$max_temp)
summary(model.corr.temp)
names(model.corr.temp) <- "corr.coef"
# let's save it to the output
write.csv(model.corr.temp, paste(path.data.output, "corr.coeff.WOWwer.csv", 
                                 sep = ""), row.names = FALSE)


# COFFFEEEEEEE TIME!

# ====== The End ===============================================================

