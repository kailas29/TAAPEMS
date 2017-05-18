# ------------------------------
# read summary sheets of UK data
# ------------------------------

library(readxl)
library(plyr)
library(ggplot2)

setwd('D:/cyclebeating/Analysis UTAC KBA data/UK PEMS/')

paths <- c('D:/cyclebeating/Analysis UTAC KBA data/UK PEMS/euro-5-vehicle-data/',
           'D:/cyclebeating/Analysis UTAC KBA data/UK PEMS/euro-6-vehicle-data/')

uk.results <- data.frame()
file <- "euro-6-audi-a3-1598.xlsx"
for (path in paths) {
  xlsx.files <- list.files(path)
  
  for (file in xlsx.files) {
    
    #sheets <- excel_sheets(paste0(path, file))
  
    data <- read_excel(paste0(path, file), sheet = 1)
    res <- data.frame(brand = data[9, 2],
                      model = data[9, 3],
                      cylvol = data[9, 4],
                      norm = paste0('Euro', data[9, 7]),
                      declared.co2 = data[9, 11], 
                      lab.atm.pres.Pa = as.numeric(data[15, 3]) * 1000,
                      lab.humidity = as.numeric(data[15, 6]),
                      lab.temp.degC = as.numeric(data[15, 9]),
                      lab.nedc.cold.co2 = as.numeric(data[18, 3]),
                      lab.nedc.cold.nox = as.numeric(data[19, 3]),
                      lab.nedc.cold.hc = as.numeric(data[20, 3]),
                      lab.nedc.cold.hcnox = as.numeric(data[21, 3]),
                      lab.nedc.cold.co = as.numeric(data[22, 3]),
                      lab.nedc.cold.pn = as.numeric(data[23, 3]),
                      lab.nedc.pems.co2 = as.numeric(data[18, 4]),
                      lab.nedc.pems.nox = as.numeric(data[19, 4]),
                      lab.nedc.pems.co = as.numeric(data[22, 4]),
                      lab.nedc.hot.co2 = as.numeric(data[18, 5]),
                      lab.nedc.hot.nox = as.numeric(data[19, 5]),
                      lab.nedc.hot.hc = as.numeric(data[20, 5]),
                      lab.nedc.hot.hcnox = as.numeric(data[21, 5]),
                      lab.nedc.hot.co = as.numeric(data[22, 5]),
                      lab.nedc.hot.pn = as.numeric(data[23, 5]),
                      
                      track.pres = as.numeric(data[27, 3]),
                      track.humidity = as.numeric(data[27, 6]),
                      track.temp = as.numeric(data[27, 9]),
                      track.nedc.hot.co2 = as.numeric(data[30, 3]),
                      track.nedc.hot.nox = as.numeric(data[31, 3]),
                      
                      road.atm.pres = as.numeric(data[34, 3]),
                      road.humidity = as.numeric(data[34, 6]),
                      road.temp = as.numeric(data[34, 9]),
                      road.co2 = as.numeric(data[36,3]),
                      road.nox = as.numeric(data[36,6])
                      
                      
                      )
    uk.results <- rbind(uk.results, res)
  }
}

# add the nox limit
nox.limits <- data.frame(norm = c('Euro5', 'Euro6'), nox.limit = c(180, 80)) 
uk.results <- merge(uk.results, nox.limits)

ddply(uk.results, c('norm'), summarize, 
      mean.lab.nedc.cold.nox = mean(lab.nedc.cold.nox),
      mean.track.nedc.hot.nox = mean(track.nedc.hot.nox),
      compliant.nedc = sum(lab.nedc.cold.nox < nox.limit),
      compliant.onroad = sum(track.nedc.hot.nox < nox.limit))

#  norm mean.lab.nedc.cold.nox mean.track.nedc.hot.nox compliant.nedc compliant.onroad
# Euro5              143.46158               1153.1105             17                0
# Euro6               42.17911                379.5921             18                4

# write summary table of UK tests
write.table(uk.results, 'UKsummary.txt', sep = '\t', quote = FALSE, row.names = FALSE)

# temperature trend
plot(uk.results$track.temp, uk.results$track.nedc.hot.nox)
lm1 <- lm(data = uk.results, track.nedc.hot.nox ~ track.temp + norm)
summary(lm1)


p <- ggplot(data = uk.results, aes(x=track.temp, y=track.nedc.hot.nox, group = norm)) + geom_point(aes(colour = norm))
p <- p + geom_smooth(method = "lm", se = TRUE)
p

