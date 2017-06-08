
# ------------------------------
# read Track NEDC Hot of UK data
# ------------------------------

# clean up
rm(list = ls())

library(readxl)
library(dplyr)
#library(data.table)
#library(tibble)
setwd('C:/Vasilis/UK_readings/')

paths <- c('C:/Vasilis/UK_readings/euro-5-vehicle-data/',
           'C:/Vasilis/UK_readings/euro-6-vehicle-data/')

L.NEDCH.results.1 <- data.frame()
L.NEDCH.results.2 <- data.frame()

overview.L.NEDCH <- data.frame(car.name = c(), header1 = as.character(), num.cols = as.integer())

###########FIRST PART GIVES AND OVERVIEW OF THE DATA########
########SECOND PART PROCESS THE LAB DATA FORMATS############


# loop over all test to have an overview of the existing formats
for (path in paths) {
  xlsx.files <- list.files(path)
  for (file in xlsx.files) {
    print(paste('processing', file))
    car.name <- sub(".xlsx", "", file)
    # get the sheet number of the 'Track Hot NEDC' test
    sheets <- excel_sheets(paste0(path, file))
    # get the sheet number for the hot track test
    sheet.number <- match("L-NEDC (H)", sheets)
    if (is.na(sheet.number)) {
      print(paste('Sheet', "L-NEDC (H)", "not found in", file))
      overview.L.NEDCH <- rbind(overview.L.NEDCH,
                                     data.frame(car.name = car.name, header1 = NA, num.cols = NA))
    } else {
      test.data <- data.frame()
      try(test.data <- read_excel(paste0(path, file), sheet = sheet.number), silent = TRUE)
      header.names <- names(test.data)
      overview.L.NEDCH <- rbind(overview.L.NEDCH,
                                     data.frame(car.name = car.name, 
                                                header1 = header.names[1], 
                                                num.cols = length(header.names)))
    }      
  }
}
overview.L.NEDCH.sorted <- overview.L.NEDCH[order(overview.L.NEDCH$header, overview.L.NEDCH$num.cols),]

####OPTIONAL TO WRITE THE SUMMARY######
#write.table(overview.L.NEDCH.sorted, 'overview_LNEDC_H.txt', row.names = FALSE, quote = FALSE, sep = ';')

path <- paths[2]
file <- "euro-5-hyundai-i30automatic-1582.xlsx"

#####LIST OF NAMES TO SEARCH AND AQUIRE FROM THE EXCEL SHEETS#######

########################VERY IMPORTANT##############################
#######REMEMBER TO ADD THE VARIABLE THAT YOU NEED  HERE############# 

col.names<-c("car.name","test.type","Scheduled.Speed", "Actual.Speed","Cell.Temperature","Relative.Humidity","Tailpipe.Sample.NOx","Fuel.consumption",
             "Tailpipe.Sample.NOx.mass","Phase.distance","Particle.Number", "Tailpipe.NOx", "Tailpipe.NOx.mass","SpeedFeedback","ScheduledSpeed","PhaseDistance","CellTemperature",
              "RelativeHumidity","DirectNOX","fAmbFlowRate","fAmbTemp")

for (path in paths) {
  xlsx.files <- list.files(path)
  
  #file <- 'euro-6-honda-crv-1600.xlsx'
  for (file in xlsx.files) {
    print(paste('processing', file))
    car.name <- sub(".xlsx", "", file)
    
    # get the sheet number of the 'Track Hot NEDC' test
    sheets <- excel_sheets(paste0(path, file))
    # get the sheet number for the hot track test
    sheet.number <- match("L-NEDC (H)", sheets)
    test.data <- data.frame()
    try(test.data <- read_excel(paste0(path, file), sheet = sheet.number))
    
    if (NROW(test.data) == 0) {
      # no sheet or an empty sheet
      print(paste('Sheet', "L-NEDC (H)", "not found in", file))
      
    } else {
      # add a column with the car name and test type
      test.data <- data.frame(car.name = car.name, test.type = 'L-NEDC (H)', test.data)
      
      # data are available, but in which format???
      # by retrieving the list of header names it's possible to check the kind of data
      header.names <- names(test.data)
      num.cols <- NCOL(test.data)
      if (header.names[3] == 'Log.Time') {
       
         #####MATCH THE AQUIRED FILE HEADERS WITH THE DEFINED HEADERS FROM THE LIST IN THE BEGINNING#####
        data<-select(test.data, one_of(col.names))
        L.NEDCH.results.1 <- rbind.fill(L.NEDCH.results.1, data)
        
        } else if (header.names[3] == 'Date_Time') { 
          
          
          data<-select(test.data, one_of(col.names))
          L.NEDCH.results.2 <- rbind.fill(L.NEDCH.results.2, data)
        } else {
          print('Problem: CHECK THE FORMAT AGAIN')
        }
      }
    } # if there are data
  } # loop over files


length(unique(L.NEDCH.results.1$car.name))
length(unique(L.NEDCH.results.2$car.name))


all.L.NEDCH.UK<-rbind.fill(L.NEDCH.results.1, L.NEDCH.results.2)


#remove unnessecary items
rm(test.data, L.NEDCH.results.1, L.NEDCH.results.2, overview.L.NEDCH,
   car.name, file, header.names,num.cols, path, paths, sheet.number, sheets, xlsx.files, col.names)
#write txt file
write.table(all.L.NEDCH.UK, 'UK_LNEDCH.txt')
