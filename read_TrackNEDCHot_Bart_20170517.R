# ------------------------------
# read Track NEDC Hot of UK data
# ------------------------------

# clean up
rm(list = ls())

library(readxl)
setwd('D:/cyclebeating/Analysis TAA data/UK PEMS/')

paths <- c('D:/cyclebeating/Analysis TAA data/UK PEMS/euro-5-vehicle-data/',
           'D:/cyclebeating/Analysis TAA data/UK PEMS/euro-6-vehicle-data/')

Track.hotNEDC.results.1 <- data.frame()
Track.hotNEDC.results.2 <- data.frame()
Track.hotNEDC.results.3 <- data.frame()


path <- paths[2]
file <- "euro-6-audi-a3-1598.xlsx"

# selected columns from T-Hot NEDC data
# test.cols <- c('Relative_time',	'GPS_Altitude',	'GPS_Latitude',	'GPS_Longitude',	'GPS_VehicleSpeed',
#                'GPS_Course', 'PF_ExhaustFlowRate1',	'PF_ExhaustTemp1',	'PF_ExhaustPressure1',
#                'GA_COConc',	'GA_CO2Conc',	'GA_H2OConc',	'GA_NOConc',	'GA_NOxConc',	'GA_NO2Conc')
test.id = 1

for (path in paths) {
  xlsx.files <- list.files(path)
  
  for (file in xlsx.files) {
    print(paste('processing', file))
    # get the sheet number of the 'Track Hot NEDC' test
    sheets <- excel_sheets(paste0(path, file))
    # get the sheet number for the hot track test
    sheet.number <- match("T-Hot NEDC", sheets)
    
    if (is.na(sheet.number)) {
      print(paste('Sheet', "T-Hot NEDC", "not found in", file))
    } else {
      test.data <- read_excel(paste0(path, file), sheet = sheet.number)
      # by retrieving the list of header names it's possible to check the kind of data
      header.names <- names(test.data)
      car.name <- sub(".xlsx", "", file)
      
      # There are 2 types of files:
      # 1) starting with 'TestSequenceNumber'
      if (header.names[1] == 'TestSequenceNumber') {
        
        # some plots to check the data
        jpeg(paste0('plots/', 'GPS_speed_track_', car.name, '.jpg'), width = 2*480, height = 480)
        par(mfrow=c(1,2))
        plot(test.data$GPSSpeed, main = car.name, type = 'l') # on road NEDC
        plot(test.data$GPSLongitude, test.data$GPSLatitude, type = 'l') # you can see the test track shape
        dev.off()
        
        # add data to dataframe (tibble)
        # add a column with the car name and test type
        test.data <- data.frame(car.name = car.name, test.type = 'T-Hot NEDC', test.data)
        Track.hotNEDC.results.1 <- rbind(Track.hotNEDC.results.1, test.data)
      } # stop reading format 1
      
      # 2)
      if (header.names[1] == 'Absolute_time') {
        jpeg(paste0('plots/', 'GPS_speed_track_', car.name, '.jpg'), width = 2*480, height = 480)
        par(mfrow=c(1,2))
        plot(test.data$GPS_VehicleSpeed, main = car.name, type = 'l') # on road NEDC
        plot(test.data$GPS_Longitude, test.data$GPS_Latitude, type = 'l') # you can see the test track shape
        dev.off()

        # add data to dataframe (tibble)
        test.data <- data.frame(car.name = car.name, test.type = 'T-Hot NEDC', test.data)
        if (NCOL(test.data) == 140) {
          Track.hotNEDC.results.2 <- rbind(Track.hotNEDC.results.2, test.data)
        } 
        if (NCOL(test.data) == 176) {
          Track.hotNEDC.results.3 <- rbind(Track.hotNEDC.results.3, test.data)
        } 
        
      } # stop reading format 2
      
    }
  } # loop over files
} # loop over folders

# TO DO
# find corresponding columns in the 2 data formats and combine them in one data.frame

# write summary table of UK tests
# write.table(Track.hotNEDC.results.1, 'UK_TrackNEDCHot.txt')


