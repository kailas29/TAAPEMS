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
Track.hotNEDC.results.4 <- data.frame()

overview.ukdata <- data.frame(car.name = c(), header1 = as.character(), num.cols = as.integer())

for (path in paths) {
  xlsx.files <- list.files(path)
  for (file in xlsx.files) {
    print(paste('processing', file))
    car.name <- sub(".xlsx", "", file)
    # get the sheet number of the 'Track Hot NEDC' test
    sheets <- excel_sheets(paste0(path, file))
    # get the sheet number for the hot track test
    sheet.number <- match("T-Hot NEDC", sheets)
    if (is.na(sheet.number)) {
      print(paste('Sheet', "T-Hot NEDC", "not found in", file))
      overview.ukdata <- rbind(overview.ukdata,
                               data.frame(car.name = car.name, header1 = NA, num.cols = NA))
    } else {
      test.data <- read_excel(paste0(path, file), sheet = sheet.number)
      header.names <- names(test.data)
      overview.ukdata <- rbind(overview.ukdata,
                               data.frame(car.name = car.name, 
                                          header1 = header.names[1], 
                                          num.cols = length(header.names)))
    }      
  }
}


path <- paths[1]
file <- "euro-5-ford-mondeo-2000.xlsx"

for (path in paths) {
  xlsx.files <- list.files(path)
  
  for (file in xlsx.files) {
    print(paste('processing', file))
    car.name <- sub(".xlsx", "", file)
    overview.ukdata <- rbind(overview.ukdata, data.frame(car.name = car.name, TrackHotNEDC = -1))
    
    # get the sheet number of the 'Track Hot NEDC' test
    sheets <- excel_sheets(paste0(path, file))
    # get the sheet number for the hot track test
    sheet.number <- match("T-Hot NEDC", sheets)
    
    if (is.na(sheet.number)) {
      
      print(paste('Sheet', "T-Hot NEDC", "not found in", file))
      overview.ukdata$TrackHotNEDC[overview.ukdata$car.name == car.name] <- NA
      
    } else {
      
      test.data <- read_excel(paste0(path, file), sheet = sheet.number)
      # by retrieving the list of header names it's possible to check the kind of data
      header.names <- names(test.data)

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
        overview.ukdata$TrackHotNEDC[overview.ukdata$car.name == car.name] <- 1
        
      } # stop reading format 1
      
      # 2) almost the same as format 1, same number of columns (52) but other names (with spaces) and some different fields
      else if (header.names[1] == 'Test SequenceNumber') {
        # some plots to check the data
        jpeg(paste0('plots/', 'GPS_speed_track_', car.name, '.jpg'), width = 2*480, height = 480)
        par(mfrow=c(1,2))
        plot(test.data$`GPS Speed`, main = car.name, type = 'l') # on road NEDC
        plot(test.data$`GPS Longitude`, test.data$`GPS Latitude`, type = 'l') # you can see the test track shape
        dev.off()
        
        # add data to dataframe (tibble)
        # add a column with the car name and test type
        test.data <- data.frame(car.name = car.name, test.type = 'T-Hot NEDC', test.data)
        Track.hotNEDC.results.4 <- rbind(Track.hotNEDC.results.4, test.data)
        overview.ukdata$TrackHotNEDC[overview.ukdata$car.name == car.name] <- 4
        
      }
      
      else if (header.names[1] == 'Absolute_time') {
        jpeg(paste0('plots/', 'GPS_speed_track_', car.name, '.jpg'), width = 2*480, height = 480)
        par(mfrow=c(1,2))
        plot(test.data$GPS_VehicleSpeed, main = car.name, type = 'l') # on road NEDC
        plot(test.data$GPS_Longitude, test.data$GPS_Latitude, type = 'l') # you can see the test track shape
        dev.off()

        # add data to dataframe (tibble)
        test.data <- data.frame(car.name = car.name, test.type = 'T-Hot NEDC', test.data)
        if (NCOL(test.data) == 140) {
          Track.hotNEDC.results.2 <- rbind(Track.hotNEDC.results.2, test.data)
          overview.ukdata$TrackHotNEDC[overview.ukdata$car.name == car.name] <- 2
        } else if (NCOL(test.data) == 176) {
          Track.hotNEDC.results.3 <- rbind(Track.hotNEDC.results.3, test.data)
          overview.ukdata$TrackHotNEDC[overview.ukdata$car.name == car.name] <- 3
        } else {
          overview.ukdata$TrackHotNEDC[overview.ukdata$car.name == car.name] <- NCOL(test.data)
          print(paste(car.name, 'has', NCOL(test.data), 'columns'))
        }
        
      } # stop reading format 2
      else {
        print('unknown header')
      }
    }
  } # loop over files
} # loop over folders

print(overview.ukdata)

length(unique(Track.hotNEDC.results.1$car.name))
length(unique(Track.hotNEDC.results.2$car.name))
length(unique(Track.hotNEDC.results.3$car.name))

# TO DO
# find corresponding columns in the 2 data formats and combine them in one data.frame

# write summary table of UK tests
# write.table(Track.hotNEDC.results.1, 'UK_TrackNEDCHot.txt')


