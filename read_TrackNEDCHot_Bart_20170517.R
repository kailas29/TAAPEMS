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

overview.TrackHotNEDC <- data.frame(car.name = c(), header1 = as.character(), num.cols = as.integer())

# loop over all test to have an overview of the existing formats
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
      overview.TrackHotNEDC <- rbind(overview.TrackHotNEDC,
                               data.frame(car.name = car.name, header1 = NA, num.cols = NA))
    } else {
      test.data <- data.frame()
      try(test.data <- read_excel(paste0(path, file), sheet = sheet.number), silent = TRUE)
      header.names <- names(test.data)
      overview.TrackHotNEDC <- rbind(overview.TrackHotNEDC,
                               data.frame(car.name = car.name, 
                                          header1 = header.names[1], 
                                          num.cols = length(header.names)))
    }      
  }
}
overview.TrackHotNEDC.sorted <- overview.TrackHotNEDC[order(overview.TrackHotNEDC$header, overview.TrackHotNEDC$num.cols),]
write.table(overview.TrackHotNEDC, 'overview_TrackHotNEDC.txt', row.names = FALSE, quote = FALSE)

path <- paths[2]
file <- "euro-6-ford-focus-1499.xlsx"

# loop over all test and put the results in a data frame according to the format.
# The following formats exist:
# 1) format 1: header[1] = Absolute_time and 138 columns (one test has the other format pasted to its right, 174 cols)
# 2) format 2: header[1] = 'Test SequenceNumber' and 36 (3), 37 (1) or 52 (1) columns
# 3) format 3: 'TestSequenceNumber'       52

for (path in paths) {
  xlsx.files <- list.files(path)
  
  for (file in xlsx.files) {
    print(paste('processing', file))
    car.name <- sub(".xlsx", "", file)
    overview.TrackHotNEDC <- rbind(overview.TrackHotNEDC, data.frame(car.name = car.name, TrackHotNEDC = -1))
    
    # get the sheet number of the 'Track Hot NEDC' test
    sheets <- excel_sheets(paste0(path, file))
    # get the sheet number for the hot track test
    sheet.number <- match("T-Hot NEDC", sheets)
    test.data <- data.frame()
    try(test.data <- read_excel(paste0(path, file), sheet = sheet.number))
    
    if (NROW(test.data) == 0) {
      # no sheet or an empty sheet
      print(paste('Sheet', "T-Hot NEDC", "not found in", file))
      
    } else {
      # add a column with the car name and test type
      test.data <- data.frame(car.name = car.name, test.type = 'T-Hot NEDC', test.data)
      
      # data are available, but in which format???
      # by retrieving the list of header names it's possible to check the kind of data
      header.names <- names(test.data)

      if (header.names[1] == 'Absolute_time') {
        # 2 possibilities: 138 columns or 174 columns (=138 + the other format)
        # the plot is the same for both number of columns
        jpeg(paste0('plots/', 'GPS_speed_track_', car.name, '.jpg'), width = 2*480, height = 480)
        par(mfrow=c(1,2))
        plot(test.data$GPS_VehicleSpeed, main = car.name, type = 'l') # on road NEDC
        plot(test.data$GPS_Longitude, test.data$GPS_Latitude, type = 'l') # you can see the test track shape
        dev.off()
        
        num.cols <- NCOL(test.data)
        if (num.cols == 2+138) {
          Track.hotNEDC.results.1 <- rbind(Track.hotNEDC.results.1, test.data)
        } else if (num.cols == 2+174) {
          # there's only 1 case, seems they compared the to PEMS during the same trip
          # select only the first 2+138 columns
          Track.hotNEDC.results.1 <- rbind(Track.hotNEDC.results.1, test.data[,1:140])
        } else {
          print('Problem: other number of columns for format Absolute_time')
        }
        
        # # add data to dataframe (tibble)
        # test.data <- data.frame(car.name = car.name, test.type = 'T-Hot NEDC', test.data)
        # if (NCOL(test.data) == 140) {
        #   Track.hotNEDC.results.2 <- rbind(Track.hotNEDC.results.2, test.data)
        #   overview.TrackHotNEDC$TrackHotNEDC[overview.TrackHotNEDC$car.name == car.name] <- 2
        # } else if (NCOL(test.data) == 176) {
        #   Track.hotNEDC.results.3 <- rbind(Track.hotNEDC.results.3, test.data)
        #   overview.TrackHotNEDC$TrackHotNEDC[overview.TrackHotNEDC$car.name == car.name] <- 3
        # } else {
        #   overview.TrackHotNEDC$TrackHotNEDC[overview.TrackHotNEDC$car.name == car.name] <- NCOL(test.data)
        #   print(paste(car.name, 'has', NCOL(test.data), 'columns'))
        # }
        
      } else if (header.names[1] == 'Test SequenceNumber') {
        
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
        overview.TrackHotNEDC$TrackHotNEDC[overview.TrackHotNEDC$car.name == car.name] <- 1
        
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
        overview.TrackHotNEDC$TrackHotNEDC[overview.TrackHotNEDC$car.name == car.name] <- 4
        
      }
      
      else 
      else {
        print('unknown header')
      }
    } # if there are data
  } # loop over files
} # loop over folders

print(overview.TrackHotNEDC)

length(unique(Track.hotNEDC.results.1$car.name))
length(unique(Track.hotNEDC.results.2$car.name))
length(unique(Track.hotNEDC.results.3$car.name))

# TO DO
# find corresponding columns in the 2 data formats and combine them in one data.frame

# write summary table of UK tests
# write.table(Track.hotNEDC.results.1, 'UK_TrackNEDCHot.txt')


