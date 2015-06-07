# Rough calculation of the object size you want to load
# the size is in MB

calculateObjectSize <- function( rows = 0 , columns = 0) rows * columns * 8 / 2^20
estimatedObjectSize <- calculateObjectSize(2075259, 9)

# Set file variables
dataURL        <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
zippedDataFile <- "data_household_power_consumption.zip"
dataFile       <- "household_power_consumption.txt"

# set date class to define the 'date' column
setClass('myDate')
setAs(
    "character",
    "myDate", 
    function(from) as.Date(from, format="%d/%m/%Y") 
)

# Download and load the data from the file
download.file(
    url      = dataURL, 
    destfile = zippedDataFile,
    method   = "libcurl")

data <- read.table(
    file       = unz(zippedDataFile, dataFile), 
    header     = TRUE, 
    sep        = ";", 
    na.strings = "?",
    colClasses = c("myDate","character", rep("numeric", 7))
)

names(data) <- tolower(names(data))

# Filter the data: only the records for the dates 2007-02-01 and 2007-02-02
# are needed for this excercise
data <- data[
    data$date >= as.Date("2007-02-01")  & 
    data$date <= as.Date("2007-02-02"),]

data$date_time <- as.POSIXct(paste(data$date, data$time), format="%Y-%m-%d %H:%M:%S")

# open graphics file device - png
png(filename = "plot1.png", width = 480, height = 480, units = "px")

# Draw the first plot
hist(x=data$global_active_power,   
     main="Global Active Power", 
     xlab="Global Active Power (kilowatts)",
     col="red")

# close graphics file device
dev.off()