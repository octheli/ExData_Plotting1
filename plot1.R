plot1 <- function() {
## Draw plot1.png for the Project 1 of the course Exploratory Data Analysis.
## A file containing household power consumption data recorded with time is
## manually downloaded, unzipped, and saved in "household_power_consumption.txt"
## in the current working directory.
  
## read 3000 rows of data that contains selected dates.
  powerConsumption <-
      read.csv2("household_power_consumption.txt",
          header = FALSE, colClasses = "character", nrows = 3000, skip = 66530)
  
## read the first line of the data that includes variable names.
  header <- read.csv2("household_power_consumption.txt", nrows = 1)

## Name the variables in "powerConsumption".
  names(powerConsumption) <- names(header)

## names(powerConsumption)
## [1] "Date"                  "Time"                  "Global_active_power"  
## [4] "Global_reactive_power" "Voltage"               "Global_intensity"
## [7] "Sub_metering_1"        "Sub_metering_2"        "Sub_metering_3" 

## Select the data recorded on Feb. 1-2, 2007.
  powerConsumption <- subset(powerConsumption, Date %in% c("1/2/2007", "2/2/2007"))

## Create a character vector "datetime".
  datetime <- with(powerConsumption, paste(Date, Time))
  
## Convert "datetime" to the "POSIXlt" class.
  datetime <- strptime(datetime, "%d/%m/%Y %H:%M:%S")
  
## Remove the "Date" and "Time" variables in "powerConsumption".
  powerConsumption <- subset(powerConsumption, select = -(Date:Time))

## Call the function asNumeric (defined at the end of this file)
## to convert all variables in "powerConsumption" to be numeric.
  powerConsumption <- asNumeric(powerConsumption)
  
## Add the variable "datetime" into "powerConsumption".
  powerConsumption <- cbind(datetime, powerConsumption)
  
## Set up the output "plot1.png" file with a width of 480 pixels and a height of 480 pixels.
  png(filename = "plot1.png", width = 480, height = 480, units = "px")

## Construct plot1.
  hist(powerConsumption$Global_active_power, col = "red",
       xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
  
## Turn off the file graphic device.
  dev.off()
}

asNumeric <- function(x) {
## Convert all variables in x to be numeric.
  
  for (i in 1:dim(x)[2]) x[, i] <- as.numeric(x[, i])
  return(x)
}