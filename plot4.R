# This script is intended for use with John Hopkins University - Exploratory Data Analysis - Week 1 - Course Project 1 on Coursera.

library(dplyr) # Loads dplyr library
library(lubridate) # Loads lubridate library

# Function to generate plot 2 for the course project.
# Variable is the "exdata_data_household_power_consumption" directory containing the dataset.
plot4 <- function(dataset_dir) {
    # Check if dataset_dir ends with a "/", and adds the "/" if it does not.
    if (grepl("/$", dataset_dir) == FALSE) {
        dataset_dir <- paste(dataset_dir, "/", sep = "")
    }
    
    filename <- paste(dataset_dir, "household_power_consumption.txt", sep = "") # String used to locate data file.
    data <- data.frame(read.table(filename, header = TRUE, sep = ";")) # Creates "data" data frame which reads in the data file.
    data <- data %>% transmute(Date_and_Time = Date %>% paste(Time), 
                                Global_active_power = as.numeric(as.character(Global_active_power)),
                                Sub_metering_1 = as.numeric(as.character(Sub_metering_1)),
                                Sub_metering_2 = as.numeric(as.character(Sub_metering_2)),
                                Sub_metering_3 = as.numeric(as.character(Sub_metering_3)),
                                Voltage = as.numeric(as.character(Voltage)),
                                Global_reactive_power = as.numeric(as.character(Global_reactive_power)))
        # Add Date to Time into a new column with space separator, and converts required variables to numeric from factor.
    data <- data %>% mutate(Date_and_Time = Date_and_Time %>% dmy_hms()) # Converts Date_and_Time string to POSIXct and POSIXlt.
    data <- data %>% filter(Date_and_Time >= dmy("01/02/2007") & Date_and_Time < dmy("03/02/2007"))
        # Only leaves rows between 01/02/2007 and 02/02/2007.
    
    png("plot4.png", width = 480, height = 480) # Open png file to be saved.
    par(mfcol=c(2,2))
    plot(data$Date_and_Time, data$Global_active_power, type = "l", main = "", xlab = "", ylab = "Global Active Power (kilowatts)")
        # Plot first line graph (same as plot2)
    plot(data$Date_and_Time, data$Sub_metering_1, type = "l", col = "black", main = "", xlab = "", ylab = "Energy sub metering")
    lines(data$Date_and_Time, data$Sub_metering_2, type = "l", col = "red")
    lines(data$Date_and_Time, data$Sub_metering_3, type = "l", col = "blue")
    legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty = 1, box.lty = 0)
        # Plot second line graph (same as plot3 with border removed from legend)
    plot(data$Date_and_Time, data$Voltage, type = "l", main = "", xlab = "datetime", ylab = "Voltage")
        # Plot third line graph
    plot(data$Date_and_Time, data$Global_reactive_power, type = "l", main = "", xlab = "datetime", ylab = "Global_reactive_power")
        # Plot fourth line graph
    dev.off() # Close the png file.
    
}