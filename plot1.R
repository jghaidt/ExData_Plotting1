getData <- function(zipFile) {

# Purpose:
#   To get and clean the Household Power Consumption dataset in 'zipFile'

        # Get raw data

        hpcFile <- unzip(zipFile)

        colClasses <- c(rep("character", 2), rep("numeric", 7))
        hpcData <- read.table(hpcFile, header = TRUE, sep = ";", 
                colClasses = colClasses, na.strings = "?")

        # Reformat 'Date' in 'Date1'

        hpcData$Date1 <- as.Date(hpcData$Date, "%d/%m/%Y")

        # Extract observations from 01-02 Feb 2007

        hpcData <- subset(hpcData, year(hpcData$Date1) == 2007)
        hpcData <- subset(hpcData, month(hpcData$Date1) == 2)
        hpcData <- subset(hpcData, mday(hpcData$Date1) == 1 | mday(hpcData$Date1) == 2)

        # Combine 'Date1' and 'Time' into new 'Clock'

        hpcData$Clock <- strptime(paste(as.character(hpcData$Date1), hpcData$Time, " "), 
                format = "%Y-%m-%d %H:%M:%S")

        # Clean up

        hpcData$Date <- NULL
        hpcData$Time <- NULL
        hpcData$Date1 <- NULL

        return(hpcData)        
}


plotData <- function(hpcData) {

# Purpose:
#   To plot Household Power Consumption dataset to current device

        hist(hpcData$Global_active_power, 
                col = "red", 
                xlab = "Global Active Power (kilowatts)", 
                main = "Global Active Power"
                )

        return(0)
}


plotToScreen <- function(hpcData) {

# Purpose:
#   To plot to the screen

        plotData(hpcData)

        return(0)
}


plotToPNG <- function(hpcData, pngFile) {

# Purpose:
#   To plot to a file in PNG format

        png(filename = pngFile
                , width = 480, height = 480, units = "px"
                , pointsize = 12
#                , bg = "white", res = NA, family = "", restoreConsole = TRUE
#                , type = c("windows", "cairo", "cairo-png"), antialias
                )
        plotData(hpcData)
        dev.off()
        
        return(0)
}


# Main script to generate and save plot

library(lubridate)

hpcData <- getData("exdata_data_household_power_consumption.zip")

plotToScreen(hpcData)

plotToPNG(hpcData, "plot1.png")
