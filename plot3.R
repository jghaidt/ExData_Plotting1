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

        xRng <- range(hpcData$Clock)
        yRng1 <- range(hpcData$Sub_metering_1, na.rm = TRUE)
        yRng2 <- range(hpcData$Sub_metering_2, na.rm = TRUE)
        yRng3 <- range(hpcData$Sub_metering_3, na.rm = TRUE)
        yRng <- range(yRng1, yRng2, yRng3)

        plot(xRng, yRng
                , xlab = ""
                , ylab = "Energy sub metering"
                , type = "n"
                )

        lines(hpcData$Clock, hpcData$Sub_metering_1
                , type = "l"
                , col = "black"
                )
        lines(hpcData$Clock, hpcData$Sub_metering_2
                , type = "l"
                , col = "red"
                )
        lines(hpcData$Clock, hpcData$Sub_metering_3
                , type = "l"
                , col = "blue"
                )

        legend("topright", names(hpcData)[5:7]
                , col = c("black", "red", "blue")
                , lwd = 2
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

plotToPNG(hpcData, "plot3.png")
