getData <- function(zipFile) {

# Purpose:
#   To get and clean the Household Power Consumption dataset in 'zipFile'

        # Get dataset file name

        hpcFile <- unzip(zipFile)

        # Get raw data

        colClasses <- c(rep("character", 2), rep("numeric", 7))
        hpcData <- read.table(hpcFile, header = TRUE, sep = ";", 
                colClasses = colClasses, na.strings = "?")

        # Reformat 'Date'

        hpcData$d <- as.Date(hpcData$Date, "%d/%m/%Y")

        # Select data from 01-02 Feb 2007

        hpcData <- subset(hpcData, year(hpcData$d) == 2007)
        hpcData <- subset(hpcData, month(hpcData$d) == 2)
        hpcData <- subset(hpcData, mday(hpcData$d) == 1 | mday(hpcData$d) == 2)

        # Combine 'd' and 'Time' into new 'Date'

        hpcData$Date <- strptime(paste(as.character(hpcData$d), hpcData$Time, " "), 
                format = "%Y-%m-%d %H:%M:%S")

        # Clean up

        hpcData$Time <- NULL
        hpcData$d <- NULL

        # Rename 'Date' column

        n <- length(names(hpcData))
        names(hpcData) <- c("Clock", names(hpcData)[2:n])

        return(hpcData)        
}


plotData <- function(hpcData) {

# Purpose:
#   To plot Household Power Consumption dataset to current device

        par(mfrow = c(2, 2))

        # Upper left plot

        xRng <- range(hpcData$Clock)
        yRng <- range(hpcData$Global_active_power, na.rm = TRUE)

        plot(xRng, yRng
                , xlab = ""
                , ylab = "Global Active Power (kilowatts)"
                , type = "n"
                )

        lines(hpcData$Clock, hpcData$Global_active_power
                , type = "l"
                )

        # Upper right plot

        xRng <- range(hpcData$Clock)
        yRng <- range(hpcData$Voltage, na.rm = TRUE)

        plot(xRng, yRng
                , xlab = "datetime"
                , ylab = "Voltage"
                , type = "n"
                )

        lines(hpcData$Clock, hpcData$Voltage
                , type = "l"
                )

        # Lower left plot

        xRng <- range(hpcData$Clock)
        yRng1 <- range(hpcData$Sub_metering_1, na.rm = TRUE)
        yRng2 <- range(hpcData$Sub_metering_2, na.rm = TRUE)
        yRng3 <- range(hpcData$Sub_metering_3, na.rm = TRUE)
        yRng <- range(yRng1, yRng2, yRng3)

        plot(xRng, yRng
                , xlab = ""
                , ylab = "Engergy sub metering"
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

        legend("topright", names(hpcData)[6:8]
                , col = c("black", "red", "blue")
                , bty = "n"
                , lwd = 2
                )

        # Lower right plot

        xRng <- range(hpcData$Clock)
        yRng <- range(hpcData$Global_reactive_power, na.rm = TRUE)

        plot(xRng, yRng
                , xlab = "datetime"
                , ylab = "Global_reactive_power"
                , type = "n"
                )

        lines(hpcData$Clock, hpcData$Global_reactive_power
                , type = "l"
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

plotToPNG(hpcData, "plot4.png")
