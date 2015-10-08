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

plotToPNG(hpcData, "plot2.png")
