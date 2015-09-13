library(data.table)

plot3 <- function() {

        # Extract column names and return them as a vector
        colNames <- unlist(strsplit(readLines("./household_power_consumption.txt", n = 1), ";"))
        
        # Since the records are sorted by date time, we can skip
        # all the way down the file including last minute of 31/1/2007.
        # Then read in records of the next two days. Assign column names
        # back to the extracted records.
        targetDT <- data.table(read.table("./household_power_consumption.txt",
                                          skip = grep("31/1/2007;23:59:00", 
                                                      readLines("./household_power_consumption.txt")), 
                                          nrows = 2880, col.names = colNames,
                                          sep = ";", header = F, quote = "", stringsAsFactors = F))
        
        # Ignore the incompleted records (?s and NA)
        targetDT <- subset(targetDT, !is.na(Sub_metering_3))
        
        # Convert date time fields to date object
        dateObj <- strptime(paste(targetDT$Date, targetDT$Time, sep = " "),
                            "%d/%m/%Y %T")
        
        png(filename = "./plot3.png")
        
        # Ready to plot
        with(targetDT, {
                plot(dateObj, Sub_metering_1, type = "n",
                     xlab = "", ylab = "Energy sub metering")
                points(dateObj, Sub_metering_1, pch = NA_integer_)
                lines(dateObj, Sub_metering_1, col = "black")
                points(dateObj, Sub_metering_2, pch = NA_integer_)
                lines(dateObj, Sub_metering_2, col = "red")
                points(dateObj, Sub_metering_3, pch = NA_integer_)
                lines(dateObj, Sub_metering_3, col = "blue")
                legend("topright", lty = "solid", col = c("black", "red", "blue"), 
                       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        })
        
        ## Close the PNG file device
        dev.off()
}