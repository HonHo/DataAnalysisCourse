library(data.table)

plot1 <- function() {

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
        
        png(filename = "./plot1.png")
        
        # Ready to plot
        hist(as.numeric(targetDT$Global_active_power),
             col = "red",
             main = "Global Active Power",
             xlab = "Global Active Power (kilowatts)")
        
        # Close the PNG file device
        dev.off()
}