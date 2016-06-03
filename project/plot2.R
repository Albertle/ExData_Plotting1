plot2 <- function(dataSetAdd){
        #read data from the given address
        library(plyr)
        data <- read.table(dataSetAdd, sep = ';', skip = 1, col.names = c("Date", "Time","Global_active_power", "Global_reactive_power", "Voltage","Global_intensity", "Sub_metering_1","Sub_metering_2", "Sub_metering_3"))
        whole_date <- paste(data$Date,data$Time)
        whole_date <- strptime(whole_date, "%d/%m/%Y %H:%M:%S")
        data <- mutate(data, complete_date = whole_date)
        begin <- begin <- strptime("2007-02-01 00:00:00", "%Y-%m-%d %H:%M:%S")
        end <- strptime("2007-02-02 23:59:59", "%Y-%m-%d %H:%M:%S")
        after_begin <- data$complete_date >= begin
        before_end <- data$complete_date <= end
        #get the data we want
        selected_data <- data[(after_begin & before_end),]
        #begin to plot the plot2.png
        #windows();
        png("plot2.png", width = 480, height = 480)
        required_data <- as.numeric(as.character(selected_data$Global_active_power))
        plot(selected_data$complete_date, required_data, type="l", lty=1, lwd=1, xlab="", ylab = "Global Active Power (kilowatts)")
        dev.off();
        return(selected_data)
}