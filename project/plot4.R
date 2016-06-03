plot4 <- function(dataSetAdd){
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
        #begin to plot the plot4.png
        #windows();
        png("plot4.png", width = 480, height = 480)
        original <- par()
        par(lty = 1, lwd = 1, mfrow = c(2,2))
        required_data_global_active_power <- as.numeric(as.character(selected_data$Global_active_power))
        required_data_voltage <- as.numeric(as.character(selected_data$Voltage))
        required_data_global_reactive_power <- as.numeric(as.character(selected_data$Global_reactive_power))
        required_data_1 <- as.numeric(as.character(selected_data$Sub_metering_1))
        required_data_2 <- as.numeric(as.character(selected_data$Sub_metering_2))
        required_data_3 <- as.numeric(as.character(selected_data$Sub_metering_3))
        #plot the (1,1) image
        plot(selected_data$complete_date, required_data_global_active_power, type = "l",main = "", xlab = "", ylab = "Global Active Power")
        #plot the (1,2) image
        plot(selected_data$complete_date, required_data_voltage, type = "l", main = "", xlab = "datetime", ylab = "Voltage")
        #plot the (2,1) image
        plot(selected_data$complete_date, required_data_1, type = "l",xlab="", ylab = "Energy sub metering", col = "black")
        lines(selected_data$complete_date, required_data_2, col = "red")
        lines(selected_data$complete_date, required_data_3, col = "blue")
        legend(x = "topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black","red","blue"), lty = 1)
        #plot the (2,2) image
        plot(selected_data$complete_date, required_data_global_reactive_power, type = "l", main = "", xlab = "datetime", ylab = "Global_reactive_power")

        dev.off()
        par(original)
        return(selected_data)
}