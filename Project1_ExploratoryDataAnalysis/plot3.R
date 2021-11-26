
# load packages
library(data.table)
library(lubridate)

# Read in data
data_2rows <- fread(input = "household_power_consumption.txt", nrow = 1, data.table = FALSE)
data <- fread(cmd = "grep '^[12]/2/2007' household_power_consumption.txt", data.table = FALSE)
colnames(data) <- colnames(data_2rows)

data <- subset(data, Date == "1/2/2007" | Date == "2/2/2007")
data['date_time'] <- dmy_hms(paste(data$Date, data$Time))


# plot 3
png(filename = "plot3.png", width = 480, height = 480, units = "px")

plot(data$date_time, data$Sub_metering_1,type= "l", xlab = "", ylab = "Energy sub metering")
lines(data$date_time, data$Sub_metering_2, type= 'l', col = 'red')
lines(data$date_time, data$Sub_metering_3, type= 'l', col = 'blue')
legend("topright", 
       legend = paste0("Sub_metering_", 1:3),
       col = c("black", "red", "blue"),
       lty = 1, lwd = 1)

dev.off()
