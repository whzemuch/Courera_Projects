# load packages
library(data.table)
library(lubridate)

# Read in data
data_2rows <- fread(input = "household_power_consumption.txt", nrow = 1, data.table = FALSE)
data <- fread(cmd = "grep '^[12]/2/2007' household_power_consumption.txt", data.table = FALSE)
colnames(data) <- colnames(data_2rows)

data <- subset(data, Date == "1/2/2007" | Date == "2/2/2007")
data['date_time'] <- dmy_hms(paste(data$Date, data$Time))

png(filename = "plot2.png", width = 480, height = 640, units = "px")

# plot 2
plot(data$date_time, data$Global_active_power, 
     type = "l",
     xlab = "", 
     ylab = "Global active power(kilowatts)")

dev.off()
