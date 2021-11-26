# Load packages
library(data.table)

library(lubridate)

# Read in data
data_2rows <- fread(input = "household_power_consumption.txt", nrow = 1, data.table = FALSE)
data <- fread(cmd = "grep '^[12]/2/2007' household_power_consumption.txt", data.table = FALSE)
colnames(data) <- colnames(data_2rows)

data <- subset(data, Date == "1/2/2007" | Date == "2/2/2007")


data['date_time'] <- dmy_hms(paste(data$Date, data$Time))

# plot 1

png(filename = "plot1.png", width = 480, height = 640, units = "px")

hist(x = as.numeric(data$Global_active_power),
     #breaks = c(0, 2, 4, 6),
     main = "",
     col = "red",
     xlab = "Global active power(kilowatts)"
     )

dev.off()



