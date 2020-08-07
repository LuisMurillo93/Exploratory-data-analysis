file <- "C:/Users/user/Documents/GitHub/Exploratory-data-analysis/Exploratory Data analysis/household_power.zip"
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

download.file(url,file)
if (!file.exists("Homehold power data")) { 
  unzip("household_power.zip") 
}

table <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?"
                , colClasses = c('character','character','numeric','numeric'
                                 ,'numeric','numeric','numeric','numeric','numeric'))

table$Date <- as.Date(table$Date, "%d/%m/%Y")
table2 <- subset(table,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
table2 <- table2[complete.cases(table2),]
dateTime <- paste(table2$Date, table2$Time)
dateTime <- setNames(dateTime, "DateTime")
table2 <- table2[ ,!(names(table) %in% c("Date","Time"))]
table2 <- cbind(dateTime, table2)
table2$dateTime <- as.POSIXct(dateTime)
str(table2)

hist(table2$Global_active_power, main="Global Active Power"
     , xlab = "Global Active Power (kilowatts)", col="red")

dev.copy(png,"plot1.png", width=480, height=480)
dev.off()

plot(table2$Global_active_power~table2$dateTime, type="l",
     ylab="Global Active Power (kilowatts)", xlab="")
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()

with(table2, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()

par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(table2, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})


dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()












