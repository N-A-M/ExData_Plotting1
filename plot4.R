plot4 <- function (){
    # reading the header of the large data set and saving in a table
    library("data.table")
    header <- data.table:::fread("./household_power_consumption.txt"
                                 ,sep = ";"
                                 ,header = T
                                 ,na.strings ="?"
                                 ,colClasses = "character"
                                 ,nrows = 0
                                 ,data.table = T
                                 
    )
    # reading the dates column of the large data set for comparison
    PCData <- data.table:::fread("./household_power_consumption.txt"
                                 ,sep = ";"
                                 ,header = T
                                 ,na.strings ="?"
                                 ,colClasses = "character"
                                 ,select = "Date"
                                 ,data.table = T
                                 
    )
    #str(PCData)
    # adding a logical column to the date table that checks required dates
    PCData <- PCData[,s:= PCData$Date %in% c("01/02/2007","1/2/2007","02/02/2007","2/2/2007") ]
    
    
    #str(PCData)
    # getting the row indices of those that satisfy the condition
    idx <- which ( PCData$s )
    
    # reading from file the rows identified
    reqData <- data.table:::fread("./household_power_consumption.txt"
                                  ,sep = ";"
                                  ,header = F
                                  ,na.strings ="?"
                                  ,colClasses = c("Date","POSIXlt",rep("numeric",7))
                                  ,skip = range(idx)[1]
                                  ,nrows = range(idx)[2]- range(idx)[1]+1
                                  ,data.table = T
    )
    # setting the names for the read data
    setnames(reqData, names(reqData) ,  names(header))
    names(reqData)
    
    
    #reqData[,Time_stamp:= ]
    tt <- strptime( paste(reqData$Date,reqData$Time,sep=" ") ,  "%d/%m%y %h:%m%s") 
    t <- seq.int(from = as.Date("01/02/2007", format = "%d/%m/%Y")
                 ,to = as.Date("03/02/2007", format = "%d/%m/%Y")
                 ,along.with = tt )
    par(mfrow=c(2,2)
        ,mar = c(4,4,4,2))
    plot( t
          ,as.vector(reqData$Global_active_power,mode = "numeric")
          ,type="l"
          ,ylab = "Global Active Power"
          ,xlab = "")
    
    plot( t
          ,as.vector(reqData$Voltage ,mode = "numeric")
          ,type="l"
          ,ylab = "Voltage"
          ,xlab = "datetime")
    
    plot( t
          ,as.vector(reqData$Sub_metering_1,mode = "numeric")
          ,type="l"
          ,col = "black"        
          ,ylab = "Energy sub metering"
          ,xlab = "")
    lines( t
           ,as.vector(reqData$Sub_metering_2,mode = "numeric")
           ,col="red"
    )
    lines( t
           ,as.vector(reqData$Sub_metering_3,mode = "numeric")
           ,col="blue"
    )
    legend( "topright"
            ,c("sub_metering_1","sub_metering_2","sub_metering_3")# puts text in the legend
            ,col=c("black","red","blue")
            ,lty="solid"
            ,lwd="1"
            ,bty = "n"
            ,xjust = 1
    )
    
    plot( t
          ,as.vector(reqData$Global_reactive_power ,mode = "numeric")
          ,type="l"
          ,ylab = "Global_reactive_power"
          ,xlab = "datetime")
    
    dev.copy(png,"Plot4.png"
             ,width=480
             ,height=480 
    )
    dev.off()
    
    
}
