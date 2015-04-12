plot2 <- function (){
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
    #names(reqData)
    
    tt <- strptime( paste(reqData$Date,reqData$Time,sep=" ") ,  "%d/%m%y %h:%m%s") 
    t <- seq.int(from = as.Date("01/02/2007", format = "%d/%m/%Y")
                 ,to = as.Date("03/02/2007", format = "%d/%m/%Y")
                 ,along.with = tt )
    plot( t
          ,as.vector(reqData$Global_active_power,mode = "numeric")
          ,type="l"
          ,ylab = "Global Active Power(kilowatts)"
          ,xlab = "")
    
    dev.copy(png,"Plot2.png"
             ,width=480
             ,height=480 
    )
    dev.off()
}