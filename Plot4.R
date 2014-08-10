################################################################################
#################   Calculating Memory Requirements      #######################
#################                                        ######################
#################################################################################

# function to calculate the Data size

predictDataSize = function(size, type = "numeric") {
  if(type == "numeric") {
    bytePerNumber = 8
  } else {
    stop(sprintf("Unknown type: %s", type))
  }
  estimateSize = (size * bytePerNumber)
  class(estimateSize) = "object_size"
  print(estimateSize, units = "auto")
}

#The dataset has 2,075,259 rows and 9 columns,all of which are numeric data
predictDataSize( 2075259*9, "numeric")

# at least 142.5 Mb ist required to store this data!

################################################################################
#########################                        ###############################
#########################    resizing the data   ###############################
#########################                        ###############################
################################################################################
    
    # change the working directory
    setwd("E:/data/")
    
    #set the system parameter
    Sys.setlocale("LC_TIME", "English")
    
        
    # download and unzip the data
    if (!file.exists("./household_power_consumption.txt")){
      url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
      download.file(url, "./exdata-data-household_power_consumption.zip")
      unzip("./exdata-data-household_power_consumption.zip")
    }
    

    #load the data set as data table  
    require(data.table)
    DTable <- fread("./household_power_consumption.txt",na.strings="?",
                    colClasses="character")
    
    
  
    #subseting the Table
    DT<-subset(DTable, DTable$Date== "1/2/2007"|DTable$Date== "2/2/2007")
    
    # convert the data tabe into data frame
    df<-as.data.frame.matrix(DT)
    
    # convert variable Time
    df$Time <- strptime(paste(df$Date,df$Time), "%d/%m/%Y %H:%M:%S")
    
    #convert variable Date
    df$Date<- as.Date(df$Date,format='%m/%d/%Y')
    
  

##################################################################################
##############################             #######################################
##############################  plotting   #######################################
##############################             #######################################
##################################################################################

    
    
    # open png function to safe the file as  png 
        png("Plot4.png",type = c("cairo"))
    
    # file will have 4 plots: 2 columns and 2 rows
        par(mfrow = c(2, 2), mar = c(4, 4, 2, 1),lwd=0.8)
    
    ##plot:top left
        with(df,
             plot(Time,
                  as.numeric(Global_active_power),
                  type = "l",
                  xlab ="",
                  ylab="Global active power"
                  )
             ) 
    ##############################################################################
    
    
    ## Plot:top right
        with(df,
             plot(Time,
                  as.numeric(Voltage),
                  type = "l",
                  xlab ="datetime",
                  ylab="Voltage"
                  )
             )
    ###############################################################################
    
    
    
    ## plot:back left 
    #call the plot function with only the line "Sub_metering_1"
        with(df,
             plot(Time,
                  Sub_metering_1,
                  type = "l",
                  xlab = "",
                  ylab="Energy sub metering"
                  )
             )
    
    
    #add the line "Sub_metering_2"
        with(df,lines(Time,as.numeric(Sub_metering_2),col="red"))
    
    #add the line "Sub_metering_3"
        with(df,lines(Time,as.numeric(Sub_metering_3),col="blue"))
    
    #add the legend
        legend("topright",
               c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
               cex=0.8,
               lwd=c(2.5,2.5),
               lty=c(1,1),
               col=c("black","red","blue")
               )
    ###############################################################################
    
    #plot:back rigth
        with(df,
             plot(Time,
                  as.numeric(Global_reactive_power),
                  type = "l",
                  xlab = "datetime",
                  ylab="Global_reactive_power"
                  )
             )
    
    ##############################################################################        
    
    #close the connection
    dev.off()
    

