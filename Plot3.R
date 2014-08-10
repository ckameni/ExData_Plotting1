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
    
    
    ####################### make the data tiddy
    
    #subseting the Table
    DT<-subset(DTable, DTable$Date== "1/2/2007"|DTable$Date== "2/2/2007")
    
    # first convert data tabe into data frame
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
    
    
    
    # make a plot function
        the_plot3 <- function(){
          x<-df$Time
          y<-as.numeric(df$Sub_metering_1)
          plot(
                x,
                y,
                type = "l",
                xlab="",
                ylab="Energy sub metering",
                )
        }
    
    # open png function to safe the file as  png 
        png("Plot3.png",type = c("windows")  )
    
    #call the plot function with only the line "Sub_metering_1"
        the_plot3()
    
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
    
    #close the connection
        dev.off()
    

