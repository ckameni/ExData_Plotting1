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


    # make a plot function
    the_plot2 <- function(){
        x<-df$Time
        y<-as.numeric(df$Global_active_power)
        plot(
            x,
            y,
            type = "l",
            xlab="",
            ylab="Global active power(kilowatts)",
        )
    }
  
  
      # open png function to safe the file as  png 
      png("Plot2.png",type = c("windows")  )
  
      #call the plot function
      the_plot2()
  
      #close the connection
      dev.off()


