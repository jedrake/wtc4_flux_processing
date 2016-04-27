###########################################################################################################################################
#- Upload monthly chunks of the trendlog data to HIEv
###########################################################################################################################################


#------------------------------------------------------------------------------------------------------------------------------------------
# load packages and functions
require(data.table)
require(dplyr)
require(HIEv)
library(lubridate)
library(RODBC)

setToken(tokenfile="HIEv_token.txt")
#------------------------------------------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------------------------------------------------------
# Read in the trendlog dataframes, update them
source("R/updatetrendlogs.R")

load("data/TrendlogChDF.RData")
load("data/TrendlogRefDF.RData")


#------------------------------------------------------------------------------------------------------------------------------------------







#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------
#- process and upload monthly csv file for the chamber dataset
#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------------------------
# export monthly csv files of the chamber data

#create a vector with the year and month, use to split dataframe into list of dataframes
Monthyear <- format(TrendlogChDF$DateTime, "%Y %m")
Ch.l <- split(TrendlogChDF, Monthyear)

# loop over the list and export dataframes
outChfns <- c() #preallocate vector to "catch" the output
for(i in 1:length(Ch.l)){
  Date <- as.Date(Ch.l[[i]]$DateTime)
  d <- format(range(Date), "%Y%m%d") # get the range of dates to plug into the file name
  
  outChfns[i] <- paste0("output/forHIEv/WTC_TEMP-PARRA_CM_WTCMET-MIN_",d[1],"-",d[2],"_L0_v1.csv")
  write.csv(Ch.l[[i]], outChfns[i], row.names=FALSE)
}
#------------------------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------------------------
# Upload the monthly files

uploadit <- FALSE
if(uploadit){
  
  #list the file names that are already on HIEv
  filesonHIEv <- searchHIEv(filename="WTC_TEMP-PARRA_CM_WTCMET-MIN")$filename
  
  #list the file names that exist on my computer and could be uploaded
  localfiles <- list.files(path="output/forHIEv/",pattern="WTC_TEMP-PARRA_CM_WTCMET-MIN")
  
  #find the local files that are NOT on HIEv
  newfiles <- localfiles[!localfiles %in% filesonHIEv]
  
  toupload <- newfiles[1:2] # manually edit these to upload?
  
  #read in the files to upload
  outChfns <- c() #preallocate vector to "catch" the read in files
  for (i in 1:length(toupload)){
    outChfns[[i]] <- read.csv(paste(file="output/forHIEv/",toupload[i],sep=""))
  }
  
  #read in the description file
  m <- readLines("output/forHIEv/WTC_TEMP-PARRA_CM_WTCMET-MIN_data_description.txt",skipNul=T)
  
  #loop over the files and upload them. Note that the correct experiment is "84"
  for(i in 1:length(toupload)){
    HIEv:::uploadToHIEv(paste("Output/forHIEv/",toupload[[i]],sep=""), experiment=84, description = m)
  }
  
  
  
}
#------------------------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------
#- finished with chambers
#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------











#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------
#- process and upload monthly csv file for the reference dataset
#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------------------------
# export monthly csv files of the reference data

#create a vector with the year and month, use to split dataframe into list of dataframes
Monthyear.r <- format(TrendlogRefDF$DateTime, "%Y %m")
Ref.l <- split(TrendlogRefDF, Monthyear.r)

# loop over the list and export dataframes
outReffns <- c() #preallocate vector to "catch" the output
for(i in 1:length(Ref.l)){
  Date <- as.Date(Ref.l[[i]]$DateTime)
  d <- format(range(Date), "%Y%m%d") # get the range of dates to plug into the file name
  
  outReffns[i] <- paste0("output/forHIEv/WTC_TEMP-PARRA_CM_REFMET-MIN_",d[1],"-",d[2],"_L0_v1.csv")
  write.csv(Ref.l[[i]], outReffns[i], row.names=FALSE)
}
#------------------------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------------------------
# Upload the monthly files

uploadit <- FALSE
if(uploadit){
  
  #list the file names that are already on HIEv
  filesonHIEv <- searchHIEv(filename="WTC_TEMP-PARRA_CM_REFMET-MIN")$filename
  
  #list the file names that exist on my computer and could be uploaded
  localfiles <- list.files(path="output/forHIEv/",pattern="WTC_TEMP-PARRA_CM_REFMET-MIN")
  
  #find the local files that are NOT on HIEv
  newfiles <- localfiles[!localfiles %in% filesonHIEv]
  
  toupload <- newfiles[1:2] # manually edit these to upload?
  
  #read in the files to upload
  outReffns <- c() #preallocate vector to "catch" the read in files
  for (i in 1:length(toupload)){
    outReffns[[i]] <- read.csv(paste(file="output/forHIEv/",toupload[i],sep=""))
  }
  
  #read in the description file
  m <- readLines("output/forHIEv/WTC_TEMP-PARRA_CM_REFMET-MIN_data_description.txt",skipNul=T)
  
  #loop over the files and upload them. 
  for(i in 1:length(toupload)){
    HIEv:::uploadToHIEv(paste("Output/forHIEv/",toupload[[i]],sep=""), experiment=84, description = m)
  }
  
  
  
}
#------------------------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------
#- finished with reference
#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------
