#' Download and convert WTC trendlog data
#'@description This function downloads a so-called set of 'trendlog' files from the whole-tree chamber (WTC) experiment. These files contain sub-minutely temperature, relative humidity, and set-point data. The \code{getWTCtrendlog} function does all the hard work of converting and merging the files, and returns aggregated data (normally in 15-minutely timestep). 
#'Please note that this only works on 32bit machines (and so far has only been tested on Windows). If you have a 64bit machine, in Rstudio go to Tools/Options... and change your 'R version' to the default 32bit. 
#'@details This function uses the \code{\link{loglist}} dataset, which is contained in this package.
#'@param Date A Date to be downloaded (only one date can be specified) (YYYY-MM-DD or 'Date' class). Defaults to yesterday.
#'@param timestep Aggregates the result into this timestep. 
#'@return A dataframe. 'DateTime' is POSIXct, chamber and T_treatment are also added.
getWTCtrendlog <- function(Date=Sys.Date()-1, timestep=15){
  
  
  # ODBC stuff only works on 32 bit
  if(.Platform$r_arch == "x64")
    stop("This function only works on 32bit Windows.")
  
  # Download and unzip a zipped trendlog file
  wtctrend <- searchHIEv("WTC_Trendlogs_", quiet=TRUE)
  
  # List all dates available.
  date <- as.Date(ymd(str_extract(wtctrend$filename, "[0-9]{8}")))
  
  #
  Date <- as.Date(Date)
  if(is.na(Date))stop("Date malformed, try again (YYYY-MM-DD)")
  if(!Date %in% date)stop("Date not found in WTC trendlogs.")
  i <- match(Date,date)
  
  # Download record
  message("Downloading data...", appendLF=FALSE)
  d <- downloadHIEv(wtctrend, i, quiet=TRUE)
  wtcdir <- "wtctrendlogs"
  unlink(wtcdir)
  message("done.")
  
  # Unzip
  dir.create(wtcdir, showWarnings = FALSE)
  u <- unzip(d, exdir=wtcdir)
  
  # A file with the trendlog key. 
  # This is saved as loglist.RData. Redo this if needed.
#     loglist <- read.csv("C:/Repos/WTCautoscript/trendloglist.csv", stringsAsFactors=FALSE)
#     loglist <- subset(loglist, trendlog != "xxx")
#     loglist$chamber <- as.factor(loglist$chamber)
#     lv <- levels(loglist$chamber)
#     levels(loglist$chamber) <- gsub("ch","C",lv)
#     loglist$trendlog <- as.numeric(loglist$trendlog)
#     save(loglist, file="data/loglist.RData")
  
  # The list of trendlogs we are interested in
  trendlog_nrs_list <- suppressWarnings(as.numeric(as.character(loglist$trendlog)))
  
  
  # Function to extract a table from an mdb, add chamber and variable fields.
  getData <- function(mdbfile){
    
    on.exit(if(exists("odb"))odbcClose(odb))
    
    odb <- try(odbcConnectAccess(mdbfile))
    if(inherits(odb, "try-error"))return(NULL)
    
    # Find variable name from loglist
    lognr <- as.numeric(str_extract(mdbfile, "[0-9]{10}"))
    varname <- loglist[trendlog_nrs_list ==lognr,"varname"]
    if(length(varname) == 0)return(NULL)
    if(length(varname) > 1){
      stop("Fatal error: some duplicate trendlog IDs in loglist.")
    }
    cham <- loglist[trendlog_nrs_list ==lognr,"chamber"]
    
    # Fetch table from mdb
    tabs <- sqlTables(odb)$TABLE_NAME
    if(!any(grepl("tblTrendlog_",tabs)))return(NULL)
    data <- sqlFetch(odb,  tabs[grep("tblTrendlog_",tabs)])
    
    # Reread Date - to ensure it is in UTC timezone.
    data$TimeOfSample <- ymd_hms(as.character(data$TimeOfSample))
    if(all(is.na(data$TimeOfSample)))return(NULL)
    
    # Nearest timestep (forward searching)
    data$DateTime <- nearestTimeStep(data$TimeOfSample, nminutes=timestep)
    
    # Keep only 15minutely averaged data.
    data <- aggregate(SampleValue ~ DateTime, FUN=mean,data=data)
    data$variable <- varname
    data$chamber <- cham
    
    return(data)
  }
  
  # Read all mdbs. Discard NULL ones. Row-bind them all.
  message("Converting data...", appendLF=FALSE)
  dats <- lapply(u,getData)
  dats <- dats[! sapply(dats, is.null)]
  dats <- suppressWarnings(as.data.frame(rbind_all(dats)))
  if(nrow(dats) == 0){
    warning("No data for ", Date)
    return(NULL)
  }
  message("done.")
  
  # clean up
  unlink(d)
  unlink(u)
  unlink(wtcdir)
  

  message("Reshaping and aggregating...", appendLF=FALSE)
  # Reshape. 
  logdatach <- reshape(dats[which(dats$chamber!='ref'),], direction="wide",
                     idvar=c("DateTime","chamber"),
                     timevar="variable")
  
  
  logdataref <- reshape(dats[which(dats$chamber=='ref'),], direction="wide",
                     idvar=c("DateTime","chamber"),
                     timevar="variable")
  
  
  # I don't know how to avoid the SampleValue.xxx names in the df; clean up.
  nm <- names(logdatach)
  names(logdatach) <- gsub("SampleValue.","",nm)
  nm <- names(logdataref)
  names(logdataref) <- gsub("SampleValue.","",nm)
# tidy dataframe
#  logdata$chamber <- as.factor(logdata$chamber)
#  treatdf <- data.frame(chamber=c("ref", paste0("C", sprintf("%02.0f", 1:12))),
#                        T_treatment=c("reference", rep(c("ambient","elevated"),6)))
  
#  logdata <- merge(logdata, treatdf, by="chamber")
  logdatach <- logdatach[order(logdatach$DateTime),]
  logdataref <- logdataref[order(logdataref$DateTime),]
  logdataref$chamber<-NULL #drop redudant chamber column from ref data
  message("done.")
  
  return(list(logdatach,logdataref))
}

