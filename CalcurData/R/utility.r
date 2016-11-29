#' Utility functions
#' 
#' filter_wtmp throws out records in which water temp is unknown 999;
#' construct_daily constructs daily averages; this is used with PtSan Luis because original data was only a single value per day
#' 
#' @param x dataframe of bouy data
#' @param data dataframe of buoy data
#' @param buoy character designation for the buoy
#' @usage filter_wtmp(x)
#'        construct_daily(data,buoy="PSLC1")
#' @return manipulated dataframe 
#' @export filter_wtmp construct_daily
#' @aliases filter_wtmp construct_daily
#' @author Jeff Laake
filter_wtmp=function(x) return(x[x$WTMP!=999,])

construct_daily=function(data,buoy="PSLC1")
{
	data$Date=paste(data$Month,data$Day,data$Year,sep="/")
	x=tapply(data$WTMP,data$Date,mean)
	data=data.frame(Date=names(x),SST=as.vector(x),Buoy=buoy,stringsAsFactors=FALSE)
	return(data)
}
