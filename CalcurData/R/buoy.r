#' Retrieve buoy data 
#' 
#' @param bouy NDBC buoy designation (ie ESB 46053)
#' @param year 4 digit year to extract
#' @param month text string for month from current year data
#' @param dir directory location for databases; if NULL uses value in databases.txt; if "" uses package directory
#' @return extracted dataframe 
#' @export read_ndbc read_ndbc_month 
#' @aliases read_ndbc read_ndbc_month 
#' @author Jeff Laake
#' @examples
#' esb_2013=read_ndbc("46053",2013)
#' esb_jan=read_ndbc_month("46053","Jan",2014)
read_ndbc=function(buoy,year)
{
	conn=url(paste("http://www.ndbc.noaa.gov/view_text_file.php?filename=",buoy,"h",year,".txt.gz&dir=data/historical/stdmet/",sep=""))
	try(df=read.delim(conn,row.names=NULL,skip=2,sep="",header=FALSE))
	if(class(df)=="try-error")
	{
		message(paste("Year = ", year, " not available for buoy ",buoy,sep=""))
		close(conn)
		return(NULL)
	}
	names(df)=c("Year","Month","Day","Hour","Minute","WDIR","WSPD","GST","WVHT","DPD","APD","MWD","PRES","ATMP","WTMP","DEWP","VIS","TIDE")
	df$Buoy=buoy  
	return(df[df$Year==year,])
}
read_ndbc_month=function(buoy,month,year)
{
	months=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	conn=url(paste("http://www.ndbc.noaa.gov/view_text_file.php?filename=",buoy,match(month,months),year,".txt.gz&dir=data/stdmet/",month,"/",sep=""))
	df=try(read.delim(conn, row.names = NULL, skip = 2, sep = "", header = FALSE),silent=TRUE)
	if(class(df)=="try-error")
	{
		close(conn)
		conn=url(paste("http://www.ndbc.noaa.gov/data/stdmet/",month,"/",buoy,".txt",sep=""))
		df=try(read.delim(conn, row.names = NULL, skip = 2, sep = "", header = FALSE),silent=TRUE)
		if(class(df)=="try-error")
		{
			message(paste("Month = ",month," Year = ", year, " not available for buoy ",buoy,sep=""))
			close(conn)
			return(NULL)
		}else
		{
			names(df)=c("Year","Month","Day","Hour","Minute","WDIR","WSPD","GST","WVHT","DPD","APD","MWD","PRES","ATMP","WTMP","DEWP","VIS","TIDE")
			df$Buoy=buoy
			return(df)
		}
	}
	names(df)=c("Year","Month","Day","Hour","Minute","WDIR","WSPD","GST","WVHT","DPD","APD","MWD","PRES","ATMP","WTMP","DEWP","VIS","TIDE")
	df$Buoy=buoy
	return(df)
}
