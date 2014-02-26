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
#' esb_jan=read_ndbc_month("46053","Jan")
read_ndbc=function(buoy,year)
{
	conn=url(paste("http://www.ndbc.noaa.gov/view_text_file.php?filename=",buoy,"h",year,".txt.gz&dir=data/historical/stdmet/",sep=""))
	df=read.delim(conn,row.names=NULL,skip=2,sep="",header=FALSE)
	names(df)=c("Year","Month","Day","Hour","Minute","WDIR","WSPD","GST","WVHT","DPD","APD","MWD","PRES","ATMP","WTMP","DEWP","VIS","TIDE")
	df$Buoy=buoy  
	return(df[df$Year==year,])
}

read_ndbc_month=function(buoy,month)
{
	conn=url(paste("http://www.ndbc.noaa.gov/data/stdmet/",month,"/",buoy,".txt",sep=""))
	df=read.delim(conn,row.names=NULL,skip=2,sep="",header=FALSE)  
	names(df)=c("Year","Month","Day","Hour","Minute","WDIR","WSPD","GST","WVHT","DPD","APD","MWD","PRES","ATMP","WTMP","DEWP","VIS","TIDE")
	df$Buoy=buoy
	return(df)
}

