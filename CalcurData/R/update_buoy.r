#' Update buoy data
#' 
#' update_buoy_data adds data to existing table but deletes records in common in original data
#' update_all uses update_buoy_data to add a month or year to each of the existing tables
#' 
#' @param newdata newly extracted buoy data to add to database
#' @param tablename name of table in environmental database (eg "PtSanLuisDailySST")
#' @param month text string for month from current year data
#' @param year 4 digit year to extract
#' @param dir directory location for databases; if NULL uses value in databases.txt; if "" uses package directory
#' @return no return value
#' @export update_buoy_data update_all 
#' @aliases update_buoy_data update_all 
#' @author Jeff Laake
#' @examples
#' esb_2013=read_ndbc("46053",2013)
#' update_buoy_data(esb_2013,"EastSantaBarbaraChannelBuoyData",buoy="46053")
#' update_all(month="Jan",2014)
update_buoy_data=function(newdata,tablename,dir=NULL)
{
	if(is.null(newdata))
	{
		message("no data provided")
		return(NULL)
	}
	olddata=getCalcurData("Environ",tablename,dir=dir)
	if(is.null(olddata$Date))
		oldDate=as.Date(paste(olddata$YYYY,olddata$MM,olddata$DD,sep="/"))
	else 
		oldDate=as.character(olddata$Date)
	if(is.null(newdata$Date))
		newDate=as.Date(paste(newdata$Year,newdata$Month,newdata$Day,sep="/"))
	else
		newDate=as.character(newdata$Date)
	newdata=newdata[,!names(newdata)%in% c("Minute","TIDE")]
	if("ID"%in%names(olddata))
	{
		names(newdata)=names(olddata)[-1]
	}else
		names(newdata)=names(olddata)	
	olddata=olddata[!as.Date(oldDate)%in%newDate,]
	if("ID"%in%names(olddata))
	{
		newdata=rbind(olddata[,-1],newdata)
	} else
	{
		newdata=rbind(olddata,newdata)	
	}
	if("Date"%in%names(newdata))
	{
		newdata$Date=as.character(newdata$Date)
		newdata=newdata[order(as.Date(newdata$Date,format="%m/%d/%Y")),]
	}
	else
	{
		newdata=newdata[order(paste(newdata$YYYY,formatC(newdata$MM,width=2,flag=0),formatC(newdata$DD,width=2,flag=0),formatC(newdata$HH,width=2,flag=0),sep="")),]
	}
	if("ID"%in%names(olddata))
		newdata=cbind(ID=1:nrow(newdata),newdata)
	if(is.factor(newdata$Buoy))newdata$Buoy=as.character(newdata$Buoy)
	saveCalcurData(newdata, "Environ",tablename,dir=dir)
	invisible()
}
update_all=function(month=NULL,year=NULL,dir=NULL)
{
	if(!is.null(month))
	{
		if(is.null(year))stop("\nMust set year with month or just year.")
	} else
	{
		if(is.null(year))stop("\n Must set year.")
	}
	if(year<2015)
	{
		cat("\nUpdating EastSantaBarbaraChannelBuoy\n")
		if(!is.null(month))
			data=read_ndbc_month("46053",month,year)
		else
			data=read_ndbc("46053",year)
		if(!is.null(data))
			update_buoy_data(data,"EastSantaBarbaraChannelBuoyData",dir=dir)
	}
   cat("\nUpdating WestSantaBarbaraChannelBuoy\n")
   if(!is.null(month))
		data=read_ndbc_month("46054",month,year)
	else
		data=read_ndbc("46054",year)
	if(!is.null(data))
		update_buoy_data(data,"WestSantaBarbaraChannelBuoyData",dir=dir)
	cat("\nUpdating PtArguelloBuoy\n")
	if(!is.null(month))
		data=read_ndbc_month("46218",month,year)
	else
		data=read_ndbc("46218",year)
	if(!is.null(data))
		update_buoy_data(data,"PtArguelloBuoyData",dir=dir)
	cat("\nUpdating PtSantaMariaBuoy\n")
	if(!is.null(month))
		data=read_ndbc_month("46011",month,year)
	else
		data=read_ndbc("46011",year)
	if(!is.null(data))
		update_buoy_data(data,"PtSantaMariaBuoyData",dir=dir)  
	cat("\nUpdating CapeSanMartinBuoy\n")	
	if(!is.null(month))
		data=read_ndbc_month("46028",month,year)
	else
		data=read_ndbc("46028",year)
	if(!is.null(data))
		update_buoy_data(data,"CapeSanMartinBuoyData",dir=dir)
	cat("\nUpdating PtSanLuis\n")
	if(!is.null(month))
		data=read_ndbc_month("pslc1",month,year)
	else
		data=read_ndbc("pslc1",year)
	data=construct_daily(filter_wtmp(data))
	update_buoy_data(data,"PtSanLuisDailySST",dir=dir)  
	invisible()
}   
