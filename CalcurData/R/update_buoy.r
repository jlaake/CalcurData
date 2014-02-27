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
#' update_buoy_data(esb_2013,"EastSantaBarbaraChannelBuoyData",buoy="46053",dir="")
#' update_all(month="Jan",dir="")
update_buoy_data=function(newdata,tablename,dir)
{
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
		newdata=newdata[order(as.Date(paste(newdata$YYYY,newdata$MM,newdata$DD,sep="/"))),]
	}
	if("ID"%in%names(olddata))
		newdata=cbind(ID=1:nrow(newdata),newdata)
	if(is.factor(newdata$Buoy))newdata$Buoy=as.character(newdata$Buoy)
	saveCalcurData(newdata, "Environ",tablename,dir=dir)
	invisible()
}
update_all=function(month=NULL,year=NULL,dir)
{
	if(!is.null(month))
	{
		if(!is.null(year))stop("\nCan only set month or year. Not both.")
	} else
	{
		if(is.null(year))stop("\n Must set year or month.")
	}
	if(!is.null(month))
		data=read_ndbc_month("46053",month)
	else
		data=read_ndbc("46053",year)
	update_buoy_data(data,"EastSantaBarbaraChannelBuoyData",dir=dir)
	if(!is.null(month))
		data=read_ndbc_month("46054",month)
	else
		data=read_ndbc("46054",year)
	update_buoy_data(data,"WestSantaBarbaraChannelBuoyData",dir=dir)
	if(!is.null(month))
		data=read_ndbc_month("46218",month)
	else
		data=read_ndbc("46218",year)
	update_buoy_data(data,"PtArguelloBuoyData",dir=dir)
	if(!is.null(month))
		data=read_ndbc_month("46011",month)
	else
		data=read_ndbc("46011",year)
	update_buoy_data(data,"PtSantaMariaBuoyData",dir=dir)  
	if(!is.null(month))
		data=read_ndbc_month("46028",month)
	else
		data=read_ndbc("46028",year)
	update_buoy_data(data,"CapeSanMartinBuoyData",dir=dir)
	if(!is.null(month))
		data=read_ndbc_month("pslc1",month)
	else
		data=read_ndbc("pslc1",year)
	data=construct_daily(filter_wtmp(data))
	update_buoy_data(data,"PtSanLuisDailySST",dir=dir)  
	invisible()
}   
