#' Retrieve tables from Calcur Databases
#' 
#' This function acts as an interface for other software to extract tables
#' from the databases in a way that keeps the other functions independent from
#' changes to the way the data are stored.  However, the names of the tables
#' and the basic table structure within each database must remain static. It only
#' works with 32 bit R if Access is 32 bit.  It is written with this presumption and
#' if there is a connection error then it will tell you to try 32 bit R if the version
#' being used is 64 bit.  If 64 bit Access is available it presumably will only work
#' with 64 bit R because the 32 and 64 bit ODBC implementations cannot be on the same machine
#' presently.  All access to the tables is accomplished with the R package RODBC.
#' 
#' There are 4 ways to run this function:
#' 1) getCalcurData() will list the codes for the databases and a description
#' 2) getCalcurData(db="Zc") specifying a database code (db) will list the tables in the database
#' 3) getCalcurData(db="Zc", tbl="Alive") will return the contents of the table as a dataframe.
#' 4) getCalcurData(db="Zc", tbl=c("Alive","Zcbrand")) will return a list of dataframes with a dataframe for
#' each table.
#' 
#' Note that the name of the tbl is not case-specific and need not match the
#' case in the Access database.  If specified as a vector of tables, the list
#' names will match the names specified (eg Alive and Zcbrand in the example above).
#' 
#' The default directory specification is dir="J:/Master" which is 
#' the directory calcur/databases/master.  If you specify dir="" then
#' it will expect to find the databases in the directory for the package location in the 
#' library.  I use that for testing and to have a local copy not on the network.
#' Any location can be used as long as the database file is at that location.
#' 
#' @param db name code for database; see databases.txt for a list of codes
#' @param tbl vector of table names in database
#' @param dir directory location for databases; if NULL uses value in databases.txt; if "" uses package directory
#' @return if db is null, prints list of databases; if db specified but tbl is null, prints 
#' list of tables; if both db and tbl are provided, returns the table as a dataframe
#' or a list of dataframes if more than one table requested.
#' @export
#' @author Jeff Laake
#' @examples
#' # examples that assume databases are in their standard location.
#' getCalcurData()
#' getCalcurData(db="Zc")
#' resights=getCalcurData(db="Zc", tbl="Alive")
#' str(resights)
#' datalist=getCalcurData(db="Zc", tbl=c("Alive","Zcbrand"))
#' names(datalist)
#' 
getCalcurData=function(db=NULL,tbl=NULL,dir=NULL)
{
#   Get file of database names (db), definitions and filenames
    sdir=system.file(package="CalcurData")
	databases=read.delim(file.path(sdir,"databases.txt"))
#   If null db, output list of databases
	if(is.null(db)|| !db %in% databases$db)
	  cat("Database names:\n",
			 paste(paste(format(databases$db[order(databases$db)],width=15),databases$description[order(databases$db)],sep=":\t"),collapse="\n "),"\n")
	else
	{
            # Connect to database if file exists
			if(is.null(dir))dir=databases$dir[databases$db==db]
			if(dir=="")dir=sdir
			fdir=file.path(dir,databases$filename[databases$db==db])
			if(file.exists(fdir))
			{
				if(strsplit(fdir,"\\.")[[1]][2]!="mdb")
            connection<-odbcConnectAccess2007(fdir)
        else
				    connection=odbcConnectAccess(fdir)
				if(connection==-1)
				{
					cat("\nError in connecting to database\n")
					if(unlist(strsplit(R.Version()$system,","))[1]=="x86_64")
						stop("Try 32 bit R")
					stop	
				}
			}
			else
				stop("Bad file location. Check dir value")
            # Get list of tables in the database
			tables=sqlTables(connection,tableType="TABLE")$TABLE_NAME
            # If null tbl value, print list of table names
			if(is.null(tbl))
			{
				cat("Tables in: ",db,"\n",paste(paste(tables,collapse="\n "),"\n"))
				odbcClose(connection)
			}
			else
                # If tbl is provided make sure that all values are valid
				if(any(!tolower(tbl)%in%tolower(tables)))
				{
					stop("Following tbl values not in tables:",paste(tbl[!tbl%in%tables],collapse=","))
					odbcClose(connection)
				}
				else
				{
                # Fetch each table in database and return as list if more than one
					if(length(tbl)==1)
					{
						datalist=sqlFetch(connection,tables[tolower(tbl)==tolower(tables)])
						
					}
					else
					{
						datalist=vector("list",length(tbl))
						for(i in 1:length(tbl))
							datalist[[i]]=sqlFetch(connection,tables[tolower(tbl[i])==tolower(tables)])
						names(datalist)=tbl
					}
					odbcClose(connection)
					return(datalist)
				}
						
	}
}
