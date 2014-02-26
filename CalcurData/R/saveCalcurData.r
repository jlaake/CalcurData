#' Retrieve tables from Calcur Databases
#' 
#' This function acts as an interface for other software to save tables
#' in the databases in a way that keeps the other functions independent from
#' changes to the way the data are stored.  However, the names of the tables
#' and the basic table structure within each database must remain static. It only
#' works with 32 bit R if Access is 32 bit.  It is written with this presumption and
#' if there is a connection error then it will tell you to try 32 bit R if the version
#' being used is 64 bit.  If 64 bit Access is available it presumably will only work
#' with 64 bit R because the 32 and 64 bit ODBC implementations cannot be on the same machine
#' presently.  All access to the tables is accomplished with the R package RODBC.
#' 
#' There is only one way to run this function:
#' 1) saveCalcurData(x, db="Zc", tbl="Alive") will save the contents in x to table tbl in the specified 
#' dataframe after removing any existing table with that name.  Names here are case-specific unlike
#' with getCalcurData.
#' 
#' The default directory specification is the one in databases.txt.  If you specify dir="" then
#' it will expect to find the databases in the directory for the package location in the 
#' library.  I use that for testing and to have a local copy not on the network.
#' Any location can be used as long as the database file is at that location.
#' 
#' @param x dataframe to be saved as table
#' @param db name code for database; see databases.txt for a list of codes
#' @param tbl name of table in database
#' @param dir directory location for databases; if NULL uses files with package
#' @return NULL
#' @export
#' @author Jeff Laake
#' 
saveCalcurData=function(x,db,tbl,dir=NULL)
{
#   Get file of database names (db), definitions and filenames
	sdir=system.file(package="CalcurData")
	databases=read.delim(file.path(sdir,"databases.txt"))
#   If null db, output list of databases
	if(is.null(db)|| !db %in% databases$db)
		cat("db must be specified and be a valid code in databases.txt:\n",
				paste(paste(format(databases$db[order(databases$db)],width=15),databases$description[order(databases$db)],sep=":\t"),collapse="\n "),"\n")
	else
	{
		# Connect to database if file exists
		if(is.null(dir))
			dir=databases$dir[databases$db==db]
		else
		    if(dir=="")dir=sdir
		fdir=file.path(dir,databases$filename[databases$db==db])
		if(file.exists(fdir))
		{
			connection=odbcConnectAccess2007(fdir)
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
		# If tbl is provided make sure that all values are valid
		if(length(tbl)>1)
		{
		   stop("\nOnly one value of tbl can be specified\n")		
		}else
		{
		   # drop table in database and then store new table
			xx=sqlDrop(connection,tbl,errors=FALSE)
			xx=sqlSave(connection,x,tbl,append=FALSE,rownames=FALSE)
		}
		odbcClose(connection)
		return(NULL)	
	}
}
