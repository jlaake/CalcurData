#' Connect to Access database
#' 
#' Function used by getCalcurData and saveCalcurData to connect to Access databases.
#' 
#' @param databases list of valid databases
#' @param db name code for database; see databases.txt for a list of codes
#' @param dir directory location for databases; if NULL uses value in databases.txt; if "" uses package directory
#' @return connection if all good
#' @export
#' @author Jeff Laake
DBconnect=function(databases,db,dir)
{
  # Connect to database if file exists
  if(is.null(dir))dir=databases$dir[databases$db==db]
  if(dir=="")dir=sdir
  fdir=file.path(dir,databases$filename[databases$db==db])
  if(file.exists(fdir))
  {
    extensions=strsplit(fdir,"\\.")[[1]]
    if(extensions[length(extensions)]!="mdb")
      connection<-odbcConnectAccess2007(fdir)
    else
      connection=odbcConnectAccess(fdir)
    if(connection==-1)
    {
      cat("\nError in connecting to database\n")
      if(unlist(strsplit(R.Version()$system,","))[1]=="x86_64")
        stop("Try 32 bit R")
    } else
      return(connection)
  } else
    stop("Bad file location. Check dir value")
}