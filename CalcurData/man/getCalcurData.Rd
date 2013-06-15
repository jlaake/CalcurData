\name{getCalcurData}
\alias{getCalcurData}
\title{Retrieve tables from Calcur Databases}
\usage{
  getCalcurData(db = NULL, tbl = NULL, dir = NULL)
}
\arguments{
  \item{db}{name code for database; see databases.txt for a
  list of codes}

  \item{tbl}{vector of table names in database}

  \item{dir}{directory location for databases; if NULL uses
  value in databases.txt; if "" uses package directory}
}
\value{
  if db is null, prints list of databases; if db specified
  but tbl is null, prints list of tables; if both db and
  tbl are provided, returns the table as a dataframe or a
  list of dataframes if more than one table requested.
}
\description{
  This function acts as an interface for other software to
  extract tables from the databases in a way that keeps the
  other functions independent from changes to the way the
  data are stored.  However, the names of the tables and
  the basic table structure within each database must
  remain static. It only works with 32 bit R if Access is
  32 bit.  It is written with this presumption and if there
  is a connection error then it will tell you to try 32 bit
  R if the version being used is 64 bit.  If 64 bit Access
  is available it presumably will only work with 64 bit R
  because the 32 and 64 bit ODBC implementations cannot be
  on the same machine presently.  All access to the tables
  is accomplished with the R package RODBC.
}
\details{
  There are 4 ways to run this function: 1) getCalcurData()
  will list the codes for the databases and a description
  2) getCalcurData(db="Zc") specifying a database code (db)
  will list the tables in the database 3)
  getCalcurData(db="Zc", tbl="Alive") will return the
  contents of the table as a dataframe. 4)
  getCalcurData(db="Zc", tbl=c("Alive","Zcbrand")) will
  return a list of dataframes with a dataframe for each
  table.

  Note that the name of the tbl is not case-specific and
  need not match the case in the Access database.  If
  specified as a vector of tables, the list names will
  match the names specified (eg Alive and Zcbrand in the
  example above).

  The default directory specification is dir="J:/Master"
  which is the directory calcur/databases/master.  If you
  specify dir="" then it will expect to find the databases
  in the directory for the package location in the library.
  I use that for testing and to have a local copy not on
  the network. Any location can be used as long as the
  database file is at that location.
}
\examples{
# examples that assume databases are in their standard location.
getCalcurData()
getCalcurData(db="Zc")
resights=getCalcurData(db="Zc", tbl="Alive")
str(resights)
datalist=getCalcurData(db="Zc", tbl=c("Alive","Zcbrand"))
names(datalist)
}
\author{
  Jeff Laake
}

