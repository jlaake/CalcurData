#' California Current Program Data Access
#' 
#' This package contains a single function getData that provides a single access
#' function to tables from the assortment of Access databases in 
#' Databases under Calcur image.  Having a single access point allows other packages like
#' CIPinnipedAnalysis, CuSurvival etc to be independent of the database which means
#' the databases could be changed to say Oracle and only this one function getData would need to be
#' changed and all of the other packages remain unchanged. 
#'  
#' The documentation for \code{\link{getData}} explains further the arguments and ways
#' that it can be used. The package contains a tab-delimited text file named databases.txt
#' which can be modified in anyway. There are 3 fields in each record: db, filename and
#' description. db is the code used to identify the database and it would not be clever to change the "db" field as
#' the other packages will rely on that being unchanged.  However, you can change the filename
#' or the description without any unintended consequences. You can add additional databases
#' as needed.   
#' 
#' \tabular{ll}{ Package: \tab CalcurData\cr Type: \tab Package\cr Version:
#' \tab 1.0\cr Date: \tab 2012-03-27\cr License: \tab GPL-2\cr LazyLoad: \tab
#' yes\cr }
#' 
#' @name CalcurData-package
#' @aliases CalcurData-package CalcurData
#' @docType package
#' @author Jeff Laake Maintainer: <Jeff.Laake@@noaa.gov>
#' @keywords package
#' 
NULL
