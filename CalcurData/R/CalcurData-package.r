#' California Current Program Data Access
#' 
#' This package contains two functions 1) getCalcurData that provides a single access
#' function to tables from the assortment of Access databases in 
#' Databases under Calcur image and 2) saveCalcurData that provides a single access function
#' to save a table in one of the Access databases. Having a single access point allows other packages like
#' CIPinnipedAnalysis, CuSurvival etc to be independent of the database which means
#' the databases could be changed to say Oracle and only this one function getCalcurData would need to be
#' changed and all of the other packages remain unchanged. 
#'  
#' The documentation for \code{\link{getCalcurData}} and \code{\link{saveCalcurData}} explains further the arguments and ways
#' they can be used. The package contains a tab-delimited text file named databases.txt
#' which can be modified in anyway. There are 4 fields in each record: db, filename,
#' description and dir. db is the code used to identify the database and it would not be clever to change the "db" field as
#' the other packages will rely on that being unchanged. The dir is the default directory to be used for the
#' database. You can change the filename, dir or the description without any unintended consequences. You can
#' also add databases as needed.   
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
