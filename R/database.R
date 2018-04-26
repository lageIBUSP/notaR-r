#' Global configuration
#'
#' The PATH global variable must be defined when notaR is used by a web interface, in order
#' to correctly set up the data files. For local use, leave it defined as the current working directory.
#' @export
#' @rdname global
PATH=getwd()

#' Database utilities
#'
#' Function used to connect with the database with default or specified credentials and "cache" this connection
#' If a connection is already established, this function
#' will simply return the same connection.
#' Usually, this will be used internally by the other functions. 
#' 
#' @export
#' @import RMySQL DBI
#' @rdname database
#' @param dbuser Database user
#' @param dbpass Database password
#' @param dbname MySQL database name
#' @examples
#' con = connect()
connect <- function (dbuser = 'notaR', dbpass = 'notaR', dbname = 'notaR') {
    # Do we already have a connection?
    con = try(get(".connection", envir=globalenv()), silent=TRUE)
    if (inherits(con, "try-error")) {
        con = dbConnect(MySQL(), user=dbuser, password=dbpass, dbname=dbname)
        assign(".connection", con, envir=globalenv())
    }
    return(con)
}
# Construida na chamada PHP como 
# connect($DBUSER, $DBPASS, $DBNAME)

#' Accessory function to test if a database query returned no results
#' @export
#' @rdname database
#' @param object The result from a database query
no.results <- function(object) {
    length(object[,1]) == 0
}
