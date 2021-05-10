#' Title
#'
#' @param table_name
#' @param database_name
#'
#' @return
#' @export
#'
#' @examples
get_sqL_table <- function(table_name, database_name, username, password){

  conn <- DBI::dbConnect(odbc::odbc(), .connection_string=str_c('driver={SQL Server};server=', server_name, ';database=', database_name, ';trusted_connection=true;UID=', username, ';PWD=', password))


}
