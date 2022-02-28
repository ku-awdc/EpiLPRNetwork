#' Get an SQL connection to the specified table
#'
#' @param table_name the name of the table to return
#'
#' @import keyring
#' @import RODBC
#' @import odbc
#'
#' @export
get_sql_table <- function(table_name){

  # Only needs to be run once:
  # key_set_with_value("Dias_SQL_epiLPR", username="*driver string for epiLPR*", password=getPass::getPass())
  # key_set_with_value("Dias_SQL_IBSOR", username="*driver string for IB_SOR*", password=getPass::getPass())
  cstring <- key_list("Dias_SQL_epiLPR")[,"username"]
  stopifnot(length(cstring)==1)
  conn <- DBI::dbConnect(odbc::odbc(), .connection_string=str_c(cstring, key_get("Dias_SQL_epiLPR", username=cstring)), encoding="UTF-8")
  cstring <- key_list("Dias_SQL_IBSOR")[,"username"]
  stopifnot(length(cstring)==1)
  conn_sor <- DBI::dbConnect(odbc::odbc(), .connection_string=str_c(cstring, key_get("Dias_SQL_IBSOR", username=cstring)), encoding="latin1")

  if(table_name=="contacts"){
    tbl <- tbl(conn, dbplyr::in_schema("EpiLPR3", "data_contacts"))
  }else if(table_name=="procedures"){
    tbl <- tbl(conn, dbplyr::in_schema("EpiLPR3", "data_procedures"))
  }else if(table_name=="sor"){
    tbl <- tbl(conn_sor, dbplyr::in_schema("dbo", "Class_SOR"))
  }else if(table_name=="address"){
    tbl <- tbl(conn_sor, dbplyr::in_schema("dbo", "AddressInformation"))
  }else if(table_name=="organisation"){
    tbl <- tbl(conn_sor, dbplyr::in_schema("dbo", "OrganizationalUnit"))
  }else if(table_name=="institution"){
    tbl <- tbl(conn_sor, dbplyr::in_schema("dbo", "HealthInstitution"))
  }else if(table_name=="owner"){
    tbl <-tbl(conn_sor, dbplyr::in_schema("dbo", "InstitutionOwner"))
  }else{
    stop("table name not recognised")
  }

  return(tbl)

}
