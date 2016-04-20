############################################################################
## Generic functions for interaction with the Securities Master Database ##
############################################################################
library(RMySQL)
# Return connection to the securities master database
# Caller is responsible for closing the connection
connectionSecuritiesMaster <- function()
{
  sqlDriver = dbDriver("MySQL")
  credentials = file.path(getwd(),"config","credentials.cnf")
  securitiesMasterGroup = "algotrading_securities_master" 
  mydb = dbConnect(sqlDriver, default.file=credentials, group=securitiesMasterGroup)
  
  return(mydb)
}

# This generic function allows the user to perform DB query's of the following form:
# SELECT columns FROM tableName WHERE conditions. View DBFacadeExample.R, example 1.
#
# select (vector): column names
# tableName (string): the name of the table
# where (dataframe): each individual row value represents a condition 
# of the form 'columnname = rowvalue".
#
# Note: a separate query will be executed for each distinct row in the 
# 'where' dataframe. 
# One dataframe containing the result for ALL the querys
# will be returned
queryDB <- function(select,tableName,where,conn=NULL,printQuery=FALSE)
{
  # Open DB connection
  if(is.null(conn))
    mydb <- connectionSecuritiesMaster()
  else
    mydb <- conn
  
  whereCols <- names(where)
  whereRowCount <- nrow(where)
  where[] <- apply(where,2,as.character)
  
  resultSet <- data.frame()
  selectStr <- paste("SELECT",paste(select,collapse=", "),"FROM",tableName)
  
  for(i in seq(1,whereRowCount))
  {
    if(!is.null(where))
      conditionStr <- paste("WHERE ",paste(whereCols,"=",paste("'",
                        where[i,],"'",sep=""),collapse=" and "),";",sep="")
    else
      conditionStr <- ";"
    
    queryStr <- paste(selectStr,conditionStr)
    if(printQuery) 
      print(queryStr)
    
    queryResult <- dbGetQuery(mydb,queryStr)
    resultSet <- rbind(resultSet,queryResult)
  }
  
  # Close the connection if it was created locally
  if(is.null(conn))
    dbDisconnect(mydb)
  
  return(resultSet)
}


# This generic function allows the user to perform DB update query's of the following form:
# UPDATE table SET column WHERE condition. View DBFacadeExample.R, example 2.
#
# tableName (string): the name of the table that should be updated
# set
# where (dataframe): each individual row value represent a condition 
# of the form 'columnname = rowvalue".
#
# Note: a separate query will be executed for each distinct row in the 
# 'where' dataframe. One dataframe containing the result for ALL the querys
# will be returned
updateDB <- function(tableName,set,where,conn=NULL,printQuery=FALSE)
{ 
  # Open DB connection
  if(is.null(conn))
    mydb <- connectionSecuritiesMaster()
  else
    mydb <- conn

  updateStr <- paste("UPDATE",tableName,"SET")

  setCols <- names(set); set[] <- apply(set,2,as.character)
  whereCols <- names(where); where <- apply(where,2,as.character)
  
  for(i in seq(1,nrow(set)))
  {
    valueStr <- paste(setCols,"=",paste("'",set[i,],"'",sep=""),collapse=", ")
    conditionStr <- paste("WHERE ",paste(whereCols,"=",paste("'",where[i,],"'",sep=""),collapse=" and "),";",sep="")
    queryStr <- paste(updateStr,valueStr,conditionStr)
    if(printQuery) 
      print(queryStr)
    
    dbGetQuery(mydb,queryStr)
  }
  
  # Close the connection if it was created locally
  if(is.null(conn))
    dbDisconnect(mydb)
}


insertUpdateDB <- function(tableName,seriesData,conn=NULL,printQuery=FALSE)
{
  # Open DB connection
  if(is.null(conn))
    mydb <- connectionSecuritiesMaster()
  else
    mydb <- conn
  
  # Generate insertion query
  colNames <- names(seriesData)
  insertionStr <- paste("INSERT INTO ",tableName, "(", paste(colNames, collapse=", "), ")", sep="")
  valueListStr <- paste("'", apply(seriesData[,colNames], 1, paste, collapse="', '"), "'", sep="") 
  valueListStr <- paste("VALUES (", paste(valueListStr, collapse="), ("), ")",sep="")
  updateStr <- ";"
  
  # If seriesdata contains the primary key column, then we must update
  # the rows for which the keys are already present in the dB
  keyStr <- paste("SHOW KEYS FROM",tableName,"WHERE Key_name = 'PRIMARY';")
  primaryKey <- dbGetQuery(mydb,keyStr)$Column_name
  
  if(primaryKey %in% colNames) # Primary key already in DB
  {
    colNamesWithoutPK <- colNames[!(colNames %in% primaryKey)]
    updateStr <- paste("ON DUPLICATE KEY UPDATE ",
                      paste(paste(colNamesWithoutPK,"=VALUES(",colNamesWithoutPK,")",sep=""),collapse=", "),";",sep="")
  }
  
  # Generate the complete query string
  insertUpdateQueryStr <- paste(insertionStr,valueListStr,updateStr)
  if(printQuery) print(insertUpdateQueryStr)
  
  # Execute the insertion / update
  dbGetQuery(mydb,insertUpdateQueryStr)
  
  # Close the connection if it was created locally
  if(is.null(conn))
    dbDisconnect(mydb)
}

# This function sets the 'isblocked' symbol flag to true or false for the timeseries
# that are linked to a particular ticker/symbol
# Optionally, a blocking reason can also be added
blockActivateTimeseriesDB <- function(ticker,block=TRUE,reason=NULL,conn=NULL)
{
  if(is.null(conn))
    mydb=connectionSecuritiesMaster()
  else
    mydb=conn
  
  # Fetch the tickers' symbol_id
  idSymbol = dbGetQuery(mydb,paste("select id_symbol from symbol where ticker = '",ticker,"';",sep=""))
  
  flag = "TRUE" # Set timeseries.isblocked to TRUE when blocking the symbols' timeseries
  if(!block)
  {
    flag= "FALSE" # Set timeseries.reasonBlocked to FALSE when deblocking the timeseries
    updateQuery = paste("update timeseries set reasonblocked = NULL where id_symbol = ",idSymbol,";",sep="")
    print(updateQuery); dbGetQuery(mydb,updateQuery)
  }
  else if(!is.null(reasonBlocked)) # Add blocking reason to DB
  {
    updateQuery = paste("update timeseries set reasonblocked = '",reason,"' where id_symbol = ",idSymbol,";",sep="")
    print(updateQuery); dbGetQuery(mydb,updateQuery)
  }
  
  # Execute the update query
  updateQuery = paste("update timeseries set isblocked = ",flag," where id_symbol = ",idSymbol,";",sep="")
  print(updateQuery); dbGetQuery(mydb,updateQuery)
  
  # Close the connection if it was created locally
  if(is.null(conn))
    dbDisconnect(mydb)
}