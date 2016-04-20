############################################################################
## Example interaction with the Securities Master Database                ##
############################################################################
source("DBFacade.R")
DBCon <- connectionSecuritiesMaster();

# 0. Example select query without DB facade
queryString <- "select id_symbol,ticker from symbol where where comment = 'BEL20'"
bel20 <- dbGetQuery(DBCon,queryString)

# 1. Example select query with facade
# SELECT id_symbol, ticker, id_sector_icb 
# FROM timeseries
# WHERE id_sector_icb in (1,3,5);
# AND currency = EUR;
select = c("id_symbol","ticker","id_sector_icb","currency")
tableName = "symbol";
where = data.frame(id_sector_icb = c(1,3,5), currency = rep("EUR",3))

resultSet <- queryDB(select,tableName,where,conn=DBCon,printQuery=TRUE)
resultSet


# 2. Example update query 
# (Warning: Uncommenting and runnning the code below will perform a DB modification!)
#
# UPDATE symbol
# SET id_exchange = 'EURONEXTId'
# where id_symbol in 'BEL20Symbols'
#
# euronextExchangeId <- queryDB(select="id_exchange",tableName="exchange",
#                              where=data.frame(abbrev="EURONEXT"))
# symbolsIdBel20 <- queryDB(c("id_symbol"),"symbol",data.frame(comment="BEL20"))
#
# tableName <- "symbol"
# set <- data.frame(id_exchange = rep(as.numeric(euronextExchangeId),nrow(symbolsIdBel20)))
# where <- data.frame(id_symbol = as.vector(symbolsIdBel20))
#
# updateDB(tableName,set,where,DBCon,printQuery=TRUE)

dbDisconnect(DBCon)