############################################################################
### Fetch timeseries data for SPDR ETF funds and SPY: Convert to xts    ####
############################################################################
fetchDataSectorSPDR <- function(conn=NULL)
{
  # Open DB connection
  if(is.null(conn))
    mydb <- connectionSecuritiesMaster()
  else
    mydb <- conn
  
  query = "select d.timestamp, s.ticker, d.price from datapoint d
            inner join timeseries t on t.id_timeseries = d.id_timeseries
            inner join symbol s on s.id_symbol = t.id_symbol
              where s.comment = 'INDEX' 
              and ticker in ('SPY','XLE','XLU','XLK','XLB','XLP','XLY','XLI','XLV','XLF');"
  
  # Fetch DB tuples
  timeSeriesDBTuples <- dbGetQuery(conn=mydb, statement=query)
  # Convert the DB tuples into xts series
  timeSeriesXTS <- processDBTuples(timeSeriesDBTuples)
  
  # Close the connection if it was created locally
  if(is.null(conn))
    dbDisconnect(mydb)
  
  return(timeSeriesXTS)
}

############################################################################
### Fetch timeseries data for 14 BEL20 stocks and BEL: Convert to xts   ####
############################################################################
fetchDataBEL20 <- function(conn=NULL)
{
  # Open DB connection
  if(is.null(conn))
    mydb <- connectionSecuritiesMaster()
  else
    mydb <- conn
  
  query = "select d.timestamp, s.ticker, d.price from datapoint d
            inner join timeseries t on t.id_timeseries = d.id_timeseries
            inner join symbol s on s.id_symbol = t.id_symbol
              where (s.ticker = 'BEL' or s.comment = 'BEL20') 
              and d.timestamp > '2002-11-01';"
  
  # Fetch DB tuples
  timeSeriesDBTuples <- dbGetQuery(conn=mydb, statement=query)
  # Convert the DB tuples into xts series
  timeSeriesXTS <- processDBTuples(timeSeriesDBTuples)
  
  # Close the connection if it was created locally
  if(is.null(conn))
    dbDisconnect(mydb)
  
  return(timeSeriesXTS)
}

############################################################################
### Fetch timeseries data for the S&P500 stocks: Convert to xts         ####
############################################################################
fetchDataSnP500 <- function(conn=NULL)
{
  # Open DB connection
  if(is.null(conn))
    mydb <- connectionSecuritiesMaster()
  else
    mydb <- conn
  
  query <- "select d.timestamp, s.ticker, d.price from datapoint d
                inner join timeseries t on t.id_timeseries = d.id_timeseries
                inner join symbol s on s.id_symbol = t.id_symbol
                  where s.comment = 'S&P500';"
  
  # Fetch DB tuples
  timeSeriesDBTuples <- dbGetQuery(conn=mydb, statement=query)
  # Convert the DB tuples into xts series
  timeSeriesXTS <- processDBTuples(timeSeriesDBTuples)
  
  # Close the connection if it was created locally
  if(is.null(conn))
    dbDisconnect(mydb)
  
  return(timeSeriesXTS)
}

############################################################################
### Initialize SPDR ETF trading Cost and transactionStructure           ####
############################################################################
fetchTransactionCostsSPDR <- function()
{
  nrAssets <- 9
  # Commisssion per share, fixed, percentage, min, max
  SPDR.commission <- c(0.0035, 0, 0, 0.35, Inf)
  # Exchange costs per share, fixed, percentage, min, max
  SPDR.exchangeFee <- c(0.00275, 0, 0, 0, Inf)
  # Clearning cost per share, fixed, percentage, min, max
  SPDR.clearingFee <- c(0.0002, 0, 0, 0, Inf)
  
  # Spreads and random slippage for SPDR, expressed as a fraction (spread / shareprice)
  SPDR.spreads <- data.frame(XLE=c(0.00016, 0.00008),
                             XLU=c(0.0002, 0.0001),
                             XLK=c(0.00024, 0.00012),
                             XLB=c(0.00023, 0.00012),
                             XLP=c(0.0002, 0.0001),
                             XLY=c(0.00013, 0.00008),
                             XLI=c(0.00018, 0.00009),
                             XLV=c(0.00015, 0.00008),
                             XLF=c(0.00045, 0.0002))
  # Express and random slippage expressed as percentage
  SPDR.spreads <- SPDR.spreads*100
  
  # Aggregate commission structure information
  SPDR.commissionStructure <- c(SPDR.commission, SPDR.exchangeFee, SPDR.clearingFee)
  SPDR.commissionStructure <- as.data.frame(matrix(SPDR.commissionStructure, 
                                                   nrow=length(SPDR.commissionStructure), ncol=nrAssets, byrow=FALSE))
  names(SPDR.commissionStructure) <- names(SPDR.spreads)
  # Aggregate cost information
  SPDR.tradingCosts <- rbind(SPDR.spreads, SPDR.commissionStructure)
  # Add rowname info
  rownames(SPDR.tradingCosts) <- c("Spread.Percentage", "Slippage.Percentage",
                                  "Commission.Share", "Commission.Fixed", "Commission.Percentage", 
                                  "Commission.Min", "Commission.Max",
                                  "Exchange.Share", "Exchange.Fixed", "Exchange.Percentage", 
                                  "Exchange.Min", "Exchange.Max",
                                  "Clearing.Share", "Clearing.Fixed", "Clearing.Percentage", 
                                  "Clearing.Min", "Clearing.Max")
  
  return(SPDR.tradingCosts)
}

############################################################################
### Initialize BEL20 stock trading Cost and transactionStructure        ####
############################################################################
fetchTransactionCostsBEL20 <- function()
{
  nrAssets <- 14
  # Commisssion per share, fixed, percentage, min, max
  BEL20.commission <- c(0, 0, 0.08, 1.25, Inf)
  # Exchange costs per share, fixed, percentage, min, max
  BEL20.exchangeFee <- c(0, 0.15, 0.0095, 0, Inf)
  
  # Spreads and random Slippage for BEL20, expressed as a fraction (spread / shareprice)
  BEL20.spreads <- data.frame(ACKB=c(0.0008, 0.0004),
                              AGS=c(0.0003, 0.0001),
                              ABI=c(0.0005, 0.00005),
                              BEFB=c(0.0007, 0.00008),
                              BEKB=c(0.00055, 0.0001),
                              COFB=c(0.001, 0.0001),
                              COLR=c(0.0003, 0.00001),
                              DELB=c(0.00054, 0.00005),
                              DIE=c(0.0018, 0.0002),
                              GBLB=c(0.00056, 0.00006),
                              KBC=c(0.0003, 0.0001),
                              SOLB=c(0.00047, 0.00005),
                              UCB=c(0.00043, 0.0001),
                              UMI=c(0.0008, 0.0001))
  # Spreads and random slippage Expressed as percentage
  BEL20.spreads <- BEL20.spreads*100
  
  # Aggregate commission structure information
  BEL20.commissionStructure <- c(BEL20.commission, BEL20.exchangeFee)
  BEL20.commissionStructure <- as.data.frame(matrix(BEL20.commissionStructure, 
                                                    nrow=length(BEL20.commissionStructure), 
                                                    ncol=nrAssets, byrow=FALSE))
  names(BEL20.commissionStructure) <- names(BEL20.spreads)
  
  # Clearing cost per share, fixed, percentage, min, max
  BEL20.clearingFee <- c(0, 0.1, 0, 0, Inf)
  BEL20.clearingFee <- as.data.frame(matrix(BEL20.clearingFee, 
                                            nrow=length(BEL20.clearingFee), ncol=nrAssets, byrow=FALSE))
  names(BEL20.clearingFee) <- names(BEL20.spreads)
  # If blue chip stocks then use 0.04 fixed, otherwise 0.10 fixed
  BEL20.clearingFee[2, c("AGS", "ABI", "COLR", "DELB", 
                         "KBC", "SOLB", "UCB", "UMI")] <- 0.04
  
  # Aggregate all cost information
  BEL20.tradingCosts <- rbind(BEL20.spreads, BEL20.commissionStructure, BEL20.clearingFee)
  # Add rownames
  rownames(BEL20.tradingCosts) <- c("Spread.Percentage", "Slippage.Percentage",
                                "Commission.Share", "Commission.Fixed", "Commission.Percentage", 
                                "Commission.Min", "Commission.Max",
                                "Exchange.Share", "Exchange.Fixed", "Exchange.Percentage", 
                                "Exchange.Min", "Exchange.Max",
                                "Clearing.Share", "Clearing.Fixed", "Clearing.Percentage", 
                                "Clearing.Min", "Clearing.Max")
  
  return(BEL20.tradingCosts)
}


############################################################################
### Converts database tuples to an xts object with pricedata            ####
### Data is alligned and NA's removed (locf)                            ####
############################################################################
processDBTuples <- function(timeseries)
{
  # Get Unique tickers from timeSeries dataframe
  tickers <- unique(timeseries$ticker)
  # get Unique timestamps from timeSeries dataframe
  timestamps <- as.POSIXlt(unique(timeseries$timestamp))
  
  # Create dataframe containing the unique timestamps
  df <- data.frame(timestamps=timestamps)
  # Order the timestamps
  df <- data.frame(df[order(df$timestamps),])
  # Add NA columns for every ticker
  df <- data.frame(cbind(df,matrix(NA, nrow = nrow(df), ncol = length(tickers))))
  # Add column name
  names(df) <- c("timestamp",tickers)
  
  for(i in seq(1,length(tickers)))
  {
    # Get timestamps and price data for symbol i
    series <- timeseries[timeseries$ticker==tickers[[i]],c(1,3)]
    # convert timestamps to POXIXlt objects
    seriesTimestamps <- as.POSIXlt(series[,1])
    seriesData <- series[,2]
    
    # Align prices for this symbol with the timestamps in the dataframe
    # This is necessary because not all tickers have data for the same timestams
    df[as.POSIXct(df$timestamp) %in% as.POSIXct(seriesTimestamps),i+1] <- seriesData
  }
  
  # Remove instruments for which there is no data at the start of the interval
  df <- df[,c(TRUE,!(is.na(df[1,-1])))]
  # Remove instruments for which there is no data at the end of the interval
  df <- df[,c(TRUE,!is.na(df[nrow(df),-1]))]
  # remove NA's, last observation carried foreard
  df <- na.locf(df,fromLast=FALSE)
  
  df[, 2:ncol(df)] <- sapply(df[, 2:ncol(df)], as.numeric)
  
  # Convert to XTS object
  seriesXTS <- as.xts(df[,-1],order.by=as.POSIXlt(df[,1]))
  
  return(seriesXTS)
}