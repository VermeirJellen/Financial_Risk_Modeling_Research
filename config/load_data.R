############################################################################
### Loading time series data from external DB - Convert to xts         #####
############################################################################
# Create DB connection
mydb <- connectionSecuritiesMaster()

# Fetch Select Sector SPDR and SPY data
SnPIndices <- fetchDataSectorSPDR(conn=mydb)
# Extract SPY timseries
SPY <- SnPIndices$SPY

# Fetch BEL20 index and stock data
BEL20 <- fetchDataBEL20(conn=mydb)
# Extract BEL timeseries
BEL <- BEL20$BEL
# Extract AGS timeseries
AGS <- BEL20$AGS
# Extract ACKB timeseries
ACKB <- BEL20$ACKB

# Disconnect from DB
dbDisconnect(mydb)

# Fetch SPDR trading Costs
SPDR.transactionCosts <- fetchTransactionCostsSPDR()
# Fetch BEL20 trading costs
BEL20.transactionCosts <- fetchTransactionCostsBEL20()