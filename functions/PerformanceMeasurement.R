############################################################################
### A few basic performance measurement functions.                      ####
############################################################################
cumulativeReturns <- function(x){ return(cumprod(1+x/100)) }
customSharpe <- function(x){round(sqrt(252)*mean(x)/sd(x),digits=4)}
customAPR <- function(x){round((prod(1+x/100)^(252/length(x))-1)*100,digits=4)}