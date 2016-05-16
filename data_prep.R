Sys.setenv(TZ="UTC")
library(quantmod)
library(PerformanceAnalytics)

# Load the updated edhec dataset
load("data/edhec.rda")

# Prep data for examples
# Abreviate column names for convenience and plotting
colnames(edhec) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV", "SS", "FoF")


if(file.exists("data/sector.rda")){
  # load the sector etf returns and the market data environment
  load("data/sector.rda")
  load("data/market.rda")
  load("data/md_env.RData")
} else {
  md <- new.env()
  # SP500 ETF as a proxy of the market
  mkt.sym <- "SPY"
  # sector ETFs
  sec.sym <- c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
  # commodity ETFs
  # com.sym <- c("GLD", "SLV", "USO", "DBA", "UNG")
  # symbols <- c(mkt.sym, eq.sym, com.sym)
  symbols <- c(mkt.sym, sec.sym)
  getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='1999-01-01', env = md)
  for(symbol in symbols) {
      x <- md[[symbol]]
      x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)
      indexFormat(x) <- '%Y-%m-%d'
      colnames(x) <- gsub("x",symbol,colnames(x))
      md[[symbol]] <- x
  }
  ret <- na.omit(Return.calculate(do.call(cbind, eapply(md, function(x) Ad(x))), "discrete"))
  colnames(ret) <- gsub("\\.[^.]*$", "", colnames(ret))
  # sector and market returns
  R.sector <- ret[,sec.sym]
  R.mkt <- ret[, mkt.sym]
  # save the sector etf returns and the market data environment
  save(R.sector, file="data/sector.rda")
  save(R.mkt, file="data/market.rda")
  save(md, file="data/md_env.RData")
}
