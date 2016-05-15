# Load the updated edhec dataset
load("data/edhec.rda")

# Prep data for examples
# Abreviate column names for convenience and plotting
colnames(edhec) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV", "SS", "FoF")


if(file.exists("data/sector.rda")){
  load("data/sector.rda")
} else {
  md <- new.env()
  symbols <- c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
  getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='1999-01-01', env = md)
  for(symbol in symbols) {
      x <- md[[symbol]]
      x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)
      indexFormat(x) <- '%Y-%m-%d'
      colnames(x) <- gsub("x",symbol,colnames(x))
      md[[symbol]] <- x
  }
  ret.sector <- na.omit(Return.calculate(do.call(cbind, eapply(md, function(x) Ad(x))), "discrete"))
  save(ret.sector, file="data/sector.rda")
}
