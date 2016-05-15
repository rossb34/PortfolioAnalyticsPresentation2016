# https://tradeblotter.wordpress.com/2012/06/04/download-and-parse-edhec-hedge-fund-indexes/
library(xts)
df <- read.table("data/history.csv", header=TRUE, as.is=TRUE, sep=";")
head(df)
edhec <- xts(apply(df[,-1], 2, function(x) as.numeric(gsub("%", "", x))) / 100, as.Date(df[,1], format="%d/%m/%Y"))
save(edhec, file="data/edhec.rda")
