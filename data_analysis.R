# run the data prep script
source("data_prep.R")

# common resolution for projectors is 1024 × 768
w <- 1024 * 0.8
h <- 768 * 0.6

library(PerformanceAnalytics)

png("figures/sector_etf.png", width = w, height = h, units = "px")
chart.RiskReturnScatter(R.sector, main = "Sector ETFs Annualized Return and Risk")
dev.off()
