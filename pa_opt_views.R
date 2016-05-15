
# run the data prep script
source("data_prep.R")

library(PortfolioAnalytics)
library(quantmod)
library(foreach)
library(iterators)

# sector returns
R <- R.sector

# trend indicator
trend.ind <- na.omit(cbind(Ad(md[[mkt.sym]]), TTR::EMA(Ad(md[[mkt.sym]]), n = 12)))
regime <- ifelse(trend.ind$SPY.Adjusted > trend.ind$EMA, 1, 2)

##### Long-Short Ranking #####
# long-short weights based on past returns
ls.weights <- function(R, on="months", n.lookback=12, n.top=3){
  ep <- endpoints(R, on=on)
  ep <- ep[ep > 0]
  out <- foreach(e = iter(ep), .combine=rbind) %do% {
    ret <- Return.cumulative(tail(R[0:e], n.lookback))
    # rank of 1 is the asset with the lowest return
    # rank of n is the asset with the highest return for n assets
    tmp.w <- rep(0, ncol(ret))
    tmp.rank <- rank(ret, ties.method = "random")
    tmp.w[tmp.rank >= (ncol(R) - n.top + 1)] <- -1 / n.top # 1.3 / n.top
    tmp.w[tmp.rank <= n.top] <- 1 / n.top # -0.3 / n.top
    tmp.w
  }
  out <- xts(out, order.by = index(R[ep]))
  colnames(out) <- colnames(R)
  out
}
w <- ls.weights(R, on="quarters", n.lookback=12)
r.ls.rank <- Return.portfolio(R, weights = w, geometric = FALSE)
colnames(r.ls.rank) <- "ls.ranking"


##### naive dollar neutral portfolio #####
# naive portfolio
portf.naive <- portfolio.spec(colnames(R))
portf.naive <- add.constraint(portf.naive, type="weight_sum",
                              min_sum=-0.25, max_sum=0.25)
portf.naive <- add.constraint(portf.naive, type="box", min=-0.5, max=0.5)
portf.naive <- add.constraint(portf.naive, type = "leverage_exposure", leverage=2)
portf.naive <- add.objective(portf.naive, type="risk", name="StdDev")
portf.naive <- add.objective(portf.naive, type="risk_budget",
                             name="StdDev", max_prisk=0.50)
rp.naive <- random_portfolios(portf.naive, permutations=1000, rp_method='sample')

opt.naive <- optimize.portfolio.rebalancing(R, portf.naive,
                                            optimize_method="random",
                                            rebalance_on="quarters",
                                            training_period=36,
                                            rp=rp.naive, trace=TRUE)
# compute arithmetic portfolio returns because of negative weights
ret.naive <- Return.portfolio(R, extractWeights(opt.naive), geometric = FALSE)
colnames(ret.naive) <- "naive.ls"
charts.PerformanceSummary(cbind(ret.naive, R.mkt))
table.AnnualizedReturns(cbind(ret.naive, R.mkt))

##### Separate Long and Short Portfolios #####
# long portfolio
p.long <- portfolio.spec(assets=colnames(R))
p.long <- add.constraint(p.long, type="weight_sum",
                         min_sum=0.99, max_sum=1.01)
p.long <- add.constraint(p.long, type="box", min=0, max=0.85)
p.long <- add.objective(p.long, type="risk", name="StdDev")
p.long <- add.objective(p.long, type="risk_budget",
                        name="StdDev", max_prisk=0.50)
rp.long <- random_portfolios(p.long, permutations=1000, rp_method='sample')
opt.long <- optimize.portfolio.rebalancing(R, p.long,
                                           optimize_method = "random",
                                           trace = TRUE, rp = rp.long,
                                           rebalance_on = "quarters",
                                           training_period = 36)
r.long <- Return.portfolio(R, extractWeights(opt.long), geometric = FALSE)
colnames(r.long) <- "long"

# short portfolio
p.short <- portfolio.spec(assets=colnames(R))
p.short <- add.constraint(p.short, type="weight_sum",
                          min_sum=-1.01, max_sum=-0.99)
p.short <- add.constraint(p.short, type="box", min=-0.85, max=0)
p.short <- add.objective(p.short, type="risk", name="StdDev")
p.short <- add.objective(p.short, type="risk_budget",
                         name="StdDev", max_prisk=0.50)
rp.short <- random_portfolios(p.short, permutations=1000, rp_method='sample')
opt.short <- optimize.portfolio.rebalancing(R, p.short,
                                            optimize_method = "random",
                                            trace = TRUE, rp = rp.short,
                                            rebalance_on = "quarters",
                                            training_period = 36)
r.short <- Return.portfolio(R, extractWeights(opt.short), geometric = FALSE)
colnames(r.short) <- "short"

# returns from the long and short portfolio
z.ls <- cbind(r.long, r.short)

# combined portfolio
p <- portfolio.spec(assets=colnames(z.ls))
p <- add.constraint(p, type="weight_sum",
                    min_sum=0.99, max_sum=1.01)
p <- add.constraint(p, type="box", min=0.1, max=1)
p <- add.objective(p, type="return", name="mean")
p <- add.objective(p, type="risk", name="ES", arguments=list(p=0.9))
rp <- random_portfolios(p, permutations=1000, rp_method='sample')

opt.ls <- optimize.portfolio.rebalancing(z.ls, p,
                                         optimize_method = "random",
                                         trace = TRUE, rp = rp,
                                         rebalance_on = "quarters",
                                         training_period = 36)
r.ls <- Return.portfolio(z.ls, extractWeights(opt.ls), geometric = FALSE)
colnames(r.ls) <- "ls"

##### Separate Long and Short with Trend Indicator and Regime Portfolios #####
# short portfolio with trend indicator and regime model

# use the regime switching model for the short portfolio
# we use the SPY 12 period EMA as an indicator
# regime 1: Price > EMA
# regime 2: Price <= EMA

# the short portfolio should reduce exposure when in regime 1
# the basic idea here is that the trend indicator is signaling that the equity
# market is trending higher and we are expressing a long bias view by reducing
# exposure in the short portfolio
p.short.1 <- portfolio.spec(assets=colnames(R))
p.short.1 <- add.constraint(p.short.1, type="weight_sum",
                            min_sum=-0.26, max_sum=-0.24)
p.short.1 <- add.constraint(portfolio=p.short.1, type="box", min=-0.25, max=0)
p.short.1 <- add.objective(portfolio=p.short.1, type="risk", name="StdDev")
p.short.1 <- add.objective(p.short.1, type="risk_budget",
                           name="StdDev", max_prisk=0.50)
# rp.short.1 <- random_portfolios(p.short.1, permutations=1000, rp_method='sample')

# the short portfolio for regime 2 is the same as the short portfolio we
# defined earlier
p.short.2 <- p.short
# define the portfolios for the regime model
# regime 1: Price > EMA
# regime 2: Price <= EMA
regime.port <- regime.portfolios(regime,
                                 combine.portfolios(list(p.short.1, p.short.2)))
# run the optimization for the regime model
opt.short.regime <- optimize.portfolio.rebalancing(R, regime.port,
                                                   optimize_method="random",
                                                   rebalance_on="quarters",
                                                   training_period=36,
                                                   search_size=1000,
                                                   trace=TRUE)
r.short.regime <- Return.portfolio(R, extractWeights(opt.short.regime), geometric = FALSE)
colnames(r.short.regime) <- "short.regime"

# returns from the long and short portfolio
z.ls.regime <- cbind(r.long, r.short.regime)

# combined portfolio
p <- portfolio.spec(assets=colnames(z.ls.regime))
p <- add.constraint(p, type="weight_sum",
                    min_sum=0.99, max_sum=1.01)
p <- add.constraint(p, type="box", min=0.1, max=1)
p <- add.objective(p, type="return", name="mean")
p <- add.objective(p, type="risk", name="ES", arguments=list(p=0.9))
rp <- random_portfolios(p, permutations=1000, rp_method='sample')

opt.ls.regime <- optimize.portfolio.rebalancing(z.ls.regime, p,
                                                optimize_method = "random",
                                                trace = TRUE, rp = rp,
                                                rebalance_on = "quarters",
                                                training_period = 36)
r.ls.regime <- Return.portfolio(z.ls.regime, extractWeights(opt.ls.regime), geometric = FALSE)
colnames(r.ls.regime) <- "ls.regime"

ret.all <- cbind(r.ls, r.ls.regime)
charts.PerformanceSummary(ret.all)
table.AnnualizedReturns(ret.all)



z <- cbind(r.ls, r.ls.regime, r.ls.rank)
charts.PerformanceSummary(z)
table.AnnualizedReturns(z)













##### scratch #####
# #' The 'top level' portfolio has objectives for equal contribution to risk
# #' where modified ES is the risk measure.
# portf <- portfolio.spec(assets=c("long", "short"))
# portf <- add.constraint(portfolio=portf, type="weight_sum",
#                         min_sum=0.99, max_sum=1.01)
# portf <- add.constraint(portfolio=portf, type="box", min=0.3, max=1)
# portf <- add.objective(portfolio=portf, type="risk", name="StdDev")
# # portf <- add.objective(portfolio=portf, type="risk", name="ES",
# #                        arguments=list(p=0.9))
#
# #' Specify a mult-layer portfolio.
# mult.portf <- mult.portfolio.spec(portf)
#
# #' Add portf1 as a sub portfolio with optimization parameters specific to
# #' running optimize.portfolio.rebalancing with portf1.
# mult.portf <- add.sub.portfolio(mult.portf, p.long, rp=rp.long,
#                                 optimize_method="random",
#                                 rebalance_on="quarters",
#                                 training_period=136)
#
# #' Add portf2 as a sub portfolio with optimization parameters specific to
# #' running optimize.portfolio.rebalancing with portf2.
# mult.portf <- add.sub.portfolio(mult.portf, p.short, rp=rp.short,
#                                 optimize_method="random",
#                                 rebalance_on="quarters",
#                                 training_period=136)
#
# #' Generate random portfolios for the top layer optimization
# set.seed(123)
# rp.top <- random_portfolios(portf, 5000)
#
# #' Run the multi layer optimization
# opt.mult <- optimize.portfolio(R, mult.portf,
#                                optimize_method="random",
#                                trace=TRUE, rp=rp.top)
# opt.mult
#
# z <- PortfolioAnalytics:::proxy.mult.portfolio(R, mult.portf)
#
# opt.r.mult <- optimize.portfolio.rebalancing(R, mult.portf$top.portfolio,
#                                              optimize_method="random",
#                                              rebalance_on="quarters",
#                                              training_period=36,
#                                              rp=rp.top, trace=TRUE)


