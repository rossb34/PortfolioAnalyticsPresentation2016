source("data_prep.R")

library(PortfolioAnalytics)
data(edhec)
R <- edhec[,1:8]
#R <- edhec
my_colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")

# compute risk and return metrics of asset returns
x.assets <- StdDev(R)
y.assets <- colMeans(R)

# set up chart ranges
x.lower <- min(0, min(x.assets) * 0.9)
x.upper <- max(x.assets) * 1.1
y.lower <- min(0, min(y.assets) * 0.9)
y.upper <- max(y.assets) * 1.1

# define a baseline portfolio with full investment and long only constraint
portf.baseline <- portfolio.spec(colnames(R))
portf.baseline <- add.constraint(portf.baseline, type="weight_sum",
                           min_sum=0.99, max_sum=1.01)
portf.baseline <- add.constraint(portf.baseline, type="box", min=0, max=1)

# create an efficient frontier that only shows the hull and assets
ef <- create.EfficientFrontier(R, portfolio = portf.baseline,
                               type = "mean-StdDev", n.portfolios = 100)
chart.EfficientFrontier(ef, match.col="StdDev", pch=18, col="lightblue")

# generate random portfolios for the baseline portfolio
rp.base <- random_portfolios(portf.baseline, permutations=5000,
                             rp_method='sample')
# compute mean and standard deviation of baseline portfolio
rp.base.mean <- apply(rp.base, 1, function(x) mean(R %*% x))
rp.base.sd <- apply(rp.base, 1, function(x) StdDev(R, weights=x))

# plot the feasible space of the baseline portfolio
plot(x=x.assets, y=y.assets, type="n", main="Feasible Space",
     xlim=c(x.lower, x.upper), ylim=c(y.lower, y.upper),
     ylab="mean", xlab="StdDev", cex.axis=0.8)
# baseline portfolio feasible space
points(x=rp.base.sd, y=rp.base.mean, col=my_colors[2], pch=1)
# assets
points(x=x.assets, y=y.assets, col="black", pch=19)
text(x=x.assets, y=y.assets, labels=colnames(R), pos=4, cex=0.8)

# simple case of box constraints
p1 <- portfolio.spec(colnames(R))
p1 <- add.constraint(p1, type="weight_sum", min_sum=0.99, max_sum=1.01)
p1 <- add.constraint(p1, type="box", min=0.05, max=0.6)
p1 <- add.objective(p1, type="return", name="mean")
p1 <- add.objective(p1, type="risk", name="StdDev")
rp1 <- random_portfolios(p1, permutations=5000, rp_method='sample')
opt1 <- optimize.portfolio(R, portfolio=p1, optimize_method="random",
                              rp=rp1, trace = TRUE)
xt1 <- extractStats(opt1)

# group constraints
p2 <- portfolio.spec(colnames(R))
p2 <- add.constraint(p2, type="weight_sum", min_sum=0.99, max_sum=1.01)
p2 <- add.constraint(p2, type="box", min=0.05, max=0.6)
p2 <- add.constraint(p2, type="group", groups=list(1:5, 5:8),
                        group_min=c(0.1, 0.15), group_max=c(0.85, 0.55),
                        group_labels=c("GroupA", "GroupB"))
p2 <- add.objective(p2, type="return", name="mean")
p2 <- add.objective(p2, type="risk", name="StdDev")
rp2 <- random_portfolios(p2, permutations=5000, rp_method='sample')
opt2 <- optimize.portfolio(R, portfolio=p2, optimize_method="random",
                           rp=rp2, trace = TRUE)
xt2 <- extractStats(opt2)

# box and position limit constraints
p3 <- portfolio.spec(colnames(R))
p3 <- add.constraint(p3, type="weight_sum", min_sum=0.99, max_sum=1.01)
p3 <- add.constraint(p3, type="box", min=0, max=1)
p3 <- add.constraint(p3, type="position_limit", max_pos=4)
p3 <- add.objective(p3, type="return", name="mean")
p3 <- add.objective(p3, type="risk", name="StdDev")
rp3 <- random_portfolios(p3, permutations=5000, rp_method='sample')
opt3 <- optimize.portfolio(R, portfolio=p3, optimize_method="random",
                           rp=rp3, trace = TRUE)
xt3 <- extractStats(opt3)
plot(x=xt3[,"StdDev"], xt3[,"mean"])




# chart.RiskReward(opt.new, risk.col = "StdDev",
#                  xlim=c(x.lower, x.upper), ylim=c(y.lower, y.upper))
# rp.new.mean <- apply(rp.new, 1, function(x) mean(R %*% x))
# rp.new.sd <- apply(rp.new, 1, function(x) StdDev(R, weights=x))

portf.new <- add.constraint(portf.new, type="position_limit", max_pos=3)
# portf.new <- add.constraint(portf.new,type="diversification", div_target=0.7)


# compute mean and standard deviation of new portfolio
rp.new <- random_portfolios(portf.new, permutations=5000, rp_method='sample')
rp.new.mean <- apply(rp.new, 1, function(x) mean(R %*% x))
rp.new.sd <- apply(rp.new, 1, function(x) StdDev(R, weights=x))
sr <- rp.new.mean / rp.new.sd
rp.new[which.max(sr),]
points(x=rp.new.sd[which.max(sr)], y=rp.new.mean[which.max(sr)], col="orange", pch=1)


plot(x=x.assets, y=y.assets, col="black", main="Random Portfolio Methods",
     xlim=c(x.lower, x.upper), ylim=c(y.lower, y.upper),
     ylab="mean", xlab="StdDev", pch=16)
points(x=rp.base.sd, y=rp.base.mean, col=my_colors[2], pch=1)
points(x=rp.new$sd, y=rp.new$mean, col=my_colors[4], pch=5)
legend("bottomright", legend=c("baseline portfolio", "new portfolio"),
       col=my_colors[c(2,4)],
       pch=c(1, 5), bty="n")

