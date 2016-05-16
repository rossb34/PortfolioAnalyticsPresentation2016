library(PortfolioAnalytics)

# common resolution for projectors is 1024 × 768
w <- 1024 * 0.8
h <- 768 * 0.6

# really simple helper function to find the max sharpe ratio portfolio
# given the output from extract stats
find.max.sr <- function(x){
  x[which.max(x[,"mean"] / x[,"StdDev"]),]
}

source("data_prep.R")

# in sample
R <- edhec["/2014",1:8]
# out of sample
R.os <- edhec["/2015",1:8]
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

# define base portfolio
portf.base <- portfolio.spec(colnames(R))
# the only thing I will be changing is constraints so define all the objectives
# set multiplier of 0 so the values are calculated, but no optimization is done
portf.base <- add.objective(portf.base, type="return", name="mean", multiplier=0)
portf.base <- add.objective(portf.base, type="risk", name="StdDev", multiplier=0)
portf.base <- add.objective(portf.base, type="risk", name="ES", arguments=list(p=0.9), multiplier=0)

##### Example 0: Basic Efficient Frontier #####
# full investment and long only constraint
p0 <- portf.base
p0 <- add.constraint(p0, type="weight_sum",
                     min_sum=0.99, max_sum=1.01)
p0 <- add.constraint(p0, type="box", min=0, max=1)

# create an efficient frontier that only shows the hull and assets
ef <- create.EfficientFrontier(R, portfolio = p0,
                               type = "mean-StdDev", n.portfolios = 100)

png("figures/basic_mv_ef.png", width = w, height = h, units = "px")
chart.EfficientFrontier(ef, match.col="StdDev", pch=18, col="lightblue")
dev.off()


##### Example 1: Full Investment, Long Only #####
p1 <- portf.base
p1 <- add.constraint(p1, type="weight_sum",
                     min_sum=0.99, max_sum=1.01)
p1 <- add.constraint(p1, type="box", min=0, max=1)

# generate random portfolios for the baseline portfolio
rp1 <- random_portfolios(p1, permutations=5000, rp_method='sample')

opt1 <- optimize.portfolio(R, p1, optimize_method="random", rp=rp1, trace=TRUE)
xt1 <- extractStats(opt1)
p1.mean <- xt1[,"mean"]
p1.sd <- xt1[,"StdDev"]
p1.es <- xt1[,"ES"]
opt.xt1 <- find.max.sr(xt1)

png("figures/p1_ef.png", width = w, height = h, units = "px")
# plot the feasible space
plot(x=x.assets, y=y.assets, type="n", main="Feasible Space",
     xlim=c(x.lower, x.upper), ylim=c(y.lower, y.upper),
     ylab="mean", xlab="StdDev", cex.axis=0.8)
# baseline portfolio feasible space
points(x=p1.sd, y=p1.mean, col=my_colors[2], pch=1)
# assets
points(x=x.assets, y=y.assets, col="black", pch=19)
text(x=x.assets, y=y.assets, labels=colnames(R), pos=4, cex=0.8)
# max sharpe ratio portfolio
points(x=opt.xt1["StdDev"], y=opt.xt1["mean"], col="orange", pch=19)
text(x=opt.xt1["StdDev"], y=opt.xt1["mean"], pos=2, cex=0.8,
     labels=paste("Max SR =",round(opt.xt1["mean"]/opt.xt1["StdDev"],4)), col="black")
dev.off()

##### Example 2: Full Investment, Long Only Box Constraints #####
p2 <- portf.base
p2 <- add.constraint(p2, type="weight_sum",
                     min_sum=0.99, max_sum=1.01)
p2 <- add.constraint(p2, type="box", min=0.1, max=0.85)

# generate random portfolios for the baseline portfolio
rp2 <- random_portfolios(p2, permutations=5000, rp_method='sample')

opt2 <- optimize.portfolio(R, p2, optimize_method="random", rp=rp2, trace=TRUE)
xt2 <- extractStats(opt2)
p2.mean <- xt2[,"mean"]
p2.sd <- xt2[,"StdDev"]
p2.es <- xt2[,"ES"]
opt.xt2 <- find.max.sr(xt2)

png("figures/p2_ef.png", width = w, height = h, units = "px")
plot(x=x.assets, y=y.assets, type="n", main="Feasible Space",
     xlim=c(x.lower, x.upper), ylim=c(y.lower, y.upper),
     ylab="mean", xlab="StdDev", cex.axis=0.8)
# baseline portfolio feasible space
points(x=p1.sd, y=p1.mean, col=my_colors[2], pch=1)
points(x=p2.sd, y=p2.mean, col=my_colors[3], pch=1)
# assets
points(x=x.assets, y=y.assets, col="black", pch=19)
text(x=x.assets, y=y.assets, labels=colnames(R), pos=4, cex=0.8)
# max sharpe ratio portfolio
points(x=opt.xt2["StdDev"], y=opt.xt2["mean"], col="orange", pch=19)
text(x=opt.xt2["StdDev"], y=opt.xt2["mean"], pos=2, cex=0.8,
     labels=paste("Max SR =",round(opt.xt2["mean"]/opt.xt2["StdDev"],4)), col="black")
legend("topleft", legend = c("Portfolio 1", "Portfolio 2"), bty="n",
       pch = c(1, 1), col = c(my_colors[2], my_colors[3]))
dev.off()

##### Example 3: Full Investment, Long Only and Group Constraints #####
p3 <- portf.base
p3 <- add.constraint(p3, type="weight_sum",
                     min_sum=0.99, max_sum=1.01)
p3 <- add.constraint(p3, type="box", min=0, max=1)
p3 <- add.constraint(p3, type="group", groups=list(c(2, 4, 8), c(1, 5, 7), c(3, 6)),
                        group_min=c(0.1, 0.25, 0.1), group_max=c(0.65, 0.45, 0.55),
                        group_labels=c("RV", "Dir", "Other"))

# generate random portfolios for the baseline portfolio
rp3 <- random_portfolios(p3, permutations=5000, rp_method='sample')

opt3 <- optimize.portfolio(R, p3, optimize_method="random", rp=rp3, trace=TRUE)
xt3 <- extractStats(opt3)
p3.mean <- xt3[,"mean"]
p3.sd <- xt3[,"StdDev"]
p3.es <- xt3[,"ES"]
opt.xt3 <- find.max.sr(xt3)

png("figures/p3_ef.png", width = w, height = h, units = "px")
plot(x=x.assets, y=y.assets, type="n", main="Feasible Space",
     xlim=c(x.lower, x.upper), ylim=c(y.lower, y.upper),
     ylab="mean", xlab="StdDev", cex.axis=0.8)
# baseline portfolio feasible space
points(x=p1.sd, y=p1.mean, col=my_colors[2], pch=1)
points(x=p3.sd, y=p3.mean, col=my_colors[3], pch=1)
# assets
points(x=x.assets, y=y.assets, col="black", pch=19)
text(x=x.assets, y=y.assets, labels=colnames(R), pos=4, cex=0.8)
# max sharpe ratio portfolio
points(x=opt.xt3["StdDev"], y=opt.xt3["mean"], col="orange", pch=19)
text(x=opt.xt3["StdDev"], y=opt.xt3["mean"], pos=2, cex=0.8,
     labels=paste("Max SR =",round(opt.xt3["mean"]/opt.xt3["StdDev"],4)), col="black")
legend("topleft", legend = c("Portfolio 1", "Portfolio 3"), bty="n",
       pch = c(1, 1), col = c(my_colors[2], my_colors[3]))
dev.off()

##### Example 4: Full Investment, Long Only Box and Position Limit Constraints #####
p4 <- portf.base
p4 <- add.constraint(p4, type="weight_sum",
                     min_sum=0.99, max_sum=1.01)
p4 <- add.constraint(p4, type="box", min=0, max=1)
p4 <- add.constraint(p4, type="position_limit", max_pos=2)

# generate random portfolios for the baseline portfolio
rp4 <- random_portfolios(p4, permutations=5000, rp_method='sample')

opt4 <- optimize.portfolio(R, p4, optimize_method="random", rp=rp4, trace=TRUE)
xt4 <- extractStats(opt4)
p4.mean <- xt4[,"mean"]
p4.sd <- xt4[,"StdDev"]
p4.es <- xt4[,"ES"]
opt.xt4 <- find.max.sr(xt4)

png("figures/p4_ef.png", width = w, height = h, units = "px")
plot(x=x.assets, y=y.assets, type="n", main="Feasible Space",
     xlim=c(x.lower, x.upper), ylim=c(y.lower, y.upper),
     ylab="mean", xlab="StdDev", cex.axis=0.8)
# baseline portfolio feasible space
points(x=p1.sd, y=p1.mean, col=my_colors[2], pch=1)
points(x=p4.sd, y=p4.mean, col=my_colors[3], pch=1)
# assets
points(x=x.assets, y=y.assets, col="black", pch=19)
text(x=x.assets, y=y.assets, labels=colnames(R), pos=4, cex=0.8)
# max sharpe ratio portfolio
points(x=opt.xt4["StdDev"], y=opt.xt4["mean"], col="orange", pch=19)
text(x=opt.xt4["StdDev"], y=opt.xt4["mean"], pos=2, cex=0.8,
     labels=paste("Max SR =",round(opt.xt4["mean"]/opt.xt4["StdDev"],4)), col="black")
legend("topleft", legend = c("Portfolio 1", "Portfolio 4"), bty="n",
       pch = c(1, 1), col = c(my_colors[2], my_colors[3]))
dev.off()

##### Example 5: Full Investment, Allow Shorts with Box and Leverage Constraints #####
p5 <- portf.base
p5 <- add.constraint(p5, type="weight_sum", 
                     min_sum=0.99, max_sum=1.01)
p5 <- add.constraint(p5, type="box", min=-0.15, max=0.6)
p5 <- add.constraint(p5, type="leverage_exposure", leverage=1.6)

# generate random portfolios for the baseline portfolio
rp5 <- random_portfolios(p5, permutations=5000, rp_method='sample')

opt5 <- optimize.portfolio(R, p5, optimize_method="random", rp=rp5, trace=TRUE)
xt5 <- extractStats(opt5)
p5.mean <- xt5[,"mean"]
p5.sd <- xt5[,"StdDev"]
p5.es <- xt5[,"ES"]
opt.xt5 <- find.max.sr(xt5)

png("figures/p5_ef.png", width = w, height = h, units = "px")
plot(x=x.assets, y=y.assets, type="n", main="Feasible Space",
     xlim=c(x.lower, x.upper), ylim=c(y.lower, y.upper),
     ylab="mean", xlab="StdDev", cex.axis=0.8)
# baseline portfolio feasible space
points(x=p1.sd, y=p1.mean, col=my_colors[2], pch=1)
points(x=p5.sd, y=p5.mean, col=my_colors[3], pch=1)
# assets
points(x=x.assets, y=y.assets, col="black", pch=19)
text(x=x.assets, y=y.assets, labels=colnames(R), pos=4, cex=0.8)
# max sharpe ratio portfolio
points(x=opt.xt5["StdDev"], y=opt.xt5["mean"], col="orange", pch=19)
text(x=opt.xt5["StdDev"], y=opt.xt5["mean"], pos=2, cex=0.8,
     labels=paste("Max SR =",round(opt.xt5["mean"]/opt.xt5["StdDev"],4)), col="black")
legend("topleft", legend = c("Portfolio 1", "Portfolio 5"), bty="n",
       pch = c(1, 1), col = c(my_colors[2], my_colors[3]))
dev.off()

##### Out of Sample #####
# TODO if I have time




##### scratch #####
# plot the feasible space of the baseline portfolio
# plot(x=x.assets, y=y.assets, type="n", main="Feasible Space",
#      xlim=c(x.lower, x.upper), ylim=c(y.lower, y.upper),
#      ylab="mean", xlab="StdDev", cex.axis=0.8)
# # baseline portfolio feasible space
# points(x=rp.base.sd, y=rp.base.mean, col=my_colors[2], pch=1)
# # assets
# points(x=x.assets, y=y.assets, col="black", pch=19)
# text(x=x.assets, y=y.assets, labels=colnames(R), pos=4, cex=0.8)
#
# chart.xt <- function(R, xt, risk=c("StdDev", "ES"), ...){
#   # compute risk and return metrics of asset returns
#   x.sd.assets <- StdDev(R)
#   y.assets <- colMeans(R)
#   
#   # set up chart ranges
#   x.lower <- min(0, min(x.assets) * 0.9)
#   x.upper <- max(x.assets) * 1.1
#   y.lower <- min(0, min(y.assets) * 0.9)
#   y.upper <- max(y.assets) * 1.1
# }
