library(PortfolioAnalytics)

source("data_prep.R")
R <- edhec[,1:5]
# mix of blue, green, and red hues
my_colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")

##### RP Demo #####

portf.lo <- portfolio.spec(colnames(R))
portf.lo <- add.constraint(portf.lo, type="weight_sum",
                           min_sum=0.99, max_sum=1.01)
portf.lo <- add.constraint(portf.lo, type="long_only")
# Generate random portfolios using the 3 methods
rp1 <- random_portfolios(portf.lo, permutations=2000, rp_method='sample')
rp2 <- random_portfolios(portf.lo, permutations=2000, rp_method='simplex')
rp3 <- random_portfolios(portf.lo, permutations=2000, rp_method='grid')
# Calculate the portfolio mean return and standard deviation
rp1_mean <- apply(rp1, 1, function(x) mean(R %*% x))
rp1_StdDev <- apply(rp1, 1, function(x) StdDev(R, weights=x))
rp2_mean <- apply(rp2, 1, function(x) mean(R %*% x))
rp2_StdDev <- apply(rp2, 1, function(x) StdDev(R, weights=x))
rp3_mean <- apply(rp3, 1, function(x) mean(R %*% x))
rp3_StdDev <- apply(rp3, 1, function(x) StdDev(R, weights=x))
x.assets <- StdDev(R)
y.assets <- colMeans(R)
###
require(rCharts)
# create an interactive plot using rCharts and nvd3 scatterChart
tmp1 <- data.frame(name="sample", mean=rp1_mean, sd=rp1_StdDev)
tmp2 <- data.frame(name="simplex", mean=rp2_mean, sd=rp2_StdDev)
tmp3 <- data.frame(name="grid", mean=rp3_mean, sd=rp3_StdDev)
tmp <- rbind(tmp1, tmp2, tmp3)
rp_viz <- nPlot(mean ~ sd, group="name", data=tmp, type="scatterChart")
rp_viz$xAxis(
  axisLabel = 'Risk (std. dev.)'
  ,tickFormat = "#!d3.format('0.4f')!#"
)
rp_viz$yAxis(
  axisLabel = 'Return'
  ,tickFormat = "#!d3.format('0.4f')!#"
)
rp_viz$chart(color = my_colors[c(2,4,6)])
#set left margin so y axis label will show up
rp_viz$chart( margin = list(left = 100) )
# rp_viz$chart(
# tooltipContent = "#!
# function(a,b,c,d) {
# //d has all the info you need
# return( '<h3>' + d.point.series + '</h3>Return: ' + d.point.y + '<br>Risk: ' + d.point.x)
# }
# !#")
####if you do not want fisheye/magnify
####let me know, and will show how to remove
####this will solve the tooltip problem
save(rp_viz, file="figures/rp_viz.rda")
###
x.lower <- min(x.assets) * 0.9
x.upper <- max(x.assets) * 1.1
y.lower <- min(y.assets) * 0.9
y.upper <- max(y.assets) * 1.1
png("figures/rp_plot.png", height = 500, width = 1000)
# plot feasible portfolios
plot(x=rp1_StdDev, y=rp1_mean, col=my_colors[2], main="Random Portfolio Methods",
     ylab="mean", xlab="StdDev", xlim=c(x.lower, x.upper),
     ylim=c(y.lower, y.upper))
points(x=rp2_StdDev, y=rp2_mean, col=my_colors[4], pch=2)
points(x=rp3_StdDev, y=rp3_mean, col=my_colors[6], pch=5)
points(x=x.assets, y=y.assets)
text(x=x.assets, y=y.assets, labels=colnames(R), pos=4, cex=0.8)
legend("bottomright", legend=c("sample", "simplex", "grid"),
       col=my_colors[c(2,4,6)],
       pch=c(1, 2, 5), bty="n")
dev.off()


png("figures/fev_plot.png", height = 500, width = 1000)
fev <- 0:5
x.assets <- StdDev(R)
y.assets <- colMeans(R)
par(mfrow=c(2, 3))
for(i in 1:length(fev)){
  rp <- rp_simplex(portfolio=portf.lo, permutations=2000, fev=fev[i])
  tmp.mean <- apply(rp, 1, function(x) mean(R %*% x))
  tmp.StdDev <- apply(rp, 1, function(x) StdDev(R=R, weights=x))
  x.lower <- min(c(tmp.StdDev, x.assets)) * 0.85
  x.upper <- max(c(tmp.StdDev, x.assets)) * 1.15
  y.lower <- min(c(tmp.mean, y.assets)) * 0.85
  y.upper <- max(c(tmp.mean, y.assets)) * 1.15
  plot(x=tmp.StdDev, y=tmp.mean, main=paste("FEV =", fev[i]),
       ylab="mean", xlab="StdDev", col=rgb(0, 0, 100, 50, maxColorValue=255),
       xlim=c(x.lower, x.upper),
       ylim=c(y.lower, y.upper))
  points(x=x.assets, y=y.assets)
  text(x=x.assets, y=y.assets, labels=colnames(R), pos=4, cex=0.8)
}
par(mfrow=c(1,1))
dev.off()
