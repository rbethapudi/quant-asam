## ---------------------------------------------
#  Percent Operating Accruals + Generalized Accruals Indices 
#  Author: Jonathan Liang (jonathan.liang.2016@anderson.ucla.edu)
## ---------------------------------------------

# Req'd libraries
require(data.table)
require(ggplot2)
require(hash)
require(reshape2)

#  Some global vars to control I/O
base.dir        <- "/Users/liangjh/Workspaces/accruals_factor/"
base.dir.input  <- paste0(base.dir, "input/")
base.dir.output <- paste0(base.dir, "output/")
base.dir.debug  <- paste0(base.dir, "output/")


#  Selective market cap quantile choices
qtile.market_cap.selection <- c(3,5,7)

#
#  -- CUMULATIVE RETURNS -- 
#    Assuming a starting & ending year, see effect of investment over multiple years 


#  Load Monthly Returns 
returns.monthly.dt <- fread(paste0(base.dir.output, "data-returns_monthly.csv"))


#  Function: calculate cumulative, for each market cap quantile, starting & ending @ specified investment period
cumulativeReturns <- function(mdt, startYear = 2001, endYear = 2008) {
  startMonth <- startYear * 100
  endMonth <- endYear * 100
  returns.monthly.cumulative.period.dt <- data.table()
  for (curr_qtile_market_cap in sort(unique(mdt$qtile_market_cap))) {
    print(paste("returns: cumulative returns for market cap quantile:", curr_qtile_market_cap))
    curr.dt <- mdt[qtile_market_cap == curr_qtile_market_cap & month > startMonth & month < endMonth][order(month),
                                                                                                      .(month,  qtile_market_cap, ret_cum_p1 = cumprod(ret_p1),
                                                                                                        ret_sp500_capwgt_cum_p1 = cumprod(ret_sp500_capwgt_p1),
                                                                                                        ret_sp500_equwgt_cum_p1 = cumprod(ret_sp500_equwgt_p1))]
    curr.dt[, `:=`(ret_cum = ret_cum_p1 - 1.0,  ret_sp500_capwgt_cum = ret_sp500_capwgt_cum_p1 - 1.0,  ret_sp500_equwgt_cum = ret_sp500_equwgt_cum_p1 - 1.0)]
    returns.monthly.cumulative.period.dt <- rbind(returns.monthly.cumulative.period.dt, curr.dt)
  }
  return(returns.monthly.cumulative.period.dt)
}

#  Plot cumulative returns
plotReturns <- function(mdt, plotTitle, plotFileName = "/plot-returns.png", marketCapQuantiles = c(2,4,6,8)) {
  returns.monthly.cumulative.period.long.dt <- melt(mdt[qtile_market_cap %in% marketCapQuantiles,
                                                        .(month, qtile_market_cap, ret_cum, ret_sp500_capwgt_cum, ret_sp500_equwgt_cum)],
                                                    id = c("month", "qtile_market_cap"))
  returns.monthly.cumulative.period.long.dt[, datemonth := as.Date(as.character(month * 100 + 1), "%Y%m%d")]
  qplot(x = datemonth, y = value, data = returns.monthly.cumulative.period.long.dt, color = variable, facets = qtile_market_cap~.) +
    geom_line() + labs(title = plotTitle)
  ggsave(file = plotFileName)
}


#  Investment period: 2001..2008
returns.monthly.cumulative.2001_2008.dt <- cumulativeReturns(returns.monthly.dt, 2001, 2008)
plotReturns(returns.monthly.cumulative.2001_2008.dt, "Cumulative Returns (2001..2008) for Market Cap Quantiles (vs. benchmarks) \n",
            paste0(base.dir.output, "plot-returns_cumulative_2001_2008.png"), marketCapQuantiles = qtile.market_cap.selection)
write.csv(x = returns.monthly.cumulative.2001_2008.dt, file = paste0(base.dir.output, "data-returns_cumulative_2001_2008.csv"))

#  Investment period: 2009..present
returns.monthly.cumulative.2009_2015.dt <- cumulativeReturns(returns.monthly.dt, 2009, 2015)
plotReturns(returns.monthly.cumulative.2009_2015.dt, "Cumulative Returns (2009..2015) for Market Cap Quantiles (vs. benchmarks) \n",
            paste0(base.dir.output, "plot-returns_cumulative_2009_2015.png"), marketCapQuantiles = qtile.market_cap.selection)
write.csv(x = returns.monthly.cumulative.2009_2015.dt, file = paste0(base.dir.output, "data-returns_cumulative_2009_2015.csv"))




