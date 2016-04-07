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
base.dir        <- "/Users/liangjh/Workspace/quant-asam/accruals_factor/"
base.dir.input  <- paste0(base.dir, "input/")
base.dir.output <- paste0(base.dir, "output/")
base.dir.debug  <- paste0(base.dir, "output/")


#  Selective market cap quantile choices
qtile.market_cap.selection <- c(3,5,7)

# ------------------------------------------------------------------------------------------------
#  --- RETURNS ---
# ------------------------------------------------------------------------------------------------

#  Load Allocated Positions
positions.dt <- fread(paste0(base.dir.output, "data-portfolio_positions.csv"))


#  -- HISTORICAL RETURNS DATA --

#  Load historical returns data
returns.dt <- fread(paste0(base.dir.input, "input_returns.csv"))
returns.dt[, iyear := as.integer(date / 10000)]
setnames(returns.dt, "TICKER", "ticker")  # rename columns for merge ease

#  Create new returns set i.e merged data
#  Merge w/ portfolio allocations and therefore eliminate any extraneous tickers (from both sides)
returns.set.dt <- merge(returns.dt, positions.dt, by = c("iyear", "PERMNO"))

#  Which tickers were eliminated?
#  Log any missing tickers (i.e. present in positions file but not in returns file)
missing.pos.tickers <- setdiff(unique(positions.dt$PERMNO), unique(returns.set.dt$PERMNO))
missing.tickers <- merge(data.frame(PERMNO = missing.pos.tickers), positions.dt, by = c("PERMNO"))
write.csv(x = missing.tickers, file = paste0(base.dir.debug, "debug-missing_tickers_in_rets.csv"))

#  Returns set data normalization, prep for monthly calculations
returns.set.dt[, `:=`(month = as.integer(date / 100), RET = as.double(RET))]
returns.monthly.dt <- returns.set.dt[, list(ret_mean = unlist(lapply(.SD, mean, na.rm = TRUE)), 
                                            ret_sd   = unlist(lapply(.SD, sd, na.rm = TRUE)), 
                                            ret_ct   = .N), 
                                     by = c("month", "iyear", "qtile_market_cap"), 
                                     .SDcols = c("RET")]

write.csv(x = returns.set.dt, file = paste0(base.dir.output, "data-returns_set.csv"))


#  -- BENCHMARK (S&P 500) --

#  Load S&P 500 returns information (i.e. benchmark)
#  rename columns to be clear: i.e. cap-weighted vs. equally-weighted
sp500.ret.dt <- fread(paste0(base.dir.input, "input_sp500_index_returns.csv"))
sp500.ret.dt[, `:=`(month = as.integer(caldt /100))]
setnames(sp500.ret.dt, "vwretd", "ret_sp500_capwgt")
setnames(sp500.ret.dt, "ewretd", "ret_sp500_equwgt")

#  Merge returns w/ benchmark
returns.monthly.dt <- merge(returns.monthly.dt, sp500.ret.dt, by = c("month"))



#
#  -- RETURNS CALCULATIONS --
#

#
#  -- PERIODIC RETURNS --
#     (by market cap quantiles)
#     (for brevity, chose market cap quintiles)

#  Prep for returns calculations
returns.monthly.dt[, `:=`(ret_p1 = ret_mean + 1.0,
                          ret_sp500_capwgt_p1 = ret_sp500_capwgt + 1.0,
                          ret_sp500_equwgt_p1 = ret_sp500_equwgt + 1.0)]

#  -- MONTHLY --
returns.monthly.long.dt <- melt(returns.monthly.dt[qtile_market_cap %in% qtile.market_cap.selection & iyear > 2001,
                                                   .(month, iyear, qtile_market_cap, ret_mean, ret_sp500_capwgt, ret_sp500_equwgt)], 
                                id = c("month", "iyear", "qtile_market_cap"))
returns.monthly.long.dt[, dtmonth := as.Date(as.character(month * 100 + 1), "%Y%m%d")]
qplot(x = dtmonth, y = value, data = returns.monthly.long.dt, color = variable, facets = qtile_market_cap~.) + 
  geom_line() + labs(title = "Monthly Return by Market Cap Quantile (vs Benchmarks) \n")
ggsave(file = paste0(base.dir.output, "plot-monthly_returns.png"))
write.csv(x = returns.monthly.dt, file = paste0(base.dir.output, "data-returns_monthly.csv"))

#  -- ANNUAL --
returns.annual.dt <- returns.monthly.dt[, .(ret = prod(ret_p1) - 1.0,
                                            ret_sp500_capwgt = prod(ret_sp500_capwgt_p1) - 1.0,
                                            ret_sp500_equwgt = prod(ret_sp500_equwgt_p1) - 1.0,
                                            count = .N), 
                                        by = .(iyear, qtile_market_cap)]
returns.annual.long.dt <- melt(returns.annual.dt[qtile_market_cap %in% qtile.market_cap.selection & iyear > 1990,
                                                  .(iyear, qtile_market_cap, ret, ret_sp500_capwgt, ret_sp500_equwgt)], 
                                id = c("iyear", "qtile_market_cap"))
qplot(x = iyear, y = value, data = returns.annual.long.dt, color = variable, facets = qtile_market_cap~.) + 
    geom_line() +  labs(title = "Annual Return by Market Cap Quantiles (vs Benchmarks) \n")
ggsave(file = paste0(base.dir.output, "plot-annual_returns.png"))
write.csv(x = returns.monthly.dt, file = paste0(base.dir.output, "data-returns_annual.csv"))




