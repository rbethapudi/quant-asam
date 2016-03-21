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

# --------------------------------------------------------------------
# --- RISK ANALYTICS ---
# --------------------------------------------------------------------


#  Load data from prior analyses
returns.monthly.dt <- fread(paste0(base.dir.output, "data-returns_monthly.csv"))
returns.set.dt <- fread(paste0(base.dir.output, "data-returns_set.csv"))


#
#  -- 1 YR US TREASURY RATES --
#   Will be used for risk calculations 
#

ust.1yr.monthly.dt <- fread(paste0(base.dir.input, "input_1yr_treasury_monthly.csv"))
ust.1yr.monthly.dt[, `:=`(month = as.integer(date /100), iyear = as.integer(date / 10000))]
ust.1yr.monthly.dt[, rate_monthly := rate / 12.0]  # convert to monthly rate

ust.1yr.annual.dt <- fread(paste0(base.dir.input, "input_1yr_treasury_annual.csv"))
ust.1yr.annual.dt[, iyear := as.integer(date/10000)]

#  -- Fama-French Factors (monthly) --
ff.benchmarks.dt <- fread(paste0(base.dir.input, "F-F_Research_Data_Factors_Monthly.csv"))
ff.benchmarks.dt[, `:=`(excess_return_mkt = excess_return_mkt / 100.0, smb = smb / 100.0, hml = hml / 100.0, rf = rf / 100.0)]   #  normalize to 0..1 scale

#
#  SHARPE RATIOS & EXCESS RETURNS
#  Risk Profiles  (strat. by market cap quantile)
#

#  Plots - Risk / Return, by market cap quantiles
qplot(x = ret_sd, y = ret_mean, data = returns.monthly.dt[qtile_market_cap %in% qtile.market_cap.selection], facets = qtile_market_cap ~ .) +
  geom_point() + stat_smooth(method = "lm") + labs(title = "Returns~Volatility for Market Cap Quantiles \n")
ggsave(file = paste0(base.dir.output, "plot-returns_fn_vol.png"))

qplot(x = ret_mean, y = ret_sd, data = returns.monthly.dt[qtile_market_cap %in% qtile.market_cap.selection], facets = qtile_market_cap ~ .) +
  geom_point() + stat_smooth(method = "lm") + labs(title = "Volatility~Returns for Market Cap Quantiles \n")
ggsave(file = paste0(base.dir.output, "plot-vol_fn_returns.png"))

#  Monthly Excess Returns / Risk
returns.stats.by.marketcap.dt <- returns.set.dt[, .(ret_mean = mean(RET, na.rm = TRUE), ret_sd = sd(RET, na.rm = TRUE), ret_ct = .N), by = .(month, qtile_market_cap)]
returns.stats.by.marketcap.dt <- merge(returns.stats.by.marketcap.dt, ust.1yr.monthly.dt, by = c("month"))
returns.stats.by.marketcap.dt[, excess_return := ret_mean - rate_monthly]
returns.stats.by.marketcap.dt[, sharpe_ratio := excess_return / ret_sd]
write.csv(x = returns.stats.by.marketcap.dt, file = paste0(base.dir.debug, "data-risk_stats_monthly_by_marketcap.csv"))

#  Plot monthly histograms, for selected segments
ggplot(returns.stats.by.marketcap.dt[qtile_market_cap %in% qtile.market_cap.selection],
       aes(x = sharpe_ratio), facets = qtile_market_cap~.) + geom_histogram() +
    facet_wrap(~qtile_market_cap, ncol = 1) + labs(title = "Sharpe Ratio Distribution by Market Cap Quantile \n")
ggsave(file = paste0(base.dir.output, "plot-sharpe_ratio_by_market_cap_quantile.png"))


#
# -- RISK FACTORS DATA (merged) --
#

# Merge data w/ FF factors
returns.stats.with.ff.dt <- merge(returns.stats.by.marketcap.dt, ff.benchmarks.dt, by = c("month"))

# Re-calculate excess returns using risk-free rate given by fama-french data - this is the 1-month t-bill rate
returns.stats.with.ff.dt[, excess_return := ret_mean - rf]


#
#  -- CAPM RISK FACTORS --
#
capmRisk <- function(dataset, quantiles, alabel) {
  factors.capm.dt <- data.table(analysis_label = character(0), market_cap_quantile = character(0), obs = numeric(0), alpha = numeric(0), rmrf = numeric(0))
  for (q in quantiles) {
    print(paste("CAPM factors for", alabel, ",quantile:", q))
    data.dt <- dataset[qtile_market_cap == q]
    regr.capm <- lm(excess_return ~ excess_return_mkt, data = data.dt)
    factors.capm.dt <- rbind(factors.capm.dt, data.table(analysis_label = alabel, market_cap_quantile = q, obs = nrow(data.dt),
                                                     alpha = coefficients(regr.capm)[["(Intercept)"]], 
                                                     rmrf = coefficients(regr.capm)[["excess_return_mkt"]]))
  }
  factors.capm.dt[, alpha_annualized := alpha * 12.0]
  return(factors.capm.dt)
}

#  Calculate for all and various holding periods, see if there is a difference based on our holding periods
factors.capm.overall.dt <- capmRisk(returns.stats.with.ff.dt, 1:10, "overall")
factors.capm.2001_2008.dt <- capmRisk(returns.stats.with.ff.dt[iyear >= 2001 & iyear <= 2008], 1:10, "2001..2008")
factors.capm.2009_2015.dt <- capmRisk(returns.stats.with.ff.dt[iyear >= 2000 & iyear <= 2015], 1:10, "2009..2015")
factors.capm.dt <- rbind(factors.capm.overall.dt, factors.capm.2001_2008.dt, factors.capm.2009_2015.dt)
write.csv(x = factors.capm.dt, file = paste0(base.dir.output, "data-risk_capm_coeffs.csv"))

regr.capm.full <- lm(excess_return ~ excess_return_mkt, data = returns.stats.with.ff.dt)
summary(regr.capm.full)

#
#  -- FAMA-FRENCH RISK FACTORS --
#

ffRisk <- function(dataset, quantiles, alabel) {
  factors.dt <- data.table(analysis_label = character(0), market_cap_quantile = character(0), obs = numeric(0),
                              alpha = numeric(0), rmrf = numeric(0), smb = numeric(0), hml = numeric(0), 
                              alpha_pvalue = numeric(0))
  for (q in quantiles) {
    print(paste("FF risk factors for", alabel, ", quantile:", q))
    data.dt <- dataset[qtile_market_cap == q]
    regr.ff <- lm(excess_return ~ excess_return_mkt + hml + smb, data = dataset[qtile_market_cap == q])
    coeffs <- coefficients(regr.ff)
    factors.dt <- rbind(factors.dt,
                        data.table(analysis_label = alabel, 
                                   market_cap_quantile = q, 
                                   obs = nrow(data.dt),
                                   alpha = coeffs[["(Intercept)"]], 
                                   rmrf = coeffs[["excess_return_mkt"]],
                                   smb = coeffs[["smb"]],
                                   hml = coeffs[["hml"]],
                                   alpha_pvalue = summary(regr.ff)$coefficients[1,4]))
  }
  factors.dt[, alpha_annualized := alpha * 12.0]
  return(factors.dt)
}

#  Calculate for all and various holding periods, see if there is a difference based on our holding periods
factors.ff.overall.dt <- ffRisk(returns.stats.with.ff.dt, 1:10, "overall")
factors.ff.2001_2008.dt <- ffRisk(returns.stats.with.ff.dt[iyear >= 2001 & iyear <= 2008], 1:10, "2001..2008")
factors.ff.2009_2015.dt <- ffRisk(returns.stats.with.ff.dt[iyear >= 2000 & iyear <= 2015], 1:10, "2009..2015")
factors.ff.dt <- rbind(factors.ff.overall.dt, factors.ff.2001_2008.dt, factors.ff.2009_2015.dt)
write.csv(x = factors.ff.dt, file = paste0(base.dir.output, "data-risk_fama_french_coeffs.csv"))

regr.ff.full <- lm(excess_return ~ excess_return_mkt + hml + smb, data = returns.stats.with.ff.dt)
summary(regr.ff.full)

