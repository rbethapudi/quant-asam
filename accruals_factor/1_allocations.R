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


# ------------------------------------------------------------
#  ---- PORTFOLIO ALLOCATIONS -----
# ------------------------------------------------------------



#  -- FUNDAMENTALS DATA ---

#  Fundamentals data (from WRDS: CRSP/Compustat merge)
fundamentals.dt <- fread(paste0(base.dir.input, "input_fundamentals-a.csv"))

#  DATA CLEANUP
#  Reconcile compustat and crsp data idiosyncratic 
fundamentals.dt <- fundamentals.dt[fic == "USA"]  #  remove anything not in the USA
fundamentals.dt[, `:=`(PERMNO = LPERMNO, cusip = substring(cusip, 1, 8))]  # convert to compatible format
#  (skipped step (revisit) : there may be some duplicate observations (?) - unsure how to filter appropriately)
fundamentals.dt <- fundamentals.dt[gsector != 40]  #  Exclude financials, according to paper


#  MODELING / MEASURE CALCULATIONS -o
#  Custom Accrual Measures
fundamentals.dt[, `:=`(pct_operating_accruals = (ni - oancf) / abs(ni),
                       pct_total_accruals = ((ni - dv) + chech)  / abs(ni))]
fundamentals.dt[, market_cap := prcc_f * csho]  # (in millions)


#  DATA CLEANSING
#  Shortcut - exclude null values for calculated fields
#  TODO: revisit when when we get more data on which years are missing values (should not remove individual rows / line-items)
print("calculate: accruals metrics...")
fundamentals.dt <- fundamentals.dt[!is.na(pct_operating_accruals) & 
                                   !is.na(market_cap)]
#  Sanitize pct operating accruals - otherwise deciles will have infinite ranges
fundamentals.dt[pct_operating_accruals > 10,  pct_operating_accruals := 10]
fundamentals.dt[pct_operating_accruals < -10, pct_operating_accruals := -10]
#  Sanitize pct total accruals - otherwise deciles will have infinite ranges
fundamentals.dt[pct_total_accruals > 10,  pct_total_accruals := 10]
fundamentals.dt[pct_total_accruals < -10, pct_total_accruals := -10]


#
#  -- BUCKETING AND QUANTILING --
#

#  SIZE BUCKETING
#  NYSE Market Value Breakpoints (from Kenneth French)
#  Values in millions (http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_me_breakpoints.html)
#  Segment universe into size portfolios, according to paper
#  This is a fixed-width file, so the parsing requires more prescriptive formatting specifications
print("data: market cap quantiles (via NYSE quantiles)...")
mvbe.raw.dt <- as.data.table(read.fwf(paste0(base.dir.input, "ME_Breakpoints.txt"), 
                                    widths = c(c(6, 5), rep(c(10), times = 20)),
                                    col.names = c(c("yyyymm", "count"), paste("q", seq(20), sep="")), 
                                    stringsAsFactors = FALSE))

#  Restrict to EOY (December) market capitalizations
#     (note: we're assuming that fiscal YE is december for all firms)
mvbe.decile.dt <- mvbe.raw.dt[grepl("12$", mvbe.raw.dt$yyyymm, perl = TRUE)]
#  Condense quantile partitions of 20 into partitions of 10
mvbe.decile.dt[, `:=`(fyear = as.integer(yyyymm / 100),
                      d1 = q2, d2 = q4, d3 = q6, d4 = q8, d5 = q10, 
                      d6 = q12, d7 = q14, d8 = q16, d9 = q18, d10 = q20)] # assemble size deciles
mvbe.decile.dt <- mvbe.decile.dt[, c(c("yyyymm", "count"), paste("q", seq(20), sep="")) := NULL]  # kill extraneous cols

                          
#  MARKET CAPITALIZATION BUCKETS
#     For each year, divide universe into 10 market-cap quantiles
print("bucketing: market cap...")
for (year in sort(unique(fundamentals.dt$fyear))) {
  current.qtile.ranges <- as.numeric(mvbe.decile.dt[fyear == year])[-1]
  fundamentals.dt[fyear == year, qtile_market_cap := as.integer(cut(market_cap, current.qtile.ranges, include.lowest=TRUE, right = TRUE))]
  
  #  Outliers - bucket into extreme buckets (i.e. minimum or maximum)
  fundamentals.dt[fyear == year & is.na(qtile_market_cap) & market_cap <= current.qtile.ranges[1], qtile_market_cap := 1]
  fundamentals.dt[fyear == year & is.na(qtile_market_cap) & market_cap >= current.qtile.ranges[length(current.qtile.ranges)], qtile_market_cap := length(current.qtile.ranges)]
}

#  Debug - size quantile bucket summary
debug.marketcap.quantiles.summary.dt <- fundamentals.dt[, .(.N), by = .(fyear, qtile_market_cap)]
write.csv(x = debug.marketcap.quantiles.summary.dt, file = paste0(base.dir.output, "data-marketcap_quantiles_summary.csv"))


print("bucketing: by accrual metric, for fiscal year and market cap quantile...")
accr.quantile.by.mktcap.map <- hash()
for (year in sort(unique(fundamentals.dt$fyear))) {
  for (curr.qtile.marketcap in sort(unique(fundamentals.dt[fyear == year]$qtile_market_cap))) {
    #  TODO: for multi-factor, we'll need to calc a composited risk score first, then quantile it up
    all.qtile.accr  <- quantile(fundamentals.dt[fyear == year & qtile_market_cap == curr.qtile.marketcap, 
                                                "pct_operating_accruals", with = FALSE][[1]], 
                                                prob = seq(0, 1, 0.1), na.rm = TRUE)
    accr.quantile.by.mktcap.map[paste(year, curr.qtile.marketcap)] <- all.qtile.accr
    print(paste("quantiles for year:", year, ", marketcap quantile:", curr.qtile.marketcap))
    
    #  Bucket all rows from the measure quantiles
    tryCatch({fundamentals.dt[fyear == year & qtile_market_cap == curr.qtile.marketcap, 
                              qtile_measure := as.integer(cut(pct_operating_accruals, as.numeric(all.qtile.accr), 
                                                         include.lowest = TRUE, right = TRUE))]
    }, warning = function(war) { print("*warning* during bucketing.  skipping...") }, 
       error = function(err) { print("*error* during bucketing.  skipping...") } )
  }  
}

#  Debug: accrual quantiles, by marketcap buckets
debug.accr.quantile.by.mktcap <- t(as.data.table(as.list(accr.quantile.by.mktcap.map)))
write.csv(x = debug.accr.quantile.by.mktcap, file = paste0(base.dir.output, "data-accrual_quantiles_by_marketcap.csv"))

#  Given measured-based quantiles, invest in quantile 1 (deemed undervalued): potential portfolio positions
print("writing investment portfolio rebalancing positions...")
positions.dt <- fundamentals.dt[qtile_measure == 1][order(fyear, qtile_market_cap, qtile_measure)][, .(fyear, qtile_market_cap, qtile_measure, conml, tic, PERMNO, cusip, market_cap, pct_operating_accruals)]
positions.dt[, `:=`(ticker = tic, iyear = fyear + 1)]  #  [1] set ticker as column (not "tic");  [2] invest in stocks the year _after_ the fiscal year
write.csv(x = positions.dt, file = paste0(base.dir.output, "data-portfolio_positions.csv"))


#
#  -- PLOT DISTRIBUTIONS / QUANTILES --
#

# -----------------------------------------------------------------------------------------------------------------------
#    DEFUNCT FUNCTION CODE
#    eval/parse doesn't seem to work in the function's context - can't find parameterized variable
#    also can't be combined with facets, or even entire expression sets
plotQuantileDistributions <- function(fdt, plotColumn = "pct_operating_accruals", facetExpr = "fyear~.", 
                                      startYear, endYear, 
                                      plotTitle, plotFile = "/plot-quantiledist.png", 
                                      marketCapQuantiles = c(1,2,3,4,5,6,7,8,9)) {
  ggplot(fdt[fyear >= startYear & fyear < endYear],
           aes(x = eval(parse(text = plotColumn))), 
           facets = eval(parse(text = facetExpr))) +
              geom_histogram() +
              coord_cartesian(xlim = c(-10,10)) +
              #facet_wrap(~ eval(parse(text = facetColumn)), ncol = 3) +
              scale_x_discrete(breaks = -10:10) +
              labs(title = plotTitle)
  ggsave(file = plotFile)
}
# -----------------------------------------------------------------------------------------------------------------------

#  histogram: % operating accruals (by fiscal year)
#plotQuantileDistributions(fdt = fundamentals.dt, plotColumn = "pct_operating_accruals", facetExpr = "fyear~.", 
#                          startYear = 2001, endYear = 2015, 
#                          plotTitle = "% Operating Accrual by Fiscal Year \n", 
#                          plotFile = paste0(base.dir.output, "plot-pctopaccr_by_fyear.png"))

print("generating plots...")

#  histogram: % operating accruals (by fiscal year)
ggplot(fundamentals.dt[fyear > 2001], aes(x = pct_operating_accruals), facets = fyear~.) + geom_histogram() +
       facet_wrap(~ fyear, ncol = 3) +  labs(title = "% Operating Accrual by Fiscal Year \n") 
ggsave(file = paste0(base.dir.output, "plot-pctopaccr_by_fyear.png"))
 
#  histogram: % operating accruals (by market cap quantile)
ggplot(fundamentals.dt[fyear > 2001 & qtile_market_cap < 10 ],  aes(x = pct_operating_accruals),  facets = fyear~.) + geom_histogram() +
       facet_wrap(~ qtile_market_cap, ncol = 3) + labs(title = "% Operating Accrual by Market Cap Quantile \n") 
ggsave(file = paste0(base.dir.output, "plot-pctopaccr_by_mktcap.png"))

#  histogram: % operating accruals (by market cap quantile) -- ONLY for market cap quantile > 1
ggplot(fundamentals.dt[fyear > 2001 & qtile_market_cap > 1 & qtile_market_cap < 10],  aes(x = pct_operating_accruals), facets = fyear~.) +  geom_histogram() +
       facet_wrap(~ qtile_market_cap, ncol = 3) + labs(title = "% Operating Accrual by Market Cap Quantile (Excl Quantile 1) \n") 
ggsave(file = paste0(base.dir.output, "plot-pctopaccr_by_mktcap_hival.png"))

# info purposes -  distribution of total accruals, by fyear
ggplot(fundamentals.dt[fyear > 2001 & qtile_market_cap < 10],  aes(x = pct_total_accruals), facets = fyear~.) + geom_histogram() +
       facet_wrap(~ fyear, ncol = 3) + labs(title = "INFO: Total Accrual by Market Cap Quantile by Fiscal Year \n") 
ggsave(file = paste0(base.dir.output, "plot-totaccr_by_fyear.png"))
  
# info purposes - total accruals, by market cap quantile > 1
ggplot(fundamentals.dt[fyear > 2001  & qtile_market_cap > 1 & qtile_market_cap < 10], aes(x = pct_total_accruals), facets = fyear~.) +  geom_histogram() + 
       facet_wrap(~ qtile_market_cap, ncol = 3) + labs(title = "INFO: Total Accrual by Market Cap Quantile (Excl Quantile 1) \n")
ggsave(file = paste0(base.dir.output, "plot-totaccr_by_mktcap_hival.png"))
    

