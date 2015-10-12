fatals <- read.csv("~/projects/LAT/movie-set-fatalities/Fatals.csv")

# make time series
# frequency is number of subunits per unit of time
# in this case, unit of time is year and there is only 1 count per year, so frequency = 1
# start and end are vectors with: c(year, subunit in year)

fatals_ts <- ts(fatals$Fatals, frequency = 1, start = c(1992,1), end = c(2014,1))

########################
# PACKAGE: strucchange #
########################

library(strucchange)

mean_bp = breakpoints(fatals_ts ~ 1)
summary(mean_bp)
plot(mean_bp)
# want to find "elbow" of BIC (Bayesian Information Criteria), it occurs at either 3 or 4 breakpoints, probably 4

plot(fatals_ts, main = 'Method: strucchange\nBreakpoints: 3', xlab = "Year", ylab = "Fatalities", type = 'b')
lines(fitted(mean_bp, breaks = 3), col = 4)

plot(fatals_ts, main = 'Method: strucchange\nBreakpoints: 4', xlab = "Year", ylab = "Fatalities", type = 'b')
lines(fitted(mean_bp, breaks = 4), col = 2)

########################
# PACKAGE: changepoint #
########################

library(changepoint)

### Binary Segmentation
# package documentation says this is "arguably the most widely used multiple changepoint search method"

mean_binseg = cpt.mean(fatals_ts, method = 'BinSeg')
plot(mean_binseg, main = 'Method: Binary Segmentation', xlab = "Year", ylab = "Fatalities")
# this suggests 4 breakpoints

### Segment Neighborhoods

mean_segneigh = cpt.mean(fatals_ts, method = 'SegNeigh')
plot(mean_segneigh, main = 'Method: Segment Neighborhoods', xlab = "Year", ylab = "Fatalities")
# this suggests 3 breakpoints
# but I don't think it's accounting for the  9 in 2000 properly

### PELT

mean_pelt = cpt.mean(fatals_ts, method = 'PELT', penalty = 'BIC')
plot(mean_pelt, main = 'Method: PELT', xlab = "Year", ylab = "Fatalities")
# this is odd because some segments are much too small
# I think this model is overfit to the data

################
# PACKAGE: bcp #
################

library(bcp)

mean_bcp = bcp(fatals$Fatals)
plot(mean_bcp)
plot(1992:2014, mean_bcp$posterior.prob, type= 'b', xlab='year', ylab = 'probability of changepoint')
abline(v=1994);abline(v=1998);abline(v=2002);abline(v=2009)
bcp_df <- data.frame(year = 1992:2014, prob_of_bp = mean_bcp$posterior.prob)
bcp_df <- bcp_df[order(bcp_df$prob_of_bp, decreasing = TRUE),]

# this is a simulation-based method that is tougher to interpret