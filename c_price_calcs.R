################################################################
# calculate loess fits and confidence intervals (1 s.d.)
################################################################
##########################################################
# calculate loess fits of NPV values
# then calculated fitted relative NPVs
##########################################################
loess_bes_npv <- loess(BES_NPV ~ c_price, all_data)
loess_bebcs_npv <- loess(BEBCS_NPV ~ c_price, all_data)
if (beccs) loess_beccs_npv <- loess(BECCS_NPV ~ c_price, all_data)
#calculate opportunity costs
cp <- all_data$c_price
fitted_bes_oc <- vector(mode="numeric", length=length(cp))
fitted_bebcs_oc <- vector(mode="numeric", length=length(cp))
if (beccs) {
  fitted_beccs_oc <- vector(mode="numeric", length=length(cp))
  for (i in 1:NROW(cp)) {
    fitted_bes_oc[i] <- max(loess_bebcs_npv$fitted[i],loess_beccs_npv$fitted[i])
    fitted_bebcs_oc[i] <- max(loess_bes_npv$fitted[i],loess_beccs_npv$fitted[i])
    fitted_beccs_oc[i] <- max(loess_bes_npv$fitted[i],loess_bebcs_npv$fitted[i])
  }
} else {
  for (i in 1:NROW(cp)) {
    fitted_bes_oc[i] <-   loess_bebcs_npv$fitted[i]
    fitted_bebcs_oc[i] <- loess_bes_npv$fitted[i]
  }
}
#then use OC to calculate relative values
fitted_bes_rv <- loess_bes_npv$fitted-fitted_bes_oc
fitted_bebcs_rv <- loess_bebcs_npv$fitted-fitted_bebcs_oc
if (beccs) fitted_beccs_rv <- loess_beccs_npv$fitted-fitted_beccs_oc

##########################################################
# calculate BEBCS variance and loess fit
##########################################################
loess_bebcs <- loess(BEBCS ~ c_price, all_data)
r2_bebcs <- loess_bebcs$residuals^2
r2l_bebcs <-log(r2_bebcs)
var_fit_bebcs <- lm(r2l_bebcs ~ all_data$c_price) # variance varies with C price, so fit function of C price to it
variance_bebcs <- exp(var_fit_bebcs$fitted.values)
stddev_bebcs <- sqrt(variance_bebcs)
ci_min_bebcs <- fitted_bebcs_rv-stddev_bebcs
ci_max_bebcs <- fitted_bebcs_rv+stddev_bebcs

##########################################################
# calculate BECCS variance and loess fit
##########################################################
if (beccs) {
  loess_beccs <- loess(BECCS ~ c_price, all_data)
  r2_beccs <- loess_beccs$residuals^2
  r2l_beccs <-log(r2_beccs)
  var_fit_beccs <- lm(r2l_beccs ~ all_data$c_price)
  variance_beccs <- exp(var_fit_beccs$fitted.values)
  stddev_beccs <- sqrt(variance_beccs)
  ci_min_beccs <- fitted_beccs_rv-stddev_beccs
  ci_max_beccs <- fitted_beccs_rv+stddev_beccs
}

##########################################################
#calculate BES variance and loess fit
##########################################################
loess_bes <- loess(BES ~ c_price, all_data)
r2_bes <- loess_bes$residuals^2
r2l_bes <-log(r2_bes)
var_fit_bes <- lm(r2l_bes ~ all_data$c_price)
variance_bes <- exp(var_fit_bes$fitted.values)
stddev_bes <- sqrt(variance_bes)
ci_min_bes <- fitted_bes_rv-stddev_bes
ci_max_bes <- fitted_bes_rv+stddev_bes

##################################################################
# calc probability of NET being best, as function of C price
##################################################################
Npoints.in.bin <- 300
Nbins <- (NROW(all_data) - Npoints.in.bin) %/% Npoints.in.bin
N <- 1:Nbins
binmax <- N
binmin <- binmax

for (i in 1:Nbins) {
  j <- (i-1) * Npoints.in.bin + 1
  binmin[i] <- all_data[j,]$c_price
  binmax[i] <- all_data[j+Npoints.in.bin-1,]$c_price
}

bins <- data.frame(binmin,binmax)
bins$binmid <- (bins$binmax + bins$binmin)/2
bins$bes_prob <- 0
bins$bebcs_prob <- 0
bins$beccs_prob <- 0
for (bin in 1:Nbins) {
  index <- (bin-1) * Npoints.in.bin + 1
  #Npoints <- NROW(which ( all_data$c_price >= binmin[bin] & all_data$c_price < binmax[bin])  )
  bespoints   <- NROW(which (all_data[index:(index+Npoints.in.bin-1)]$BES   > 0)  )
  bebcspoints <- NROW(which (all_data[index:(index+Npoints.in.bin-1)]$BEBCS > 0)  )
  beccspoints <- NROW(which (all_data[index:(index+Npoints.in.bin-1)]$BECCS > 0)  )
  bins$bes_prob[bin] <- bespoints / Npoints.in.bin
  bins$bebcs_prob[bin] <- bebcspoints / Npoints.in.bin
  bins$beccs_prob[bin] <- beccspoints / Npoints.in.bin
}
smooth_prob <- as.data.frame(bins$binmid); colnames(smooth_prob)[1] <- "c_price"
smooth_prob$bes   <-   regSmoothAuto(x=smooth_prob$c_price, y=bins$bes_prob  , lambda=2e7)$yhat
smooth_prob$bebcs <-   regSmoothAuto(x=smooth_prob$c_price, y=bins$bebcs_prob, lambda=2e7)$yhat
smooth_prob$beccs <-   regSmoothAuto(x=smooth_prob$c_price, y=bins$beccs_prob, lambda=2e7)$yhat
smooth_prob$bes[which(smooth_prob$bes <0)] <- 0
smooth_prob$bes <- regSmooth(x=smooth_prob$c_price, y=smooth_prob$bes  , lambda=2e5)$yhat
smooth_prob$bebcs <- 1- smooth_prob$bes - smooth_prob$beccs
smooth_prob_melt <- melt(smooth_prob, id=c("c_price"))

source("c_price_plots.R")