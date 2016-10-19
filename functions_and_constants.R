<<<<<<< HEAD
#define a colorblind-friendly pallete
#http://www.somersault1824.com/tips-for-designing-scientific-figures-for-color-blind-readers/
col1.bes   <- rgb(0,146,146, maxColorValue = 255)
col1.bebcs <- rgb(73,0,146, maxColorValue = 255)
col1.beccs <- rgb(219,109,0, maxColorValue = 255)
col2.bes   <- rgb(0,146,146,68, maxColorValue = 255)
col2.bebcs <- rgb(73,0,146,68, maxColorValue = 255)
col2.beccs <- rgb(219,209,0,68, maxColorValue = 255)
col1.2020  <- "black"
col2.2020  <- rgb(182,219,255, maxColorValue = 255)
col1.2050  <- "black"
col2.2050  <- rgb(255,182,219, maxColorValue = 255)
col1.2100  <- "black"
col2.2100  <- rgb(255,255,109, maxColorValue = 255)

Forest_data_length <- 5000 # length of subsample for random forest

########################################################################
# Make pretty labels from column names
########################################################################
improve_labels <- function(bad_labels) {
  improved_labels <- gsub(x = bad_labels, pattern = "\\_", replacement = " ")
  improved_labels <- gsub(x = improved_labels, pattern = "n2o", replacement = "N2O")
  improved_labels <- gsub(x = improved_labels, pattern = "co2", replacement = "CO2")
  improved_labels <- gsub(x = improved_labels, pattern = "bebcs", replacement = "BEBCS")
  improved_labels <- gsub(x = improved_labels, pattern = "bc", replacement = "BC")
  improved_labels <- gsub(x = improved_labels, pattern = "beccs", replacement = "BECCS")
  improved_labels <- gsub(x = improved_labels, pattern = "bes", replacement = "BES")
  improved_labels <- gsub(x = improved_labels, pattern = "seq", replacement = "seq.")
  improved_labels <- gsub(x = improved_labels, pattern = "disc", replacement = "discount")
  improved_labels <- gsub(x = improved_labels, pattern = "bm", replacement = "BM")
  improved_labels <- gsub(x = improved_labels, pattern = "eff", replacement = "efficiency")
  improved_labels <- gsub(x = improved_labels, pattern = "temp", replacement = "temperature")
  improved_labels <- gsub(x = improved_labels, pattern = "soc_factor", replacement = "BC SOC priming")
  improved_labels <- gsub(x = improved_labels, pattern = "soc", replacement = "SOC")
  improved_labels <- gsub(x = improved_labels, pattern = "gr", replacement = "crop")
  improved_labels <- gsub(x = improved_labels, pattern = "elec", replacement = "electricity")
  improved_labels <- gsub(x = improved_labels, pattern = "rebound", replacement = "rebound factor")
  improved_labels <- gsub(x = improved_labels, pattern = "ccs", replacement = "CCS")
  improved_labels <- gsub(x = improved_labels, pattern = "app", replacement = "application")
  improved_labels <- gsub(x = improved_labels, pattern = " e ", replacement = " energy ")
  improved_labels <- gsub(x = improved_labels, pattern = "stab", replacement = "persistence")
  improved_labels <- gsub(x = improved_labels, pattern = "ff ", replacement = "FF ")
  improved_labels <- gsub(x = improved_labels, pattern = "py ", replacement = "py. ")
  improved_labels <- gsub(x = improved_labels, pattern = "cc", replacement = "capital cost")
  #improved_labels <- gsub(x = improved_labels, pattern = "factor", replacement = "priming")
  improved_labels <- gsub(x = improved_labels, pattern = "price increase", replacement = "price")
  #improved_labels <- gsub(x = improved_labels, pattern = "BECCS ", replacement = "CCS ")
  improved_labels <- capitalize(improved_labels)
  if (improved_labels[1] == "BES")   {improved_labels[1] <- "BES RPV"}
  if (improved_labels[2] == "BECCS") {improved_labels[2] <- "BECCS RPV"}
  if (improved_labels[3] == "BEBCS") {improved_labels[3] <- "BEBCS RPV"}
  #improved_labels <- paste("`",improved_labels,"`",sep="")
  return(improved_labels)
}

param.names = c("Scenario","Year","BES_NPV","BECCS_NPV",
                "BEBCS_NPV","BES","BECCS","BEBCS","BEBCS_soil_GHG",
                "bc_price","disc_rate","bm_ash","crop_price_increase",
                "lime_price","c_price","elec_price","rebound","c_intensity",
                "ff_eff","bes_cc","bes_life","bes_eff","py_temp",
                "py_e_source","py_elec_eff","bebcs_cc","bebcstobe_life_ratio",
                "bc_haul_cost","bc_field_cost","n_app_rate","n2o_factor",
                "n2o_years","soc_factor","bc_haul_co2","soc","app_rate",
                "bc_nutrient","bc_yield_impact","bc_stab_fact","ccs_cost",
                "beccs_eff_penalty","beccs_seq_fraction")
param.names = as.data.table(as.data.frame(param.names))
param.names$labels = c("Scenario","Year","BES NPV","BECCS NPV",
                       "BEBCS NPV","BES RPV","BECCS RPV","BEBCS RPV","BEBCS soil-GHG abatement",
                       "BC price","Discount rate","Biomass ash","Crop price increase",
                       "Aglime price","Carbon price","Electricity price","Rebound factor",
                       "Power sector carbon intensity",
                       "Electrical generation efficiency (fossil fuels)",
                       "BES capital cost","BES lifetime","BES generation efficiency",
                       "Pyrolysis temperature",
                       "Pyrolysis heat source","Pyrolysis elecrical efficiency",
                       "BEBS capital cost","bebcstobe_life_ratio",
                       "BC haulage cost","BC spreading/tillage cost","N-fertilizer application rate",
                       "BC impact on soil N2O","Length of BC impact on soil N2O",
                       "BC impact on SOC turnover","BC haulage emissions","SOC","BC application rate",
                       "BC nutrient content","BC crop-yield impact","BC persistence","CO2 capture and storage cost",
                       "BECCS efficiency penalty","BECCS %CO2 sequestered")
param.to.label <- function(param) {
  if (param %in% levels(param.names$param.names)) {
    return(param.names[param.names==param]$labels)
  }
  else
  {
    return("Unknown parameter")
  }
}


#################################################################
# Define data files:
# *** Edit this list to contain the files you want to process ***
#################################################################
scenario_files <- c(
  "op_space_beccs.txt",
  "2020_RCP430_beccs.txt",
  "2050_RCP430_beccs.txt",
  "2100_RCP430_beccs.txt",
  "2020_RCP650_beccs.txt",
  "2050_RCP650_beccs.txt",
  "2100_RCP650_beccs.txt")

response <- tkmessageBox(message="Save plots?", icon="question", type = "yesno", default = "yes")
if (as.character(response)[1]=="yes") {
  saveplots <- TRUE
} else {
  saveplots <- FALSE}

response <- tkmessageBox(message="Do Random Forest?", icon="question", type = "yesno", default = "yes")
if (as.character(response)[1]=="yes") {
  do_forest <- TRUE
} else {
  do_forest <- FALSE}


#################################################################
# Define carbon price distributions for each scenario
#################################################################

mu.2020_430 <- 5.27
sigma.2020_430 <- 0.42
mu.2050_430 <- 6.63
sigma.2050_430 <- 0.61
mu.2100_430 <- 8.65
sigma.2100_430 <- 0.75

mu.2020_650 <- 3.69
sigma.2020_650 <- 0.11
mu.2050_650 <- 5.16
sigma.2050_650 <- 0.08
mu.2100_650 <- 7.35
sigma.2100_650 <- 0.22

x2020_430 <- rlnorm(1000, mu.2020_430, sigma.2020_430)  # for example
r2020_430 <- range(x2020_430)
d2020_430 <- dlnorm(r2020_430[1]:r2020_430[2], meanlog = mu.2020_430, sdlog = sigma.2020_430)

x2050_430 <- rlnorm(1000, mu.2050_430, sigma.2050_430)  # for example
r2050_430 <- range(x2050_430)
d2050_430 <- dlnorm(r2050_430[1]:r2050_430[2], meanlog = mu.2050_430, sdlog = sigma.2050_430)

x2100_430 <- rlnorm(1000, mu.2100_430, sigma.2100_430)  # for example
r2100_430 <- range(x2100_430)
d2100_430 <- dlnorm(r2100_430[1]:r2100_430[2], meanlog = mu.2100_430, sdlog = sigma.2100_430)

x2020_650 <- rlnorm(1000, mu.2020_650, sigma.2020_650)  # for example
r2020_650 <- range(x2020_650)
d2020_650 <- dlnorm(r2020_650[1]:r2020_650[2], meanlog = mu.2020_650, sdlog = sigma.2020_650)

x2050_650 <- rlnorm(1000, mu.2050_650, sigma.2050_650)  # for example
r2050_650 <- range(x2050_650)
d2050_650 <- dlnorm(r2050_650[1]:r2050_650[2], meanlog = mu.2050_650, sdlog = sigma.2050_650)

x2100_650 <- rlnorm(1000, mu.2100_650, sigma.2100_650)  # for example
r2100_650 <- range(x2100_650)
d2100_650 <- dlnorm(r2100_650[1]:r2100_650[2], meanlog = mu.2100_650, sdlog = sigma.2100_650)

c_price_dist <- data.frame(x2020_650, x2050_650, x2100_650, x2020_430, x2050_430, x2100_430)
colnames(c_price_dist) <- c("MS650-720 (2020)","MS650-720 (2050)","MS650-720 (2100)","MS430-480 (2020)","MS430-480 (2050)","MS430-480 (2100)")
c_price_dist.long <- melt(c_price_dist, value.name = "c_price")

# Function to use boxplot.stats to set the box-and-whisker locations
mybxp = function(x) {
  x <- 10^x
  bxp = log10(boxplot.stats(x)[["stats"]])
  names(bxp) = c("ymin","lower", "middle","upper","ymax")
  return(bxp)
}

# Function to use boxplot.stats for the outliers
myout = function(x) {
  x <- 10^x
  data.frame(y=log10(boxplot.stats(x)[["out"]]))
}

# Function to use mean and s.d. to set the box-and-whisker locations
my.sd.bxp = function(x) {
  sd <- sd(x)
  m <- mean(x)
  bxp = c(m-sd, m-sd, m, m+sd, m+sd)
  names(bxp) = c("ymin","lower", "middle","upper","ymax")
  return(bxp)
}

=======
#define a colorblind-friendly pallete
#http://www.somersault1824.com/tips-for-designing-scientific-figures-for-color-blind-readers/
col1.bes   <- rgb(0,146,146, maxColorValue = 255)
col1.bebcs <- rgb(73,0,146, maxColorValue = 255)
col1.beccs <- rgb(219,109,0, maxColorValue = 255)
col2.bes   <- rgb(0,146,146,68, maxColorValue = 255)
col2.bebcs <- rgb(73,0,146,68, maxColorValue = 255)
col2.beccs <- rgb(219,209,0,68, maxColorValue = 255)
col1.2020  <- "black"
col2.2020  <- rgb(182,219,255, maxColorValue = 255)
col1.2050  <- "black"
col2.2050  <- rgb(255,182,219, maxColorValue = 255)
col1.2100  <- "black"
col2.2100  <- rgb(255,255,109, maxColorValue = 255)

Forest_data_length <- 5000 # length of subsample for random forest

########################################################################
# Make pretty labels from column names
########################################################################
improve_labels <- function(bad_labels) {
  improved_labels <- gsub(x = bad_labels, pattern = "\\_", replacement = " ")
  improved_labels <- gsub(x = improved_labels, pattern = "n2o", replacement = "N2O")
  improved_labels <- gsub(x = improved_labels, pattern = "co2", replacement = "CO2")
  improved_labels <- gsub(x = improved_labels, pattern = "bebcs", replacement = "BEBCS")
  improved_labels <- gsub(x = improved_labels, pattern = "bc", replacement = "BC")
  improved_labels <- gsub(x = improved_labels, pattern = "beccs", replacement = "BECCS")
  improved_labels <- gsub(x = improved_labels, pattern = "bes", replacement = "BES")
  improved_labels <- gsub(x = improved_labels, pattern = "seq", replacement = "seq.")
  improved_labels <- gsub(x = improved_labels, pattern = "disc", replacement = "discount")
  improved_labels <- gsub(x = improved_labels, pattern = "bm", replacement = "BM")
  improved_labels <- gsub(x = improved_labels, pattern = "eff", replacement = "efficiency")
  improved_labels <- gsub(x = improved_labels, pattern = "temp", replacement = "temperature")
  improved_labels <- gsub(x = improved_labels, pattern = "soc_factor", replacement = "BC SOC priming")
  improved_labels <- gsub(x = improved_labels, pattern = "soc", replacement = "SOC")
  improved_labels <- gsub(x = improved_labels, pattern = "gr", replacement = "crop")
  improved_labels <- gsub(x = improved_labels, pattern = "elec", replacement = "electricity")
  improved_labels <- gsub(x = improved_labels, pattern = "rebound", replacement = "rebound factor")
  improved_labels <- gsub(x = improved_labels, pattern = "ccs", replacement = "CCS")
  improved_labels <- gsub(x = improved_labels, pattern = "app", replacement = "application")
  improved_labels <- gsub(x = improved_labels, pattern = " e ", replacement = " energy ")
  improved_labels <- gsub(x = improved_labels, pattern = "stab", replacement = "persistence")
  improved_labels <- gsub(x = improved_labels, pattern = "ff ", replacement = "FF ")
  improved_labels <- gsub(x = improved_labels, pattern = "py ", replacement = "py. ")
  improved_labels <- gsub(x = improved_labels, pattern = "cc", replacement = "capital cost")
  #improved_labels <- gsub(x = improved_labels, pattern = "factor", replacement = "priming")
  improved_labels <- gsub(x = improved_labels, pattern = "price increase", replacement = "price")
  #improved_labels <- gsub(x = improved_labels, pattern = "BECCS ", replacement = "CCS ")
  improved_labels <- capitalize(improved_labels)
  if (improved_labels[1] == "BES")   {improved_labels[1] <- "BES RPV"}
  if (improved_labels[2] == "BECCS") {improved_labels[2] <- "BECCS RPV"}
  if (improved_labels[3] == "BEBCS") {improved_labels[3] <- "BEBCS RPV"}
  #improved_labels <- paste("`",improved_labels,"`",sep="")
  return(improved_labels)
}

param.names = c("Scenario","Year","BES_NPV","BECCS_NPV",
                "BEBCS_NPV","BES","BECCS","BEBCS","BEBCS_soil_GHG",
                "bc_price","disc_rate","bm_ash","crop_price_increase",
                "lime_price","c_price","elec_price","rebound","c_intensity",
                "ff_eff","bes_cc","bes_life","bes_eff","py_temp",
                "py_e_source","py_elec_eff","bebcs_cc","bebcstobe_life_ratio",
                "bc_haul_cost","bc_field_cost","n_app_rate","n2o_factor",
                "n2o_years","soc_factor","bc_haul_co2","soc","app_rate",
                "bc_nutrient","bc_yield_impact","bc_stab_fact","ccs_cost",
                "beccs_eff_penalty","beccs_seq_fraction")
param.names = as.data.table(as.data.frame(param.names))
param.names$labels = c("Scenario","Year","BES NPV","BECCS NPV",
                       "BEBCS NPV","BES RPV","BECCS RPV","BEBCS RPV","BEBCS soil-GHG abatement",
                       "BC price","Discount rate","Biomass ash","Crop price increase",
                       "Aglime price","Carbon price","Electricity price","Rebound factor",
                       "Power sector carbon intensity",
                       "Electrical generation efficiency (fossil fuels)",
                       "BES capital cost","BES lifetime","BES generation efficiency",
                       "Pyrolysis temperature",
                       "Pyrolysis heat source","Pyrolysis elecrical efficiency",
                       "BEBS capital cost","bebcstobe_life_ratio",
                       "BC haulage cost","BC spreading/tillage cost","N-fertilizer application rate",
                       "BC impact on soil N2O","Length of BC impact on soil N2O",
                       "BC impact on SOC turnover","BC haulage emissions","SOC","BC application rate",
                       "BC nutrient content","BC crop-yield impact","BC persistence","CO2 capture and storage cost",
                       "BECCS efficiency penalty","BECCS %CO2 sequestered")
param.to.label <- function(param) {
  if (param %in% levels(param.names$param.names)) {
    return(param.names[param.names==param]$labels)
  }
  else
  {
    return("Unknown parameter")
  }
}


#################################################################
# Define data files:
# *** Edit this list to contain the files you want to process ***
#################################################################
scenario_files <- c(
  "op_space_beccs.txt",
  "2020_RCP430_beccs.txt",
  "2050_RCP430_beccs.txt",
  "2100_RCP430_beccs.txt",
  "2020_RCP650_beccs.txt",
  "2050_RCP650_beccs.txt",
  "2100_RCP650_beccs.txt")

response <- tkmessageBox(message="Save plots?", icon="question", type = "yesno", default = "yes")
if (as.character(response)[1]=="yes") {
  saveplots <- TRUE
} else {
  saveplots <- FALSE}

response <- tkmessageBox(message="Do Random Forest?", icon="question", type = "yesno", default = "yes")
if (as.character(response)[1]=="yes") {
  do_forest <- TRUE
} else {
  do_forest <- FALSE}


#################################################################
# Define carbon price distributions for each scenario
#################################################################

mu.2020_430 <- 5.27
sigma.2020_430 <- 0.42
mu.2050_430 <- 6.63
sigma.2050_430 <- 0.61
mu.2100_430 <- 8.65
sigma.2100_430 <- 0.75

mu.2020_650 <- 3.69
sigma.2020_650 <- 0.11
mu.2050_650 <- 5.16
sigma.2050_650 <- 0.08
mu.2100_650 <- 7.35
sigma.2100_650 <- 0.22

x2020_430 <- rlnorm(1000, mu.2020_430, sigma.2020_430)  # for example
r2020_430 <- range(x2020_430)
d2020_430 <- dlnorm(r2020_430[1]:r2020_430[2], meanlog = mu.2020_430, sdlog = sigma.2020_430)

x2050_430 <- rlnorm(1000, mu.2050_430, sigma.2050_430)  # for example
r2050_430 <- range(x2050_430)
d2050_430 <- dlnorm(r2050_430[1]:r2050_430[2], meanlog = mu.2050_430, sdlog = sigma.2050_430)

x2100_430 <- rlnorm(1000, mu.2100_430, sigma.2100_430)  # for example
r2100_430 <- range(x2100_430)
d2100_430 <- dlnorm(r2100_430[1]:r2100_430[2], meanlog = mu.2100_430, sdlog = sigma.2100_430)

x2020_650 <- rlnorm(1000, mu.2020_650, sigma.2020_650)  # for example
r2020_650 <- range(x2020_650)
d2020_650 <- dlnorm(r2020_650[1]:r2020_650[2], meanlog = mu.2020_650, sdlog = sigma.2020_650)

x2050_650 <- rlnorm(1000, mu.2050_650, sigma.2050_650)  # for example
r2050_650 <- range(x2050_650)
d2050_650 <- dlnorm(r2050_650[1]:r2050_650[2], meanlog = mu.2050_650, sdlog = sigma.2050_650)

x2100_650 <- rlnorm(1000, mu.2100_650, sigma.2100_650)  # for example
r2100_650 <- range(x2100_650)
d2100_650 <- dlnorm(r2100_650[1]:r2100_650[2], meanlog = mu.2100_650, sdlog = sigma.2100_650)

c_price_dist <- data.frame(x2020_650, x2050_650, x2100_650, x2020_430, x2050_430, x2100_430)
colnames(c_price_dist) <- c("MS650-720 (2020)","MS650-720 (2050)","MS650-720 (2100)","MS430-480 (2020)","MS430-480 (2050)","MS430-480 (2100)")
c_price_dist.long <- melt(c_price_dist, value.name = "c_price")

# Function to use boxplot.stats to set the box-and-whisker locations
mybxp = function(x) {
  x <- 10^x
  bxp = log10(boxplot.stats(x)[["stats"]])
  names(bxp) = c("ymin","lower", "middle","upper","ymax")
  return(bxp)
}

# Function to use boxplot.stats for the outliers
myout = function(x) {
  x <- 10^x
  data.frame(y=log10(boxplot.stats(x)[["out"]]))
}

# Function to use mean and s.d. to set the box-and-whisker locations
my.sd.bxp = function(x) {
  sd <- sd(x)
  m <- mean(x)
  bxp = c(m-sd, m-sd, m, m+sd, m+sd)
  names(bxp) = c("ymin","lower", "middle","upper","ymax")
  return(bxp)
}

>>>>>>> f22ddffdc4b9000d81fcf21ee02fac3fe2f46fb0
