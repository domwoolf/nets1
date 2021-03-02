library(data.table)

#####################
# Define constants
#####################
Results.directory	<- "."
time_frame	<- 100 #years
gwp_n2o <- 298.00 # "Anthropogenic and Natural Radiative Forcing". In: Climate Change 2013: The Physical Science Basis. Contribution of Working Group I to the Fifth Assessment Report of the Intergovernmental Panel on Climate Change.
gwp_ch4	<- 34.00
bm_c <- 0.50  #biomass C
bm_h <- 0.06  #H
bm_o <- 0.44 	#O
lignin <- 0.25
bm_h2o <- 0.25	#biomass moisture content
bm_ash <- 0.04
bm_base <- 0.02	#biomass base cations	- based on average base:ash ratio of straw, wood,husk,shell,plant from phyllis 2.
bm_lhv	<- 100*(0.3491*bm_c+1.1783*bm_h-0.1034*bm_o) * (18.74/19.95)	#lower heating value (GJ / Mg daf) - 18.74/19.95 conversion factor from HHV to LHV based on mean from phyllis 2 database
bc_bd <- 	1.3 #Soil bulk density	Mg/m3
soil_depth <- 0.25 #m
O_M_factor <- 0.04 #(annual operation & maintenance / annualized capital cost)
capacity_factor <- 0.85
beccs_available <- TRUE
bm_cost = NA # don't use biomass cost, because calculate relative value only


rpv = function(this.scenario){
  val = function(param) {param = as.character(substitute(param)); this.scenario[name==param, val][]}
  ##############################
  # BES calcs
  ##############################
  bes_elec_prod <- bm_lhv * val(bes_energy_efficiency) #GJ / Mg f
  FF.C.offset <- val(ffe_c_intensity) * bes_elec_prod * val(rebound) #Mg C / Mg f # changed to c intensity from bm_lhv in excel

  bes_tot_c_abatement <- FF.C.offset #Mg C / Mg f

  #costs
  bes_annuity	<-  (1-(1 /((1+val(discount))^val(bes_life))))  / val(discount) #  annuity factor
  bes_annual_cc <-  val(bes_cc) / bes_annuity # annualised capital cost per Mgf ($/Mg f)
  bes_om <- val(bes_cc)*O_M_factor # annualised capital cost per Mgf ($/Mg f)
  bes_total_costs	= sum(bes_annual_cc, bes_om, bm_cost, na.rm = T) # ($/Mg f)

  #Revenues
  bes_c_credits <- bes_tot_c_abatement*val(c_price) # ($/Mg f)
  bes_elec_revenue <- bes_elec_prod*val(elec_price) # ($/Mg f)
  bes_total_revenue <- sum(bes_c_credits, bes_elec_revenue, na.rm = T) # ($/Mg f)

  ##############################
  # BECCS calcs
  ##############################
  beccs_elec_prod <- bm_lhv *(val(bes_energy_efficiency) - val(beccs_eff_penalty)) # GJ / Mg f
  beccs_FF.C.offset	<- beccs_elec_prod * val(ffe_c_intensity) * val(rebound) #	Mg C / Mg f
  beccs_C_sequestered <- bm_c*val(beccs_seq_fraction) # Mg C / Mg f
  beccs_tot_c_abatement <- beccs_FF.C.offset + beccs_C_sequestered	# Mg C / Mg f

  #Costs
  beccs_annual_cc <- bes_annual_cc #	$/Mg f
  ccs_cost <- val(ccs_cc) * bm_c * val(beccs_seq_fraction)	# $/Mg f
  beccs_om <- val(bes_cc) * O_M_factor	# $/Mg f - O&M of the ccs component already included in the ccs_cost
  beccs_total_costs	= sum(beccs_annual_cc, ccs_cost, beccs_om, bm_cost, na.rm = T)	# $/ Mg f

  #Revenues
  beccs_c_credits <- beccs_tot_c_abatement*val(c_price)	#$ / Mg f
  beccs_elec_revenue <- beccs_elec_prod*val(elec_price)	#$ / Mg f
  beccs_elec_revenue <- beccs_elec_revenue	# NB we use BES fuel revenue, because cost of CCS energy is already included in CCS cost above (including lost revenue from energy due to reduced efficiency)
  beccs_total_revenue	<- sum(beccs_c_credits, beccs_elec_revenue, na.rm = T) # $ / Mg f

  ##############################
  # BEBCS calcs
  ##############################
  #yields
  bc_y	= 0.1260917 +0.27332*lignin + 0.5391409*exp(-0.004*val(py_temp))
  bc_c	= 0.99-0.78*exp(-0.0042*val(py_temp))
  bc_h	= -0.0041+0.1*exp(-0.0024*val(py_temp))
  bc_o	= 1-bc_c-bc_h
  biochar_C_yield	= bc_y*bc_c #	Mg C / Mg f
  g_h2_y	=0.029528*(1-exp(-0.003496*(val(py_temp)+273)))^62.980403
  g_co_y	= (0.043)/(1+exp(-0.03*(val(py_temp)+273)+17.2)) + (0.36-0.043)/(1+exp(-0.01*(val(py_temp)+273)+10.9))
  g_ch4_y	=0.07818*(1-exp(-0.0033788*(val(py_temp)+273)))^30.14865
  g_c2h4_y	=0.035637*(1-exp(-0.005221*(val(py_temp)+273)))^154.974
  Uc	=bm_c-bc_y*bc_c - g_co_y*12/28 -g_ch4_y*12/16 - g_c2h4_y*24/28
  Uo	=bm_o-bc_y*bc_o - g_co_y*16/28
  Uh	=bm_h-bc_h*bc_y - g_h2_y -g_ch4_y*4/16 - g_c2h4_y*4/28
  bo_c	=1.25*bm_c
  bo_h	=1.26*bm_h
  bo_o	=1-bo_c-bo_h
  bo_y =(Uo - Uc*32/12 - Uh*16/2)/(bo_o-bo_c*32/12-bo_h*16/2)	# Biooil yield
  g_co2_y	=(Uc-bo_c*bo_y)/(12/44)
  g_y	= sum(g_h2_y, g_co_y, g_co2_y, g_ch4_y, g_c2h4_y)
  g_h2o_y	= (Uh-bo_h*bo_y)/(2/18)
  h20_from_biomass = bm_h2o/(1-bm_h2o)
  py_tot_h2o	= h20_from_biomass + g_h2o_y

  #energy
  bm_bd <- 	250	 #kg/m^3   Feed bulk density
  bm_feed_rate <- 250	#kg/hr
  daf_feed_rate	=bm_feed_rate*(1-bm_ash)	#kg/hr
  py_res_t = ifelse(val(py_temp)>340, (654*(val(py_temp)-332)^-0.56)/60, 12) #hr  residence time
  py_vol = bm_feed_rate*py_res_t/bm_bd	#m^3  pyrolysis vessel volume
  py_aspect <- 20	# vessel length:radius aspect ratio
  py_rad	=(py_vol/(pi*py_aspect))^(1/3) #m  vessel internal radius
  py_wall <- 0.025	#m  wall thickness
  py_insul <- 0.05	#m  insulation thickness
  py_therm_cond <- 0.123	#W/m K  insulation thermal conductance (CaSiO)
  py_len <- py_rad*py_aspect	#m  vessel length
  py_area <- (2*pi*py_rad*py_len)+(2*pi*(py_rad+py_wall+py_insul)^2)	# m^2  Vessel surface area
  surface_T <- 40	#C   outer surface temperature
  py_heat_loss_rate <- py_therm_cond*(val(py_temp)-surface_T)*py_area/(py_insul*1000)	#kW

  py_ex_t <- 170	#C
  bc_cp <- 8.5	#J/mol K   biochar cp
  bc_cp <- bc_cp/12	#MJ/Mg K
  bo_hvap <- 1.22	#GJ / Mg   Bio-oil delta-H vap
  bo_cp <- 0.002	#GJ / Mg K   Bio-oil cp
  g_cp <- 30	#J/mol K  Syngas cp
  g_cp <- g_cp/15	#MJ / Mg K

  py_wall_heat_loss <- py_heat_loss_rate/(daf_feed_rate*1000/3600)	#GJ/ Mg f  heat loss from pyrolysis walls
  py_bc_heat_loss = bc_cp*(val(py_temp)-20)*bc_y/1000	#GJ/ Mg f   heat loss from biochar
  py_bo_heat_loss = bo_y*(bo_hvap+(py_ex_t-20)*bo_cp)	#GJ/ Mg f   heat loss from biooil
  py_g_heat_loss = g_cp*(py_ex_t-20)*g_y/1000	#GJ/ Mg f   heat loss from gas
  py_h2o_heat_loss = (2.676 + 0.0021*(py_ex_t-100)) * py_tot_h2o	#GJ/ Mg f   heat loss from h2o
  py_heat_loss = sum(py_wall_heat_loss, py_bc_heat_loss, py_bo_heat_loss, py_g_heat_loss, py_h2o_heat_loss)	#GJ/ Mg f   Total heat loss

  bc_hhv = 100*(0.3491*bc_c+1.1783*bc_h-0.1034*bc_o)	#GJ / Mg  biochar HHV
  bo_hhv = 100*(0.3491*bo_c+1.1783*bo_h-0.1034*bo_o)	#GJ / Mg  Bio-oil HHV
  g_hhv = (g_h2_y*141.8 + g_ch4_y*55.5 + g_co_y*10.1 + g_c2h4_y * 50.33) / (g_y)	#GJ / Mg syngas HHV
  py_E_g = g_hhv*g_y	#GJ/Mg f  combustion H in gas
  py_E_bo = bo_hhv*bo_y	#GJ/Mg f  combustion H in bioil
  py_E_bc = bc_hhv*bc_y	#GJ/Mg f  combustion H in biochar
  py_E_fuel = py_E_bo+py_E_g	#GJ/Mg f  combustion H in fuel (gas + volatiles)

  py_h_required = py_heat_loss+py_E_g+py_E_bo+py_E_bc - bm_lhv	#GJ/Mg f  Heat supply to pyrolysis
  py_h_eff <- 0.8  # heater efficiency
  py_h_fuel_required = py_h_required/py_h_eff	#GJ/Mg f  heat fuel required
  parasitic_power	= 0.07	#GJ / Mg f    NB add a further 0.53 GJ/Mg for comminution if woody feedstock used
  parasitic_fuel = parasitic_power/val(py_elec_eff)	#GJ / Mg f  Parasitic fuel required
  py_tot_fuel_required = py_h_fuel_required+parasitic_fuel	#GJ / Mg f

  py_net_fuel = c(py_E_fuel-py_tot_fuel_required,
                  py_E_fuel/(1 +  py_tot_fuel_required/bm_lhv),
                  py_E_fuel)[val(py_e_source)]	#GJ/Mg f  net comb H in biofuel after energy supply	=
  py_net_bc =   c(bc_y,
                  bc_y/(1 +  py_tot_fuel_required/bm_lhv),
                  bc_y * (1 - py_tot_fuel_required/py_E_bc) )[val(py_e_source)]	#Mg / Mg f  net biochar yield after heat supply

  py_elec_prod = py_net_fuel * val(bes_energy_efficiency)	#GJe / Mg f  electricity output per Mg biomass
  bebcs_FF.C.offset	= py_elec_prod * val(ffe_c_intensity) * val(rebound)	#Mg C / Mg f

  #bc in soil
  bc_oc_field = max(c(0.6-0.00079*val(py_temp), 0))	#	O:C molar (field)
  bc_rec_thalf = 10^(val(bc_stab_factor)*(1-bc_oc_field))	#yr	recalcitrant half life
  bc_lab_f = 0.1*exp((350-val(py_temp))*0.006)		#labile fraction
  bc_remain = exp(-time_frame*(log(2)/bc_rec_thalf))-bc_lab_f		#="bc frac remaining after " & time_frame & " years"

  bc_SOC = val(priming_factor)*py_net_bc	#Mg SOC / Mg f	npSOC feedback

  n2o_nobc = val(n_app_rate)*0.0125	#kg N/ha/yr	unamended N2O-N emissions
  n2o_bc = n2o_nobc*(1-val(n2o_factor))	#kg N/ha/yr	amended N2O-N emissions
  n2o_reduction = n2o_nobc-n2o_bc	#kg N/ha/yr	reduced N2O-N emissions
  n2o_reduction_bc = n2o_reduction	#(kg N/ yr) / Mg bc 	reduced N2O-N emissions
  n2o_reduction_bm = n2o_reduction_bc*py_net_bc	#(kg N/ yr) / Mg f	reduced N2O-N emissions
  n2o_reduction_bm_cum = n2o_reduction_bm*val(n2o_years)	#kg N/ Mg f 	reduced N2O-N emissions
  n2o_Ce_reduction_bm_cum = n2o_reduction_bm_cum/1000 * 44/28 *gwp_n2o * 12/44	#Mg Ce / Mg f	N2O feedback

  #bc crop impact
  bc_crop_value_annual = val(bc_yield_value)*val(crop_price_increase)	#$ / yr / Mg bc	annual value of crop yield increase
  bc_crop_value = bc_crop_value_annual/(val(discount)+(log(2)/bc_rec_thalf))	#$ / Mg bc	value of crop yield increment

  #bc fertilisation
  bc_ash = val(bm_ash) / (val(bm_ash) + bc_y)
  bc_cce = (5.378+1.582*(bc_ash* bm_base/bm_ash)-0.2136*bc_ash) / 100		#calcium carbonate equivalence
  bc_lime_value = bc_cce * val(aglime_price)	#$/Mg	liming economic value
  bc_nutrient_value_Mgbc = val(bc_nutrient_value) / bc_y	#$/ Mg bc	nutrient value
  bc_price = bc_crop_value + bc_lime_value + bc_nutrient_value_Mgbc	#$ / Mg bc	biochar NPV

  bebcs_soil_feedback = n2o_Ce_reduction_bm_cum + bc_SOC	#Mg Ce / Mg f	Soil effects GHG abatement
  bebcs_direct_ghg = bebcs_soil_feedback + bc_remain*py_net_bc	#Mg Ce / Mg f	Direct GHG abatement

  bebcs_tot_c_abatement	= n2o_Ce_reduction_bm_cum + bc_SOC + bc_remain*py_net_bc + bebcs_FF.C.offset - val(bc_haul_c)	#Mg C / Mg f

  #Biochar field operations
  bc_fieldcost_Mg = val(bc_field_cost) / val(bc_app_rate)  #$/Mg bc
  bc_fieldcost_Mgbm = bc_fieldcost_Mg*bc_y  #$/Mg f

  #Costs
  py_cc = val(bebcs_cc)	#$/(Mg f/yr)	capital cost of pyrolysis
  py_life = val(bebcs_life_ratio)*val(bes_life)	#yr	life time of plant
  py_annuity = (1 - (1 / ((1+val(discount))^py_life))) / val(discount)	#	annuity factor
  bebcs_annual_cc = py_cc/py_annuity	#$/Mg f	annualised capital
  bebcs_om = py_cc*O_M_factor	#$/Mg f	operation and maintenance
  bebcs_tf = val(bc_haul_cost) + bc_fieldcost_Mgbm	#$/Mg f	transport & field operations
  bebcs_total_cost = sum(beccs_annual_cc, bebcs_om, bebcs_tf, bm_cost, na.rm = T) #$/ Mg f	total costs

  #Revenues
  bebcs_c_credits = bebcs_tot_c_abatement*val(c_price)	#$ / Mg f	C credits
  bc_revenue = bc_y  * bc_price	#$ / Mg f	biochar revenue
  bebcs_elec_revenue = py_elec_prod*val(elec_price)	#$ / Mg f	elec revenue
  bebcs_total_revenue = bebcs_elec_revenue + bc_revenue + beccs_c_credits	#$ / Mg f	total revenue

  ##############################
  # Cost-benefit
  ##############################
  bes_net_value	= bes_total_revenue - bes_total_costs
  bebcs_net_value = bebcs_total_revenue - bebcs_total_cost	#$ / Mg f	Net value
  beccs_net_value <- ifelse(beccs_available, beccs_total_revenue - bes_total_costs, NA)	#$ / Mg f

  bes_opportunity_cost = max(bebcs_net_value, beccs_net_value, na.rm = T)
  beccs_opportunity_cost = max(bebcs_net_value, bes_net_value, na.rm = T)
  bebcs_opportunity_cost = max(beccs_net_value, bes_net_value, na.rm = T)	#$/ Mg f

  bes_rpv	= bes_net_value - bes_opportunity_cost # $/ Mg f
  beccs_rpv	= ifelse(beccs_available, beccs_net_value - beccs_opportunity_cost, NA)	# $/ Mg f
  bebcs_rpv = bebcs_net_value - bebcs_opportunity_cost	#$/ Mg f	OC-adjusted NV (RPV)
  return(data.table(BES_NPV=bes_net_value,
                    BECCS_NPV=beccs_net_value,
                    BEBCS_NPV=bebcs_net_value,
                    BES=bes_rpv,
                    BECCS=beccs_rpv,
                    BEBCS=bebcs_rpv))
}

####################################################
# Read scenario parameter ranges and distributions
####################################################
scenarios <- fread("scenarios.csv")
scenario.names <- c("op_space", "2014", "2020_RCP430", "2050_RCP430", "2100_RCP430", "2020_RCP530", "2050_RCP530",	"2100_RCP530", "2020_RCP650", "2050_RCP650", "2100_RCP650")
scenario.cnt <- 1
this.scenario <-  scenarios[scenario==scenario.names[scenario.cnt]]
randomise <- function(p.min, p.max, p.mean, p.sd, distribution) {
  switch(distribution,
         Uniform = runif(1, p.min, p.max),
         Normal  = rnorm(1, p.mean, p.sd),
         Log = exp(runif(1, log(p.min), log(p.max))))
}
set.mean <- function(p.min, p.max, p.mean, distribution) {
  if (distribution == "Normal") return(p.mean) else return(mean(c(p.min, p.max)))
}
this.scenario[, val := randomise(p.min, p.max, p.mean, p.sd, distribution), by = 1:NROW(this.scenario)]
this.scenario[, val := set.mean(p.min, p.max, p.mean, distribution), by = 1:NROW(this.scenario)]
# val = function(param) {param = as.character(substitute(param)); this.scenario[name==param, val][]}

###########################
# Monte Carlo
###########################
rpvs = rpv(this.scenario[, val := randomise(p.min, p.max, p.mean, p.sd, distribution), by = 1:NROW(this.scenario)])
params = data.table(t(this.scenario[,val]))
names(params) = this.scenario[,name]
result.row = cbind(rpvs, params)
# results = rbind(result.row, data.table(BES = rep(as.numeric(NA),monte_n-1)), fill=T)
results = result.row

monte_n <- 80000   #number of monte carlo simulations
for (i in 2:monte_n) {
  rpvs = rpv(this.scenario[, val := randomise(p.min, p.max, p.mean, p.sd, distribution), by = 1:NROW(this.scenario)])
  params = data.table(t(this.scenario[,val]))
  names(params) = this.scenario[,name]
  result.row = cbind(rpvs, params)
  results = rbind(results, result.row)
}

write.table(results, "opspace_beccs_test.txt", sep=",", row.names = F)

