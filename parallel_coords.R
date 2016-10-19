<<<<<<< HEAD
################################################################
# parallel coordinates
################################################################
set.seed(1234)
nlines <- 750
scenario_files <- c("2020_RCP650_beccs.txt", "2050_RCP650_beccs.txt", "2100_RCP650_beccs.txt",
                    "2020_RCP430_beccs.txt", "2050_RCP430_beccs.txt", "2100_RCP430_beccs.txt")

pcres = data.table()
for (file_name in scenario_files) {
  pcres <- rbind(pcres, fread(paste(getwd(), file_name, sep='/'))[sample(.N, nlines),])
  }

#simplify figure to see main parameters
colsToDelete = c("Scenario","BES_NPV","BECCS_NPV","BEBCS_NPV", "BEBCS_soil_GHG","rebound","ff_eff",
                 "bes_life","py_e_source", "bebcstobe_life_ratio","soc", "bc_price", "bes_eff",
                 "soc_factor", "c_intensity", "bebcs_cc", "bes_cc" , "bc_haul_cost", "bc_field_cost" ,
                 "py_temp", "beccs_seq_fraction", "beccs_eff_penalty", "n_app_rate", "n2o_years",
                 "n2o_factor", "bc_haul_co2" , "py_elec_eff" , "bc_nutrient", "app_rate", "lime_price", "bm_ash")

colsOrder <- c("BES" ,"BEBCS","BECCS",
               "c_price", "elec_price", "bc_yield_impact", "disc_rate",
               "bc_stab_fact" , "crop_price_increase","ccs_cost")

pcres <- pcres[, (colsToDelete):=NULL]
setcolorder(pcres, colsOrder)
pcres <- pcres[sample(.N),]

pcalpha <- 1:pcres[,.N]
pcalpha[which(pcres[, BEBCS > 0])] <- pcres[BEBCS > 0, (BEBCS/max(BEBCS))^3]
pcalpha[which(pcres[, BECCS > 0])] <- pcres[BECCS > 0, (BECCS/max(BECCS))^3]
pcalpha[which(pcres[, BES > 0])] <- pcres[BES > 0, (BES/max(BES))^3]

pc_col <- pcalpha
pc_col[which(pcres$BECCS >0)] <- alpha(col1.beccs, pcalpha[which(pcres$BECCS >0)])
pc_col[which(pcres$BEBCS >0)] <- alpha(col1.bebcs, pcalpha[which(pcres$BEBCS >0)])
pc_col[which(pcres$BES >0)]   <- alpha(col1.bes,   pcalpha[which(pcres$BES >0)])

param.label <- names(pcres)
for (i in 1:NROW(names(pcres))){
  param.label[i]  <- param.to.label(names(pcres)[i])
}
param.label[length(param.label)] <- "CCS cost"

pdf("pcoord.pdf", width=3.5, height=3)
par(col.lab="white", bg="white", col.axis="white", mar=c(5.5, 3.5, 2, 0), cex=0.9, ps=10, tcl=-0.2)
parcoord(pcres, col = pc_col, lty = 1,  var.label = F, las=3 )
text(1:ncol(pcres), -0.09, labels = param.label, srt = 40, adj = c(1,1), xpd = TRUE, cex=0.9)
axis(2, at=c(0,0.5,1),col.lab="black", xpd=T, tcl=-0.2)
text(0.3, 0:1, labels = c("Min","Max"),  cex=0.9, xpd=T, srt=90)
mtext("Parameter value",2,line=2.5)
mtext("(normalised to parameter range)",2,line=1.5)
legend(1.3,1.23,  c("BES","BEBCS","BECCS"), cex=1, lty=c(1,1,1), lwd=c(1.4,1.4,1.4),
       pch = c(NA, NA, NA), bty = "n", horiz=T, xpd=T, seg.len=1.3,
       x.intersp = 0.2, text.width = 2,
       col=c(col1.bes, col1.bebcs, col1.beccs))

dev.off()


=======
################################################################
# parallel coordinates
################################################################
set.seed(1234)
nlines <- 750
scenario_files <- c("2020_RCP650_beccs.txt", "2050_RCP650_beccs.txt", "2100_RCP650_beccs.txt",
                    "2020_RCP430_beccs.txt", "2050_RCP430_beccs.txt", "2100_RCP430_beccs.txt")

pcres = data.table()
for (file_name in scenario_files) {
  pcres <- rbind(pcres, fread(paste(getwd(), file_name, sep='/'))[sample(.N, nlines),])
  }

#simplify figure to see main parameters
colsToDelete = c("Scenario","BES_NPV","BECCS_NPV","BEBCS_NPV", "BEBCS_soil_GHG","rebound","ff_eff",
                 "bes_life","py_e_source", "bebcstobe_life_ratio","soc", "bc_price", "bes_eff",
                 "soc_factor", "c_intensity", "bebcs_cc", "bes_cc" , "bc_haul_cost", "bc_field_cost" ,
                 "py_temp", "beccs_seq_fraction", "beccs_eff_penalty", "n_app_rate", "n2o_years",
                 "n2o_factor", "bc_haul_co2" , "py_elec_eff" , "bc_nutrient", "app_rate", "lime_price", "bm_ash")

colsOrder <- c("BES" ,"BEBCS","BECCS",
               "c_price", "elec_price", "bc_yield_impact", "disc_rate",
               "bc_stab_fact" , "crop_price_increase","ccs_cost")

pcres <- pcres[, (colsToDelete):=NULL]
setcolorder(pcres, colsOrder)
pcres <- pcres[sample(.N),]

pcalpha <- 1:pcres[,.N]
pcalpha[which(pcres[, BEBCS > 0])] <- pcres[BEBCS > 0, (BEBCS/max(BEBCS))^3]
pcalpha[which(pcres[, BECCS > 0])] <- pcres[BECCS > 0, (BECCS/max(BECCS))^3]
pcalpha[which(pcres[, BES > 0])] <- pcres[BES > 0, (BES/max(BES))^3]

pc_col <- pcalpha
pc_col[which(pcres$BECCS >0)] <- alpha(col1.beccs, pcalpha[which(pcres$BECCS >0)])
pc_col[which(pcres$BEBCS >0)] <- alpha(col1.bebcs, pcalpha[which(pcres$BEBCS >0)])
pc_col[which(pcres$BES >0)]   <- alpha(col1.bes,   pcalpha[which(pcres$BES >0)])

param.label <- names(pcres)
for (i in 1:NROW(names(pcres))){
  param.label[i]  <- param.to.label(names(pcres)[i])
}
param.label[length(param.label)] <- "CCS cost"

pdf("pcoord.pdf", width=3.5, height=3)
par(col.lab="white", bg="white", col.axis="white", mar=c(5.5, 3.5, 2, 0), cex=0.9, ps=10, tcl=-0.2)
parcoord(pcres, col = pc_col, lty = 1,  var.label = F, las=3 )
text(1:ncol(pcres), -0.09, labels = param.label, srt = 40, adj = c(1,1), xpd = TRUE, cex=0.9)
axis(2, at=c(0,0.5,1),col.lab="black", xpd=T, tcl=-0.2)
text(0.3, 0:1, labels = c("Min","Max"),  cex=0.9, xpd=T, srt=90)
mtext("Parameter value",2,line=2.5)
mtext("(normalised to parameter range)",2,line=1.5)
legend(1.3,1.23,  c("BES","BEBCS","BECCS"), cex=1, lty=c(1,1,1), lwd=c(1.4,1.4,1.4),
       pch = c(NA, NA, NA), bty = "n", horiz=T, xpd=T, seg.len=1.3,
       x.intersp = 0.2, text.width = 2,
       col=c(col1.bes, col1.bebcs, col1.beccs))

dev.off()


>>>>>>> f22ddffdc4b9000d81fcf21ee02fac3fe2f46fb0
