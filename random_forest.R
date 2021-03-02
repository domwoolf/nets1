BEBCSfit <- randomForest(BEBCS ~
                           disc_rate + bm_ash + crop_price_increase +
                           lime_price + c_price + elec_price +
                           c_intensity + ff_eff+bes_cc + bes_life + bes_eff +
                           py_temp + py_elec_eff + bebcs_cc + bc_haul_cost +
                           n_app_rate + n2o_factor + n2o_years + soc_factor + bc_haul_co2 +
                           soc + bc_yield_impact + app_rate + bc_stab_fact +
                           ccs_cost + beccs_eff_penalty + beccs_seq_fraction,
                         data=all_data[sample(Forest_data_length),],
                         ntree=1501, proximity=TRUE, importance=TRUE, keep.forest=FALSE)
BEBCSimp <- importance(BEBCSfit)

BECCSfit <- randomForest(BECCS ~
                           disc_rate + bm_ash + crop_price_increase +
                           lime_price + c_price + elec_price +
                           c_intensity + ff_eff+bes_cc + bes_life + bes_eff +
                           py_temp + py_elec_eff + bebcs_cc + bc_haul_cost +
                           n_app_rate + n2o_factor + n2o_years + soc_factor + bc_haul_co2 +
                           soc + bc_yield_impact + app_rate + bc_stab_fact +
                           ccs_cost + beccs_eff_penalty + beccs_seq_fraction,
                         data=all_data[sample(Forest_data_length),],
                         ntree=1501, proximity=TRUE, importance=TRUE, keep.forest=FALSE)
BECCSimp <- importance(BECCSfit)

BESfit <- randomForest(BES ~
                         disc_rate + bm_ash + crop_price_increase +
                         lime_price + c_price + elec_price +
                         c_intensity + ff_eff+bes_cc + bes_life + bes_eff +
                         py_temp + py_elec_eff + bebcs_cc + bc_haul_cost +
                         n_app_rate + n2o_factor + n2o_years + soc_factor + bc_haul_co2 +
                         soc + bc_yield_impact + app_rate + bc_stab_fact +
                         ccs_cost + beccs_eff_penalty + beccs_seq_fraction,
                       data=all_data[sample(Forest_data_length),],
                       ntree=1501, proximity=TRUE, importance=TRUE, keep.forest=FALSE)

BESimp <- importance(BESfit)

###############################################
#### parameter importance plot
###############################################
NvarToPLot <- 10
xmax <- max(BEBCSimp[,1],BECCSimp[,1],BESimp[,1])
impord = order(BEBCSimp[, 1], decreasing=FALSE)
impord <- impord[(length(impord)-NvarToPLot+1):(length(impord))]
good_labels <- improve_labels(row.names(importance(BEBCSfit)))
for (i in 1:NROW(row.names(importance(BEBCSfit)))){
  good_labels[i]  <- param.to.label(row.names(importance(BEBCSfit))[i])
}

#varImpPlot(BEBCSfit, type=1, main="", labels=good_labels[impord], n.var=10)
par(ps=12)
dotchart(BEBCSimp[impord,1],
         xlim=c(0,max(BEBCSimp[,1],BECCSimp[,1],BESimp[,1])),
         labels=good_labels[impord],
         col="white", pch=16, cex=1.5, las=2)
axis(side = 2, seq_along(BEBCSimp[impord,1]), good_labels[impord], las=1, cex.axis=1.2, padj=0.4, lwd=0, lwd.ticks = 0, line=-7.3)
points(1:NvarToPLot ~ BEBCSimp[impord,1],col=col1.bebcs,pch=16)
points(1:NvarToPLot ~ BECCSimp[impord,1],col=col1.beccs,pch=0)
points(1:NvarToPLot ~ BESimp[impord,1],col=col1.bes,pch=6)


if (saveplots) {
  pdf("fig2.pdf",  width=3.5, height=3)
  par(las=1, mgp=c(2,0,0), omi=c(0.5,0,0,0)); par(mar=c(0,1,2,1)); par(ps=8); par(lwd=0.5); par(tcl=-0.1)
  dotchart(BEBCSimp[impord,1],xlim=c(0,xmax), labels=good_labels[impord],
           main="", yaxt="n", xaxt="n", pch=16, cex=1, col="white")
  axis(side = 2, seq_along(BEBCSimp[impord,1]), good_labels[impord],
       las=1, cex.axis=1, padj=0.4, lwd=0, lwd.ticks = 0, line=-7.6)
  points(1:NvarToPLot ~ BEBCSimp[impord,1], col=col1.bebcs, pch=16)
  points(1:NvarToPLot ~ BECCSimp[impord,1], pch=0, cex=1, col=col1.beccs)
  points(1:NvarToPLot ~ BESimp[impord,1], pch=6, cex=1, col=col1.bes)
  mtext(text="Parameter Importance\n(%IMSE)", las=1,side=1,outer = FALSE, at=xmax/2, line=1.5, cex=1)
  legend(-10, 12.6,  c("BEBCS","BECCS","BES"), cex=1, lty=c(NA,NA,NA), lwd=c(0.5,0.5,0.5), pch = c(16, 0, 6),
         bty = "n", ncol=3, xpd=TRUE, text.width=190, seg.len=0, col=c(col1.bebcs,col1.beccs,col1.bes))
  dev.off()
}