<<<<<<< HEAD
#######################################################################
#  parallel box plots normalised to mean and standard deviation (msd)
#  simple versions with only most important parameters as category labels
#######################################################################

results_430_2020 <- fread(paste(getwd(), "2020_RCP430_beccs.txt", sep='/'))
results_430_2050 <- fread(paste(getwd(), "2050_RCP430_beccs.txt", sep='/'))
results_430_2100 <- fread(paste(getwd(), "2100_RCP430_beccs.txt", sep='/'))
results_650_2020 <- fread(paste(getwd(), "2020_RCP650_beccs.txt", sep='/'))
results_650_2050 <- fread(paste(getwd(), "2050_RCP650_beccs.txt", sep='/'))
results_650_2100 <- fread(paste(getwd(), "2100_RCP650_beccs.txt", sep='/'))

important_params <- c("BES", "BECCS", "BEBCS", "bc_yield_impact", "disc_rate", "elec_price", "bc_stab_fact", "c_intensity",
                      "c_price", "bes_eff", "bes_cc", "bebcs_cc","ccs_cost", "beccs_seq_fraction", "beccs_eff_penalty")


pdf("pbox.pdf",  width=7.2, height=7)

par(mfrow=c(3,2))
par(mar=c(0,0,0,0))
par(oma=c(8,4,4,2))
par(xpd=T)
par(mgp=c(0,0.5,0))
par(las=2)
par(lwd=0.2)
par(ps=12)

##########################################
# RCP 430 BES
##########################################
box_norm <- scale(results_430_2020[,important_params,with=F], center = TRUE, scale = TRUE)
good_labels <- improve_labels(colnames(box_norm))
for (i in 1:NROW(colnames(box_norm))){
  good_labels[i]  <- param.to.label(colnames(box_norm)[i])
}

b0 <- boxplot(box_norm, plot=F)
bxp(b0, at=1:ncol(box_norm)*2-1,
    xlim = c(1, ncol(box_norm)*2-1), ylim = c(-2.1,2.1),
    xaxt = "n", boxwex=1.6, outline=F,
    boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
    main="",ylab="")
grid(nx=15, ny=NA, lwd=0.5, lty = "dashed", col="darkgray")
abline(h=0, col="darkgray", lty = "dashed", lwd = 0.5)
abline(v=6, col="darkgray", lty = "solid", lwd = 2)
box_subset <- box_norm[which(results_430_2020$BES > 0),]
if (NROW(box_subset)>0) {
  b1 <- boxplot(box_subset, plot=F)
  b1$stats[3,] <- colMeans(box_subset)
  b1$stats[2,] <- b1$stats[3,]- apply(box_subset, 2, sd)
  b1$stats[4,] <- b1$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b1, at=1:ncol(box_norm)*2-1.4, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2020, boxwex=0.25, whisklty = 0,
      border=col1.2020, boxwex=0.5, outline=F, staplelty = 0)
}
box_norm <- scale(results_430_2050[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_430_2050$BES > 0),]
if (NROW(box_subset)>0) {
  b2 <- boxplot(box_subset, plot=F)
  b2$stats[3,] <- colMeans(box_subset)
  b2$stats[2,] <- b2$stats[3,]- apply(box_subset, 2, sd)
  b2$stats[4,] <- b2$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b2, at=1:ncol(box_norm)*2-1, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2050, boxwex=0.25, whisklty = 0,
      border=col1.2050, boxwex=0.5, outline=F, alpha=0.4,  staplelty = 0)
}
box_norm <- scale(results_430_2100[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_430_2100$BES > 0),]
if (NROW(box_subset)>0) {
  b3 <- boxplot(box_subset, plot=F)
  b3$stats[3,] <- colMeans(box_subset)
  b3$stats[2,] <- b3$stats[3,]- apply(box_subset, 2, sd)
  b3$stats[4,] <- b3$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b3, at=1:ncol(box_norm)*2-0.6, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2100, boxwex=0.25, whisklty = 0,
      border=col1.2100, boxwex=0.5, outline=F, alpha=0.2, staplelty = 0)
}
text(0.6,1.98,"a", cex=1.6, font=2)
axis(1, at=1:ncol(box_norm)*2-1, labels = vector(mode="character", length=length(good_labels)), tick = TRUE, tcl=-0.2)


##########################################
# RCP 650 BES
##########################################
box_norm <- scale(results_650_2020[,important_params,with=F], center = TRUE, scale = TRUE)
b0 <- boxplot(box_norm, plot=F)
bxp(b0, at=1:ncol(box_norm)*2-1,
    xlim = c(1, ncol(box_norm)*2-1), ylim = c(-2.1,2.1),
    xaxt = "n", yaxt = "n", boxwex=1.6, outline=F,
    boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
    main="",ylab="")
grid(nx=15, ny=NA, lwd=0.5, lty = "dashed", col="darkgray")
abline(h=0, col="darkgray", lty = "dashed", lwd = 0.5)
abline(v=6, col="darkgray", lty = "solid", lwd = 2)
box_subset <- box_norm[which(results_650_2020$BES > 0),]
if (NROW(box_subset)>0) {
  b1 <- boxplot(box_subset, plot=F)
  b1$stats[3,] <- colMeans(box_subset)
  b1$stats[2,] <- b1$stats[3,]- apply(box_subset, 2, sd)
  b1$stats[4,] <- b1$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b1, at=1:ncol(box_norm)*2-1.4, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2020, boxwex=0.25, whisklty = 0,
      border=col1.2020, boxwex=0.5, outline=F, staplelty = 0, boxlwd=0.1)
}
box_norm <- scale(results_650_2050[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_650_2050$BES > 0),]
if (NROW(box_subset)>0) {
  b2 <- boxplot(box_subset, plot=F)
  b2$stats[3,] <- colMeans(box_subset)
  b2$stats[2,] <- b2$stats[3,]- apply(box_subset, 2, sd)
  b2$stats[4,] <- b2$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b2, at=1:ncol(box_norm)*2-1, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2050, boxwex=0.25, whisklty = 0,
      border=col1.2050, boxwex=0.5, outline=F, alpha=0.4,  staplelty = 0, boxlwd=0.1)
}
box_norm <- scale(results_650_2100[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_650_2100$BES > 0),]
if (NROW(box_subset)>0) {
  b3 <- boxplot(box_subset, plot=F)
  b3$stats[3,] <- colMeans(box_subset)
  b3$stats[2,] <- b3$stats[3,]- apply(box_subset, 2, sd)
  b3$stats[4,] <- b3$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b3, at=1:ncol(box_norm)*2-0.6, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2100, boxwex=0.25, whisklty = 0,
      border=col1.2100, boxwex=0.5, outline=F, alpha=0.2, staplelty = 0, boxlwd=0.1)
}
text(0.6,1.98,"d", cex=1.6, font=2)
axis(1, at=1:ncol(box_norm)*2-1, labels = vector(mode="character", length=length(good_labels)), tick = TRUE, tcl=-0.2)


##########################################
# RCP 430 BEBCS
##########################################
box_norm <- scale(results_430_2020[,important_params,with=F], center = TRUE, scale = TRUE)
good_labels <- improve_labels(colnames(box_norm))
for (i in 1:NROW(colnames(box_norm))){
  good_labels[i]  <- param.to.label(colnames(box_norm)[i])
}
b0 <- boxplot(box_norm, plot=F)
bxp(b0, at=1:ncol(box_norm)*2-1,
    xlim = c(1, ncol(box_norm)*2-1), ylim = c(-2.1,2.1),
    xaxt = "n", boxwex=1.6, outline=F,
    boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
    main="",ylab="",tck=-.001)
grid(nx=15, ny=NA, lwd=0.5, lty = "dashed", col="darkgray")
abline(h=0, col="darkgray", lty = "dashed", lwd = 0.5)
abline(v=6, col="darkgray", lty = "solid", lwd = 2)
box_subset <- box_norm[which(results_430_2020$BEBCS > 0),]
if (NROW(box_subset)>0) {
  b1 <- boxplot(box_subset, plot=F)
  b1$stats[3,] <- colMeans(box_subset)
  b1$stats[2,] <- b1$stats[3,]- apply(box_subset, 2, sd)
  b1$stats[4,] <- b1$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b1, at=1:ncol(box_norm)*2-1.4, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2020, boxwex=0.25, whisklty = 0,
      border=col1.2020, boxwex=0.5, outline=F, staplelty = 0)
}
box_norm <- scale(results_430_2050[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_430_2050$BEBCS > 0),]
if (NROW(box_subset)>0) {
  b2 <- boxplot(box_subset, plot=F)
  b2$stats[3,] <- colMeans(box_subset)
  b2$stats[2,] <- b2$stats[3,]- apply(box_subset, 2, sd)
  b2$stats[4,] <- b2$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b2, at=1:ncol(box_norm)*2-1, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2050, boxwex=0.25, whisklty = 0,
      border=col1.2050, boxwex=0.5, outline=F, alpha=0.4,  staplelty = 0)
}
box_norm <- scale(results_430_2100[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_430_2100$BEBCS > 0),]
if (NROW(box_subset)>0) {
  b3 <- boxplot(box_subset, plot=F)
  b3$stats[3,] <- colMeans(box_subset)
  b3$stats[2,] <- b3$stats[3,]- apply(box_subset, 2, sd)
  b3$stats[4,] <- b3$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b3, at=1:ncol(box_norm)*2-0.6, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2100, boxwex=0.25, whisklty = 0,
      border=col1.2100, boxwex=0.5, outline=F, alpha=0.2, staplelty = 0)
}
text(0.6,1.98,"b", cex=1.6, font=2)
axis(1, at=1:ncol(box_norm)*2-1, labels = vector(mode="character", length=length(good_labels)), tick = TRUE, tcl=-0.2)


##########################################
# RCP 650 BEBCS
##########################################
box_norm <- scale(results_650_2020[,important_params,with=F], center = TRUE, scale = TRUE)
good_labels <- improve_labels(colnames(box_norm))
for (i in 1:NROW(colnames(box_norm))){
  good_labels[i]  <- param.to.label(colnames(box_norm)[i])
}
b0 <- boxplot(box_norm, plot=F)
bxp(b0, at=1:ncol(box_norm)*2-1,
    xlim = c(1, ncol(box_norm)*2-1), ylim = c(-2.1,2.1),
    xaxt = "n", yaxt = "n", boxwex=1.6, outline=F,
    boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
    main="",ylab="")
grid(nx=15, ny=NA, lwd=0.5, lty = "dashed", col="darkgray")
abline(h=0, col="darkgray", lty = "dashed", lwd = 0.5)
abline(v=6, col="darkgray", lty = "solid", lwd = 2)
box_subset <- box_norm[which(results_650_2020$BEBCS > 0),]
if (NROW(box_subset)>0) {
  b1 <- boxplot(box_subset, plot=F)
  b1$stats[3,] <- colMeans(box_subset)
  b1$stats[2,] <- b1$stats[3,]- apply(box_subset, 2, sd)
  b1$stats[4,] <- b1$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b1, at=1:ncol(box_norm)*2-1.4, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2020, boxwex=0.25, whisklty = 0,
      border=col1.2020, boxwex=0.5, outline=F, staplelty = 0)
}
box_norm <- scale(results_650_2050[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_650_2050$BEBCS > 0),]
if (NROW(box_subset)>0) {
  b2 <- boxplot(box_subset, plot=F)
  b2$stats[3,] <- colMeans(box_subset)
  b2$stats[2,] <- b2$stats[3,]- apply(box_subset, 2, sd)
  b2$stats[4,] <- b2$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b2, at=1:ncol(box_norm)*2-1, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2050, boxwex=0.25, whisklty = 0,
      border=col1.2050, boxwex=0.5, outline=F, alpha=0.4,  staplelty = 0)
}
box_norm <- scale(results_650_2100[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_650_2100$BEBCS > 0),]
if (NROW(box_subset)>0) {
  b3 <- boxplot(box_subset, plot=F)
  b3$stats[3,] <- colMeans(box_subset)
  b3$stats[2,] <- b3$stats[3,]- apply(box_subset, 2, sd)
  b3$stats[4,] <- b3$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b3, at=1:ncol(box_norm)*2-0.6, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2100, boxwex=0.25, whisklty = 0,
      border=col1.2100, boxwex=0.5, outline=F, alpha=0.2, staplelty = 0)
}
text(0.6,1.98,"e", cex=1.6, font=2)
axis(1, at=1:ncol(box_norm)*2-1, labels = vector(mode="character", length=length(good_labels)), tick = TRUE, tcl=-0.2)

##########################################
# RCP 430 BECCS
##########################################
box_norm <- scale(results_430_2020[,important_params,with=F], center = TRUE, scale = TRUE)
b0 <- boxplot(box_norm, plot=F)
bxp(b0, at=1:ncol(box_norm)*2-1,
    xlim = c(1, ncol(box_norm)*2-1), ylim = c(-2.1,2.1),
    xaxt = "n", boxwex=1.6, outline=F,
    boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
    main="",ylab="")
grid(nx=15, ny=NA, lwd=0.5, lty = "dashed", col="darkgray")
abline(h=0, col="darkgray", lty = "dashed", lwd = 0.5)
abline(v=6, col="darkgray", lty = "solid", lwd = 2)
box_subset <- box_norm[which(results_430_2020$BECCS > 0),]
if (NROW(box_subset)>0) {
  b1 <- boxplot(box_subset, plot=F)
  b1$stats[3,] <- colMeans(box_subset)
  b1$stats[2,] <- b1$stats[3,]- apply(box_subset, 2, sd)
  b1$stats[4,] <- b1$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b1, at=1:ncol(box_norm)*2-1.4, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2020, boxwex=0.25, whisklty = 0,
      border=col1.2020, boxwex=0.5, outline=F, staplelty = 0)
}
box_norm <- scale(results_430_2050[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_430_2050$BECCS > 0),]
if (NROW(box_subset)>0) {
  b2 <- boxplot(box_subset, plot=F)
  b2$stats[3,] <- colMeans(box_subset)
  b2$stats[2,] <- b2$stats[3,]- apply(box_subset, 2, sd)
  b2$stats[4,] <- b2$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b2, at=1:ncol(box_norm)*2-1, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2050, boxwex=0.25, whisklty = 0,
      border=col1.2050, boxwex=0.5, outline=F, alpha=0.4,  staplelty = 0)
}
box_norm <- scale(results_430_2100[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_430_2100$BECCS > 0),]
if (NROW(box_subset)>0) {
  b3 <- boxplot(box_subset, plot=F)
  b3$stats[3,] <- colMeans(box_subset)
  b3$stats[2,] <- b3$stats[3,]- apply(box_subset, 2, sd)
  b3$stats[4,] <- b3$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b3, at=1:ncol(box_norm)*2-0.6, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2100, boxwex=0.25, whisklty = 0,
      border=col1.2100, boxwex=0.5, outline=F, alpha=0.2, staplelty = 0)
}
text(0.6,1.98,"c", cex=1.6, font=2)
axis(1, at=1:ncol(box_norm)*2-1, labels = vector(mode="character", length=length(good_labels)), tick = TRUE, tcl=-0.2)
text(1:ncol(box_norm)*2-0.7, -2.5, labels = good_labels, srt = 33, adj=c(1,0), xpd = NA, cex=1)


##########################################
# RCP 650 BECCS
##########################################
box_norm <- scale(results_650_2020[,important_params,with=F], center = TRUE, scale = TRUE)
b0 <- boxplot(box_norm, plot=F)
bxp(b0, at=1:ncol(box_norm)*2-1,
    xlim = c(1, ncol(box_norm)*2-1), ylim = c(-2.1,2.1),
    xaxt = "n", yaxt = "n", boxwex=1.6, outline=F,
    boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
    main="",ylab="")
grid(nx=15, ny=NA, lwd=0.5, lty = "dashed", col="darkgray")
abline(h=0, col="darkgray", lty = "dashed", lwd = 0.5)
abline(v=6, col="darkgray", lty = "solid", lwd = 2)
box_subset <- box_norm[which(results_650_2020$BECCS > 0),]
if (NROW(box_subset)>0) {
  b1 <- boxplot(box_subset, plot=F)
  b1$stats[3,] <- colMeans(box_subset)
  b1$stats[2,] <- b1$stats[3,]- apply(box_subset, 2, sd)
  b1$stats[4,] <- b1$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b1, at=1:ncol(box_norm)*2-1.4, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2020, boxwex=0.25, whisklty = 0,
      border=col1.2020, boxwex=0.5, outline=F, staplelty = 0)
}
box_norm <- scale(results_650_2050[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_650_2050$BECCS > 0),]
if (NROW(box_subset)>0) {
  b2 <- boxplot(box_subset, plot=F)
  b2$stats[3,] <- colMeans(box_subset)
  b2$stats[2,] <- b2$stats[3,]- apply(box_subset, 2, sd)
  b2$stats[4,] <- b2$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b2, at=1:ncol(box_norm)*2-1, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2050, boxwex=0.25, whisklty = 0,
      border=col1.2050, boxwex=0.5, outline=F, alpha=0.4,  staplelty = 0)
}
box_norm <- scale(results_650_2100[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_650_2100$BECCS > 0),]
if (NROW(box_subset)>0) {
  b3 <- boxplot(box_subset, plot=F)
  b3$stats[3,] <- colMeans(box_subset)
  b3$stats[2,] <- b3$stats[3,]- apply(box_subset, 2, sd)
  b3$stats[4,] <- b3$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b3, at=1:ncol(box_norm)*2-0.6, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2100, boxwex=0.25, whisklty = 0,
      border=col1.2100, boxwex=0.5, outline=F, alpha=0.2, staplelty = 0)
}
text(0.6,1.98,"f", cex=1.6, font=2)
axis(1, at=1:ncol(box_norm)*2-1, labels = vector(mode="character", length=length(good_labels)), tick = TRUE, tcl=-0.2)
text(1:ncol(box_norm)*2-0.7, -2.5, labels = good_labels, srt = 33, adj=c(1,0), xpd = NA, cex=1)

mtext("Standardized Value (standard deviations)", side=2, outer = TRUE, cex = 1, line = 2.2, las=0)
mtext("MS430-480", side=3, outer = TRUE, cex = 1, line = 2.2, las=0, at=0.25)
mtext("MS650-720", side=3, outer = TRUE, cex = 1, line = 2.2, las=0, at=0.75)
mtext("BES",   side=4, outer = TRUE, cex = 1, line = 1, las=0, at=0.82)
mtext("BEBCS", side=4, outer = TRUE, cex = 1, line = 1, las=0, at=0.5)
mtext("BECCS", side=4, outer = TRUE, cex = 1, line = 1, las=0, at=0.18)

rect(-11, 11.64, -7, 11.76, col = col2.2020, border = col1.2020, xpd=NA)
rect(-2,  11.64, 2,  11.76, col = col2.2050, border = col1.2050, xpd=NA)
rect(7,   11.64, 11, 11.76, col = col2.2100, border = col1.2100, xpd=NA)
mtext("2020", at=0.411, side=3, outer = TRUE, las=1, line = 0.63, cex=0.7)
mtext("2050", at=0.561, side=3, outer = TRUE, las=1, line = 0.63, cex=0.7)
mtext("2100", at=0.711, side=3, outer = TRUE, las=1, line = 0.63, cex=0.7)
dev.off()

=======
#######################################################################
#  parallel box plots normalised to mean and standard deviation (msd)
#  simple versions with only most important parameters as category labels
#######################################################################

results_430_2020 <- fread(paste(getwd(), "2020_RCP430_beccs.txt", sep='/'))
results_430_2050 <- fread(paste(getwd(), "2050_RCP430_beccs.txt", sep='/'))
results_430_2100 <- fread(paste(getwd(), "2100_RCP430_beccs.txt", sep='/'))
results_650_2020 <- fread(paste(getwd(), "2020_RCP650_beccs.txt", sep='/'))
results_650_2050 <- fread(paste(getwd(), "2050_RCP650_beccs.txt", sep='/'))
results_650_2100 <- fread(paste(getwd(), "2100_RCP650_beccs.txt", sep='/'))

important_params <- c("BES", "BECCS", "BEBCS", "bc_yield_impact", "disc_rate", "elec_price", "bc_stab_fact", "c_intensity",
                      "c_price", "bes_eff", "bes_cc", "bebcs_cc","ccs_cost", "beccs_seq_fraction", "beccs_eff_penalty")


pdf("pbox.pdf",  width=7.2, height=7)

par(mfrow=c(3,2))
par(mar=c(0,0,0,0))
par(oma=c(8,4,4,2))
par(xpd=T)
par(mgp=c(0,0.5,0))
par(las=2)
par(lwd=0.2)
par(ps=12)

##########################################
# RCP 430 BES
##########################################
box_norm <- scale(results_430_2020[,important_params,with=F], center = TRUE, scale = TRUE)
good_labels <- improve_labels(colnames(box_norm))
for (i in 1:NROW(colnames(box_norm))){
  good_labels[i]  <- param.to.label(colnames(box_norm)[i])
}

b0 <- boxplot(box_norm, plot=F)
bxp(b0, at=1:ncol(box_norm)*2-1,
    xlim = c(1, ncol(box_norm)*2-1), ylim = c(-2.1,2.1),
    xaxt = "n", boxwex=1.6, outline=F,
    boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
    main="",ylab="")
grid(nx=15, ny=NA, lwd=0.5, lty = "dashed", col="darkgray")
abline(h=0, col="darkgray", lty = "dashed", lwd = 0.5)
abline(v=6, col="darkgray", lty = "solid", lwd = 2)
box_subset <- box_norm[which(results_430_2020$BES > 0),]
if (NROW(box_subset)>0) {
  b1 <- boxplot(box_subset, plot=F)
  b1$stats[3,] <- colMeans(box_subset)
  b1$stats[2,] <- b1$stats[3,]- apply(box_subset, 2, sd)
  b1$stats[4,] <- b1$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b1, at=1:ncol(box_norm)*2-1.4, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2020, boxwex=0.25, whisklty = 0,
      border=col1.2020, boxwex=0.5, outline=F, staplelty = 0)
}
box_norm <- scale(results_430_2050[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_430_2050$BES > 0),]
if (NROW(box_subset)>0) {
  b2 <- boxplot(box_subset, plot=F)
  b2$stats[3,] <- colMeans(box_subset)
  b2$stats[2,] <- b2$stats[3,]- apply(box_subset, 2, sd)
  b2$stats[4,] <- b2$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b2, at=1:ncol(box_norm)*2-1, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2050, boxwex=0.25, whisklty = 0,
      border=col1.2050, boxwex=0.5, outline=F, alpha=0.4,  staplelty = 0)
}
box_norm <- scale(results_430_2100[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_430_2100$BES > 0),]
if (NROW(box_subset)>0) {
  b3 <- boxplot(box_subset, plot=F)
  b3$stats[3,] <- colMeans(box_subset)
  b3$stats[2,] <- b3$stats[3,]- apply(box_subset, 2, sd)
  b3$stats[4,] <- b3$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b3, at=1:ncol(box_norm)*2-0.6, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2100, boxwex=0.25, whisklty = 0,
      border=col1.2100, boxwex=0.5, outline=F, alpha=0.2, staplelty = 0)
}
text(0.6,1.98,"a", cex=1.6, font=2)
axis(1, at=1:ncol(box_norm)*2-1, labels = vector(mode="character", length=length(good_labels)), tick = TRUE, tcl=-0.2)


##########################################
# RCP 650 BES
##########################################
box_norm <- scale(results_650_2020[,important_params,with=F], center = TRUE, scale = TRUE)
b0 <- boxplot(box_norm, plot=F)
bxp(b0, at=1:ncol(box_norm)*2-1,
    xlim = c(1, ncol(box_norm)*2-1), ylim = c(-2.1,2.1),
    xaxt = "n", yaxt = "n", boxwex=1.6, outline=F,
    boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
    main="",ylab="")
grid(nx=15, ny=NA, lwd=0.5, lty = "dashed", col="darkgray")
abline(h=0, col="darkgray", lty = "dashed", lwd = 0.5)
abline(v=6, col="darkgray", lty = "solid", lwd = 2)
box_subset <- box_norm[which(results_650_2020$BES > 0),]
if (NROW(box_subset)>0) {
  b1 <- boxplot(box_subset, plot=F)
  b1$stats[3,] <- colMeans(box_subset)
  b1$stats[2,] <- b1$stats[3,]- apply(box_subset, 2, sd)
  b1$stats[4,] <- b1$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b1, at=1:ncol(box_norm)*2-1.4, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2020, boxwex=0.25, whisklty = 0,
      border=col1.2020, boxwex=0.5, outline=F, staplelty = 0, boxlwd=0.1)
}
box_norm <- scale(results_650_2050[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_650_2050$BES > 0),]
if (NROW(box_subset)>0) {
  b2 <- boxplot(box_subset, plot=F)
  b2$stats[3,] <- colMeans(box_subset)
  b2$stats[2,] <- b2$stats[3,]- apply(box_subset, 2, sd)
  b2$stats[4,] <- b2$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b2, at=1:ncol(box_norm)*2-1, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2050, boxwex=0.25, whisklty = 0,
      border=col1.2050, boxwex=0.5, outline=F, alpha=0.4,  staplelty = 0, boxlwd=0.1)
}
box_norm <- scale(results_650_2100[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_650_2100$BES > 0),]
if (NROW(box_subset)>0) {
  b3 <- boxplot(box_subset, plot=F)
  b3$stats[3,] <- colMeans(box_subset)
  b3$stats[2,] <- b3$stats[3,]- apply(box_subset, 2, sd)
  b3$stats[4,] <- b3$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b3, at=1:ncol(box_norm)*2-0.6, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2100, boxwex=0.25, whisklty = 0,
      border=col1.2100, boxwex=0.5, outline=F, alpha=0.2, staplelty = 0, boxlwd=0.1)
}
text(0.6,1.98,"d", cex=1.6, font=2)
axis(1, at=1:ncol(box_norm)*2-1, labels = vector(mode="character", length=length(good_labels)), tick = TRUE, tcl=-0.2)


##########################################
# RCP 430 BEBCS
##########################################
box_norm <- scale(results_430_2020[,important_params,with=F], center = TRUE, scale = TRUE)
good_labels <- improve_labels(colnames(box_norm))
for (i in 1:NROW(colnames(box_norm))){
  good_labels[i]  <- param.to.label(colnames(box_norm)[i])
}
b0 <- boxplot(box_norm, plot=F)
bxp(b0, at=1:ncol(box_norm)*2-1,
    xlim = c(1, ncol(box_norm)*2-1), ylim = c(-2.1,2.1),
    xaxt = "n", boxwex=1.6, outline=F,
    boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
    main="",ylab="",tck=-.001)
grid(nx=15, ny=NA, lwd=0.5, lty = "dashed", col="darkgray")
abline(h=0, col="darkgray", lty = "dashed", lwd = 0.5)
abline(v=6, col="darkgray", lty = "solid", lwd = 2)
box_subset <- box_norm[which(results_430_2020$BEBCS > 0),]
if (NROW(box_subset)>0) {
  b1 <- boxplot(box_subset, plot=F)
  b1$stats[3,] <- colMeans(box_subset)
  b1$stats[2,] <- b1$stats[3,]- apply(box_subset, 2, sd)
  b1$stats[4,] <- b1$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b1, at=1:ncol(box_norm)*2-1.4, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2020, boxwex=0.25, whisklty = 0,
      border=col1.2020, boxwex=0.5, outline=F, staplelty = 0)
}
box_norm <- scale(results_430_2050[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_430_2050$BEBCS > 0),]
if (NROW(box_subset)>0) {
  b2 <- boxplot(box_subset, plot=F)
  b2$stats[3,] <- colMeans(box_subset)
  b2$stats[2,] <- b2$stats[3,]- apply(box_subset, 2, sd)
  b2$stats[4,] <- b2$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b2, at=1:ncol(box_norm)*2-1, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2050, boxwex=0.25, whisklty = 0,
      border=col1.2050, boxwex=0.5, outline=F, alpha=0.4,  staplelty = 0)
}
box_norm <- scale(results_430_2100[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_430_2100$BEBCS > 0),]
if (NROW(box_subset)>0) {
  b3 <- boxplot(box_subset, plot=F)
  b3$stats[3,] <- colMeans(box_subset)
  b3$stats[2,] <- b3$stats[3,]- apply(box_subset, 2, sd)
  b3$stats[4,] <- b3$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b3, at=1:ncol(box_norm)*2-0.6, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2100, boxwex=0.25, whisklty = 0,
      border=col1.2100, boxwex=0.5, outline=F, alpha=0.2, staplelty = 0)
}
text(0.6,1.98,"b", cex=1.6, font=2)
axis(1, at=1:ncol(box_norm)*2-1, labels = vector(mode="character", length=length(good_labels)), tick = TRUE, tcl=-0.2)


##########################################
# RCP 650 BEBCS
##########################################
box_norm <- scale(results_650_2020[,important_params,with=F], center = TRUE, scale = TRUE)
good_labels <- improve_labels(colnames(box_norm))
for (i in 1:NROW(colnames(box_norm))){
  good_labels[i]  <- param.to.label(colnames(box_norm)[i])
}
b0 <- boxplot(box_norm, plot=F)
bxp(b0, at=1:ncol(box_norm)*2-1,
    xlim = c(1, ncol(box_norm)*2-1), ylim = c(-2.1,2.1),
    xaxt = "n", yaxt = "n", boxwex=1.6, outline=F,
    boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
    main="",ylab="")
grid(nx=15, ny=NA, lwd=0.5, lty = "dashed", col="darkgray")
abline(h=0, col="darkgray", lty = "dashed", lwd = 0.5)
abline(v=6, col="darkgray", lty = "solid", lwd = 2)
box_subset <- box_norm[which(results_650_2020$BEBCS > 0),]
if (NROW(box_subset)>0) {
  b1 <- boxplot(box_subset, plot=F)
  b1$stats[3,] <- colMeans(box_subset)
  b1$stats[2,] <- b1$stats[3,]- apply(box_subset, 2, sd)
  b1$stats[4,] <- b1$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b1, at=1:ncol(box_norm)*2-1.4, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2020, boxwex=0.25, whisklty = 0,
      border=col1.2020, boxwex=0.5, outline=F, staplelty = 0)
}
box_norm <- scale(results_650_2050[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_650_2050$BEBCS > 0),]
if (NROW(box_subset)>0) {
  b2 <- boxplot(box_subset, plot=F)
  b2$stats[3,] <- colMeans(box_subset)
  b2$stats[2,] <- b2$stats[3,]- apply(box_subset, 2, sd)
  b2$stats[4,] <- b2$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b2, at=1:ncol(box_norm)*2-1, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2050, boxwex=0.25, whisklty = 0,
      border=col1.2050, boxwex=0.5, outline=F, alpha=0.4,  staplelty = 0)
}
box_norm <- scale(results_650_2100[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_650_2100$BEBCS > 0),]
if (NROW(box_subset)>0) {
  b3 <- boxplot(box_subset, plot=F)
  b3$stats[3,] <- colMeans(box_subset)
  b3$stats[2,] <- b3$stats[3,]- apply(box_subset, 2, sd)
  b3$stats[4,] <- b3$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b3, at=1:ncol(box_norm)*2-0.6, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2100, boxwex=0.25, whisklty = 0,
      border=col1.2100, boxwex=0.5, outline=F, alpha=0.2, staplelty = 0)
}
text(0.6,1.98,"e", cex=1.6, font=2)
axis(1, at=1:ncol(box_norm)*2-1, labels = vector(mode="character", length=length(good_labels)), tick = TRUE, tcl=-0.2)

##########################################
# RCP 430 BECCS
##########################################
box_norm <- scale(results_430_2020[,important_params,with=F], center = TRUE, scale = TRUE)
b0 <- boxplot(box_norm, plot=F)
bxp(b0, at=1:ncol(box_norm)*2-1,
    xlim = c(1, ncol(box_norm)*2-1), ylim = c(-2.1,2.1),
    xaxt = "n", boxwex=1.6, outline=F,
    boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
    main="",ylab="")
grid(nx=15, ny=NA, lwd=0.5, lty = "dashed", col="darkgray")
abline(h=0, col="darkgray", lty = "dashed", lwd = 0.5)
abline(v=6, col="darkgray", lty = "solid", lwd = 2)
box_subset <- box_norm[which(results_430_2020$BECCS > 0),]
if (NROW(box_subset)>0) {
  b1 <- boxplot(box_subset, plot=F)
  b1$stats[3,] <- colMeans(box_subset)
  b1$stats[2,] <- b1$stats[3,]- apply(box_subset, 2, sd)
  b1$stats[4,] <- b1$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b1, at=1:ncol(box_norm)*2-1.4, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2020, boxwex=0.25, whisklty = 0,
      border=col1.2020, boxwex=0.5, outline=F, staplelty = 0)
}
box_norm <- scale(results_430_2050[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_430_2050$BECCS > 0),]
if (NROW(box_subset)>0) {
  b2 <- boxplot(box_subset, plot=F)
  b2$stats[3,] <- colMeans(box_subset)
  b2$stats[2,] <- b2$stats[3,]- apply(box_subset, 2, sd)
  b2$stats[4,] <- b2$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b2, at=1:ncol(box_norm)*2-1, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2050, boxwex=0.25, whisklty = 0,
      border=col1.2050, boxwex=0.5, outline=F, alpha=0.4,  staplelty = 0)
}
box_norm <- scale(results_430_2100[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_430_2100$BECCS > 0),]
if (NROW(box_subset)>0) {
  b3 <- boxplot(box_subset, plot=F)
  b3$stats[3,] <- colMeans(box_subset)
  b3$stats[2,] <- b3$stats[3,]- apply(box_subset, 2, sd)
  b3$stats[4,] <- b3$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b3, at=1:ncol(box_norm)*2-0.6, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", add = TRUE, boxfill=col2.2100, boxwex=0.25, whisklty = 0,
      border=col1.2100, boxwex=0.5, outline=F, alpha=0.2, staplelty = 0)
}
text(0.6,1.98,"c", cex=1.6, font=2)
axis(1, at=1:ncol(box_norm)*2-1, labels = vector(mode="character", length=length(good_labels)), tick = TRUE, tcl=-0.2)
text(1:ncol(box_norm)*2-0.7, -2.5, labels = good_labels, srt = 33, adj=c(1,0), xpd = NA, cex=1)


##########################################
# RCP 650 BECCS
##########################################
box_norm <- scale(results_650_2020[,important_params,with=F], center = TRUE, scale = TRUE)
b0 <- boxplot(box_norm, plot=F)
bxp(b0, at=1:ncol(box_norm)*2-1,
    xlim = c(1, ncol(box_norm)*2-1), ylim = c(-2.1,2.1),
    xaxt = "n", yaxt = "n", boxwex=1.6, outline=F,
    boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
    main="",ylab="")
grid(nx=15, ny=NA, lwd=0.5, lty = "dashed", col="darkgray")
abline(h=0, col="darkgray", lty = "dashed", lwd = 0.5)
abline(v=6, col="darkgray", lty = "solid", lwd = 2)
box_subset <- box_norm[which(results_650_2020$BECCS > 0),]
if (NROW(box_subset)>0) {
  b1 <- boxplot(box_subset, plot=F)
  b1$stats[3,] <- colMeans(box_subset)
  b1$stats[2,] <- b1$stats[3,]- apply(box_subset, 2, sd)
  b1$stats[4,] <- b1$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b1, at=1:ncol(box_norm)*2-1.4, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2020, boxwex=0.25, whisklty = 0,
      border=col1.2020, boxwex=0.5, outline=F, staplelty = 0)
}
box_norm <- scale(results_650_2050[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_650_2050$BECCS > 0),]
if (NROW(box_subset)>0) {
  b2 <- boxplot(box_subset, plot=F)
  b2$stats[3,] <- colMeans(box_subset)
  b2$stats[2,] <- b2$stats[3,]- apply(box_subset, 2, sd)
  b2$stats[4,] <- b2$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b2, at=1:ncol(box_norm)*2-1, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2050, boxwex=0.25, whisklty = 0,
      border=col1.2050, boxwex=0.5, outline=F, alpha=0.4,  staplelty = 0)
}
box_norm <- scale(results_650_2100[,important_params,with=F], center = TRUE, scale = TRUE)
box_subset <- box_norm[which(results_650_2100$BECCS > 0),]
if (NROW(box_subset)>0) {
  b3 <- boxplot(box_subset, plot=F)
  b3$stats[3,] <- colMeans(box_subset)
  b3$stats[2,] <- b3$stats[3,]- apply(box_subset, 2, sd)
  b3$stats[4,] <- b3$stats[3,]+ apply(box_subset, 2, sd)
  bxp(b3, at=1:ncol(box_norm)*2-0.6, xlim = c(1, ncol(box_norm)*2-1),
      xaxt = "n", yaxt = "n", add = TRUE, boxfill=col2.2100, boxwex=0.25, whisklty = 0,
      border=col1.2100, boxwex=0.5, outline=F, alpha=0.2, staplelty = 0)
}
text(0.6,1.98,"f", cex=1.6, font=2)
axis(1, at=1:ncol(box_norm)*2-1, labels = vector(mode="character", length=length(good_labels)), tick = TRUE, tcl=-0.2)
text(1:ncol(box_norm)*2-0.7, -2.5, labels = good_labels, srt = 33, adj=c(1,0), xpd = NA, cex=1)

mtext("Standardized Value (standard deviations)", side=2, outer = TRUE, cex = 1, line = 2.2, las=0)
mtext("MS430-480", side=3, outer = TRUE, cex = 1, line = 2.2, las=0, at=0.25)
mtext("MS650-720", side=3, outer = TRUE, cex = 1, line = 2.2, las=0, at=0.75)
mtext("BES",   side=4, outer = TRUE, cex = 1, line = 1, las=0, at=0.82)
mtext("BEBCS", side=4, outer = TRUE, cex = 1, line = 1, las=0, at=0.5)
mtext("BECCS", side=4, outer = TRUE, cex = 1, line = 1, las=0, at=0.18)

rect(-11, 11.64, -7, 11.76, col = col2.2020, border = col1.2020, xpd=NA)
rect(-2,  11.64, 2,  11.76, col = col2.2050, border = col1.2050, xpd=NA)
rect(7,   11.64, 11, 11.76, col = col2.2100, border = col1.2100, xpd=NA)
mtext("2020", at=0.411, side=3, outer = TRUE, las=1, line = 0.63, cex=0.7)
mtext("2050", at=0.561, side=3, outer = TRUE, las=1, line = 0.63, cex=0.7)
mtext("2100", at=0.711, side=3, outer = TRUE, las=1, line = 0.63, cex=0.7)
dev.off()

>>>>>>> f22ddffdc4b9000d81fcf21ee02fac3fe2f46fb0
