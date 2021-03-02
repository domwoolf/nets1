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

df <- data.frame(x2020_650, x2050_650, x2100_650, x2020_430, x2050_430, x2100_430)
colnames(df) <- c("MS650-720 (2020)","MS650-720 (2050)","MS650-720 (2100)","MS430-480 (2020)","MS430-480 (2050)","MS430-480 (2100)")

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

dfm <- melt(df, value.name = "c_price")
p1c <- ggplot(dfm, aes(x=variable, y=c_price)) + theme_bw()  + coord_flip() +
  scale_y_log10(name=expression(Carbon~Price~'($ Mg'^{-1}*' C)'), breaks = c(50,100,200,500,1000,2000,5000, 10000,20000)) +
  scale_x_discrete(name='') +
  annotation_custom(grobTree(textGrob("c", x=0.02,  y=0.91, hjust=0, gp=gpar(col="black", fontsize=20, fontfamily = "sans", fontface="bold")))) +
  stat_summary(fun.data=mybxp, geom="boxplot")  # + stat_summary(fun.data=myout, geom="point")

p1c











# par(mar=c(4,8,2,2))
# par(las = 1)
# x.label = expression("Carbon Price ($" ~ Mg^{-1} ~ "C)")
# boxplot(df, log="x", horizontal=T, outline=F, boxwex=0.5,
#         col = c("chocolate2","chocolate3", "chocolate4","slateblue", "slateblue3", "slateblue4"),
#         xlab=x.label)
# grid()

# plotfile <- "CPrice_scenarios.tif"
# tiff(plotfile, units="mm", width=90, height=30, res=300, pointsize = 6)
# par(mar=c(3,7,1,1))
# par(las = 1)
# par(mgp=c(1.5,0.4,0))
# par(tck=-0.04)
# par(cex.axis=0.4)
# boxplot(df, log="x", horizontal=T, outline=F, boxwex=0.5,
#         cex.axis=0.8, cex.lab=0.8,
#         las=1, mgp=c(1.5,0.4,0), tck=-0.02,
#         col = rep("white",6),
#         xlab=x.label)
# text(30,6,"c", cex=1.5, font=2)
# dev.off()
