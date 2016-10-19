<<<<<<< HEAD
##########################################################
# carbon price plots
##########################################################

# RPV vs carbon price
p1a <- ggplot(all_data) +
  scale_x_continuous(name = expression("Carbon Price ($" ~ Mg^{-1} ~ "C)")) +
  scale_y_continuous(name = expression("RPV ($" ~ Mg^{-1} ~ "biomass)")) +
  geom_ribbon(aes(x=all_data$c_price, ymin=ci_min_bes, ymax=ci_max_bes), alpha=0.2, fill=col1.bes) +
  geom_ribbon(aes(x=all_data$c_price, ymin=ci_min_bebcs, ymax=ci_max_bebcs), alpha=0.2, fill=col1.bebcs) +
  geom_ribbon(aes(x=all_data$c_price, ymin=ci_min_beccs, ymax=ci_max_beccs), alpha=0.2, fill=col1.beccs) +
  geom_line(aes(x=cp , y=fitted_bes_rv, colour=col1.bes), size=1) +
  geom_line(aes(x=cp , y=fitted_bebcs_rv, colour=col1.bebcs), size=1) +
  geom_line(aes(x=cp , y=fitted_beccs_rv, colour=col1.beccs), size=1) +
  scale_color_manual(values=c(col1.bes,col1.bebcs,col1.beccs), labels = c("BES", "BEBCS", "BECCS"), name="") +
  coord_cartesian(ylim = c(-600, 600), xlim =c(0,3000)) +
  annotation_custom(grobTree(textGrob("a", x=0.05,  y=0.95, hjust=0, gp=gpar(col="black", fontsize=20, fontfamily = "sans", fontface="bold")))) +
  theme_bw()  +
  theme(legend.position = "none")

# probability vs c price
p1b <- ggplot(smooth_prob_melt, aes(x=c_price, y = value, colour=variable)) +
  scale_x_continuous(name = expression("Carbon Price ($" ~ Mg^{-1} ~ "C)")) +
  scale_y_continuous(name = expression("Probability")) +
  geom_line(size=1) +
  scale_color_manual(values=c(col1.bes,col1.bebcs,col1.beccs), labels = c("BES", "BEBCS", "BECCS"), name="") +
  coord_cartesian(ylim = c(0, 1), xlim =c(0,3000))  +
  theme_bw()  +
  theme(legend.position = c(0.3,0.9), legend.key = element_blank(), legend.background = element_blank()	)+
  annotation_custom(grobTree(textGrob("b", x=0.05,  y=0.95, hjust=0, gp=gpar(col="black", fontsize=20, fontfamily = "sans", fontface="bold"))))


# carbon price distribution in each scenario
p1c <- ggplot(c_price_dist.long, aes(x=variable, y=c_price)) + theme_bw()  + coord_flip() +
  scale_y_log10(name=expression(Carbon~Price~'($ Mg'^{-1}*' C)'), breaks = c(50,100,200,500,1000,2000,5000, 10000,20000)) +
  scale_x_discrete(name='') +
  annotation_custom(grobTree(textGrob("c", x=0.02,  y=0.91, hjust=0, gp=gpar(col="black", fontsize=20, fontfamily = "sans", fontface="bold")))) +
  stat_summary(fun.data=mybxp, geom="boxplot")  # + stat_summary(fun.data=myout, geom="point")


ggdraw() +
  draw_plot(p1a, x = 0,   y = 0.35, width = 0.5, height = 0.65) +
  draw_plot(switch_axis_position(p1b, axis = 'y'), x = 0.5, y = 0.35, width = 0.5, height = 0.65) +
  draw_plot(p1c, x = 0,   y = 0, width = 0.91, height = 0.35)

if (saveplots) ggsave("c_price.pdf", width = 183 , height = 120 , units = "mm")


thumbnail <- ggplot(smooth_prob_melt, aes(x=c_price, y = value, colour=variable)) +
  scale_x_continuous(name = expression("Carbon Price")) +
  scale_y_continuous(name = expression("Probability")) +
  geom_line(size=1) +
  scale_color_manual(values=c(col1.bes,col1.bebcs,col1.beccs), labels = c("BES", "BEBCS", "BECCS"), name="") +
  coord_cartesian(ylim = c(0, 1), xlim =c(0,3000))  +
  theme_bw()  +
  theme(legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_blank(),
        text = element_text(size=2),
        axis.text = element_blank(),
        axis.title = element_text(size=8),
        plot.margin = unit(c(.03,.03,-.01,-.01), "in")) +
  annotation_custom(grobTree(textGrob("BECCS", x=0.58,  y=0.88, hjust=0, gp=gpar(col="black", fontsize=6, fontfamily = "sans", fontface="bold")))) +
  annotation_custom(grobTree(textGrob("Biochar", x=0.58,  y=0.41, hjust=0, gp=gpar(col="black", fontsize=6, fontfamily = "sans", fontface="bold")))) +
  annotation_custom(grobTree(textGrob("Bioenergy", x=0.46,  y=0.13, hjust=0, gp=gpar(col="black", fontsize=6, fontfamily = "sans", fontface="bold"))))

thumbnail
ggsave("thumb1.png", thumbnail, width = 1 , height = 1 , units = "in", dpi=100)

=======
##########################################################
# carbon price plots
##########################################################

# RPV vs carbon price
p1a <- ggplot(all_data) +
  scale_x_continuous(name = expression("Carbon Price ($" ~ Mg^{-1} ~ "C)")) +
  scale_y_continuous(name = expression("RPV ($" ~ Mg^{-1} ~ "biomass)")) +
  geom_ribbon(aes(x=all_data$c_price, ymin=ci_min_bes, ymax=ci_max_bes), alpha=0.2, fill=col1.bes) +
  geom_ribbon(aes(x=all_data$c_price, ymin=ci_min_bebcs, ymax=ci_max_bebcs), alpha=0.2, fill=col1.bebcs) +
  geom_ribbon(aes(x=all_data$c_price, ymin=ci_min_beccs, ymax=ci_max_beccs), alpha=0.2, fill=col1.beccs) +
  geom_line(aes(x=cp , y=fitted_bes_rv, colour=col1.bes), size=1) +
  geom_line(aes(x=cp , y=fitted_bebcs_rv, colour=col1.bebcs), size=1) +
  geom_line(aes(x=cp , y=fitted_beccs_rv, colour=col1.beccs), size=1) +
  scale_color_manual(values=c(col1.bes,col1.bebcs,col1.beccs), labels = c("BES", "BEBCS", "BECCS"), name="") +
  coord_cartesian(ylim = c(-600, 600), xlim =c(0,3000)) +
  annotation_custom(grobTree(textGrob("a", x=0.05,  y=0.95, hjust=0, gp=gpar(col="black", fontsize=20, fontfamily = "sans", fontface="bold")))) +
  theme_bw()  +
  theme(legend.position = "none")

# probability vs c price
p1b <- ggplot(smooth_prob_melt, aes(x=c_price, y = value, colour=variable)) +
  scale_x_continuous(name = expression("Carbon Price ($" ~ Mg^{-1} ~ "C)")) +
  scale_y_continuous(name = expression("Probability")) +
  geom_line(size=1) +
  scale_color_manual(values=c(col1.bes,col1.bebcs,col1.beccs), labels = c("BES", "BEBCS", "BECCS"), name="") +
  coord_cartesian(ylim = c(0, 1), xlim =c(0,3000))  +
  theme_bw()  +
  theme(legend.position = c(0.3,0.9), legend.key = element_blank(), legend.background = element_blank()	)+
  annotation_custom(grobTree(textGrob("b", x=0.05,  y=0.95, hjust=0, gp=gpar(col="black", fontsize=20, fontfamily = "sans", fontface="bold"))))


# carbon price distribution in each scenario
p1c <- ggplot(c_price_dist.long, aes(x=variable, y=c_price)) + theme_bw()  + coord_flip() +
  scale_y_log10(name=expression(Carbon~Price~'($ Mg'^{-1}*' C)'), breaks = c(50,100,200,500,1000,2000,5000, 10000,20000)) +
  scale_x_discrete(name='') +
  annotation_custom(grobTree(textGrob("c", x=0.02,  y=0.91, hjust=0, gp=gpar(col="black", fontsize=20, fontfamily = "sans", fontface="bold")))) +
  stat_summary(fun.data=mybxp, geom="boxplot")  # + stat_summary(fun.data=myout, geom="point")


ggdraw() +
  draw_plot(p1a, x = 0,   y = 0.35, width = 0.5, height = 0.65) +
  draw_plot(switch_axis_position(p1b, axis = 'y'), x = 0.5, y = 0.35, width = 0.5, height = 0.65) +
  draw_plot(p1c, x = 0,   y = 0, width = 0.91, height = 0.35)

if (saveplots) ggsave("c_price.pdf", width = 183 , height = 120 , units = "mm")


thumbnail <- ggplot(smooth_prob_melt, aes(x=c_price, y = value, colour=variable)) +
  scale_x_continuous(name = expression("Carbon Price")) +
  scale_y_continuous(name = expression("Probability")) +
  geom_line(size=1) +
  scale_color_manual(values=c(col1.bes,col1.bebcs,col1.beccs), labels = c("BES", "BEBCS", "BECCS"), name="") +
  coord_cartesian(ylim = c(0, 1), xlim =c(0,3000))  +
  theme_bw()  +
  theme(legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_blank(),
        text = element_text(size=2),
        axis.text = element_blank(),
        axis.title = element_text(size=8),
        plot.margin = unit(c(.03,.03,-.01,-.01), "in")) +
  annotation_custom(grobTree(textGrob("BECCS", x=0.58,  y=0.88, hjust=0, gp=gpar(col="black", fontsize=6, fontfamily = "sans", fontface="bold")))) +
  annotation_custom(grobTree(textGrob("Biochar", x=0.58,  y=0.41, hjust=0, gp=gpar(col="black", fontsize=6, fontfamily = "sans", fontface="bold")))) +
  annotation_custom(grobTree(textGrob("Bioenergy", x=0.46,  y=0.13, hjust=0, gp=gpar(col="black", fontsize=6, fontfamily = "sans", fontface="bold"))))

thumbnail
ggsave("thumb1.png", thumbnail, width = 1 , height = 1 , units = "in", dpi=100)

>>>>>>> f22ddffdc4b9000d81fcf21ee02fac3fe2f46fb0
