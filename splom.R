
# old.names=names(pcres)
# names(pcres) <- old.names
# names(pcres) <- c("BES","BEBCS","BECCS","P_C","P_E","CY_BC",
#                   "i","'BC_T'","P_Cr","CCS_cost")

names.list = list("BES"          = "BES",      
                  "BEBCS"        = "BEBCS",
                  "BECCS"        = "BECCS",
                  "c_price"      = "C Price",  
                  "elec_price"   = "E. Price",   
                  "bc_yield_impact" = "Crop Yield Impact",   
                  "disc_rate"    = "Discount",   
                  "bc_stab_fact" = "BC Persistance",
                  "crop_price_increase" = "Crop Price Increase",     
                  "ccs_cost"     ="CCS Cost")

my_labeller <- function(variable, value){
  return(param.label[value])
} 
+
  scale_colour_manual(values = c("forestgreen","royalblue4", "grey50", col1.bes, col1.bebcs, col1.beccs))

cpres <- pcres[sample(4500,100),]

cols=rep(col1.bes, 100L)
cols[cpres$BEBCS > 0] = col1.bebcs
cols[cpres$BECCS > 0] = col1.beccs
cols

cor.mat <- cor.matrix(variables=cpres, test=cor.test, method='p')
p1 <- ggcorplot2(cor.mat, data=cpres, var_text_size=1, cor_text_limits=c(3.5,4.55), 
                 main="", alpha=.1, type="points", psize=1, cols=rep(cols,45))
p1 +  theme(axis.line = element_blank(), 
            strip.text.x = element_text(size = 10, colour = "black", angle = 90),
            strip.text.y = element_text(size = 10, colour = "black", angle = 0)) +
  scale_colour_manual(values = c("forestgreen","royalblue4", "grey50", "black", "green", "orange"))
# ggsave("test.pdf", width = 150 , height = 150 , units = "mm")



cols=rep(col1.bes, 4500L)
cols[pcres$BEBCS > 0] = col1.bebcs
cols[pcres$BECCS > 0] = col1.beccs
cor.mat <- cor.matrix(variables=pcres, test=cor.test, method='p')
p1 <- ggcorplot2(cor.mat, data=pcres, var_text_size=4, cor_text_limits=c(3.5,4.55),
                 main="", alpha=.01, type="points", psize=1, cols=rep(cols,45))
p1 +  theme(axis.line = element_blank(),
            strip.text.x = element_text(size = 9, colour = "black", angle = 90),
            strip.text.y = element_text(size = 9, colour = "black", angle = 0)) +
  scale_colour_manual(values = c("forestgreen","royalblue4", "grey50"))
ggsave("test.pdf", width = 150 , height = 150 , units = "mm")





ggcorplot2 <- 
function (cor.mat, data = NULL, lines = TRUE, line.method = c("lm", "loess"), 
          type = "points", alpha = 0.25, main = "auto", var_text_size = 5, 
          psize = 0.5, pshape = 19, cols, 
          cor_text_limits = c(5, 25), level = 0.05) 
{
  x_var <- y_var <- trans <- rsq <- p <- x_label <- NULL
  ezLev <- function(x, new_order) {
    for (i in rev(new_order)) {
      x <- relevel(x, ref = i)
    }
    return(x)
  }
  if (all(line.method == c("lm", "loess"))) 
    line.method <- "lm"
  nm <- names(cor.mat)
  for (i in 1:length(nm)) dat <- if (i == 1) 
    d(eval(parse(text = nm[i]), data, parent.frame()))
  else d(dat, eval(parse(text = nm[i]), data, parent.frame()))
  data <- dat
  names(data) <- nm
  for (i in 1:length(data)) {
    data[, i] <- as.numeric(data[, i])
    data[, i] <- (data[, i] - mean(data[, i], na.rm = TRUE))/sd(data[, 
                                                                     i], na.rm = TRUE)
  }
  z <- data.frame()
  i <- 1
  j <- i
  while (i <= length(data)) {
    if (j > length(data)) {
      i <- i + 1
      j <- i
    }
    else {
      x <- data[, i]
      y <- data[, j]
      temp <- as.data.frame((cbind(x, y)))
      temp <- cbind(temp, names(data)[i], names(data)[j])
      z <- rbind(z, temp)
      j <- j + 1
    }
  }
  z <- cbind(z, alpha)
  names(z) = c("x_var", "y_var", "x_label", "y_label", "trans")
  z$x_label <- ezLev(factor(z$x_label), names(data))
  z$y_label <- ezLev(factor(z$y_label), names(data))
  z = z[z$x_label != z$y_label, ]
  z_cor <- data.frame()
  i <- 1
  j <- i
  while (i <= length(data)) {
    if (j > length(data)) {
      i <- i + 1
      j <- i
    }
    else {
      x <- na.omit(data[, i])
      y <- na.omit(data[, j])
      x_mid <- min(x) + diff(range(x))/2
      y_mid <- min(y) + diff(range(y))/2
      this_cor <- cor.mat[[i]][[j]]$estimate
      this_cor.test <- cor.mat[[i]][[j]]
      # this_col <- ifelse(this_cor.test$p.value < level, "red", "grey1")
      this_col <- vector("character", length(this_cor.test$p.value ))
      this_col <- "red"
      this_col[this_cor < 0] <- "blue"
      this_col[this_cor.test$p.value > level] <- "grey1"
      this_size <- (this_cor)^2
      cor_text <- ifelse(this_cor > 0, substr(format(c(this_cor, 
                                                       0.123456789), digits = 2)[1], 2, 4), paste("-", 
                                                                                                  substr(format(c(this_cor, 0.123456789), digits = 2)[1], 
                                                                                                         3, 5), sep = ""))
      b <- as.data.frame(cor_text)
      b <- cbind(b, x_mid, y_mid, this_col, this_size, 
                 names(data)[j], names(data)[i])
      z_cor <- rbind(z_cor, b)
      j <- j + 1
    }
  }
  # browser()
  names(z_cor) <- c("cor", "x_mid", "y_mid", "p", "rsq", "x_label", 
                    "y_label")
  z_cor$x_label <- ezLev(factor(z_cor$x_label), names(data))
  z_cor$y_label <- ezLev(factor(z_cor$y_label), names(data))
  diag <- z_cor[z_cor$x_label == z_cor$y_label, ]
  z_cor <- z_cor[z_cor$x_label != z_cor$y_label, ]
  points_layer <- geom_point(aes(x = x_var, y = y_var, alpha = trans), colour=cols, shape = pshape, size = psize, data = z)
  bin_layer <- geom_hex(data = z, mapping = aes(x = x_var, y = y_var, alpha = trans), bins = 10)
  lm_line_layer <- stat_smooth(aes(x = x_var, y = y_var), method = line.method, size=0.2, colour="black")
  cor_text <- geom_text(aes(x = y_mid, y = x_mid, label = cor, size = rsq, colour = p), data = z_cor)   
  var_text <- geom_text(aes(x = y_mid, y = x_mid, label = x_label), data = diag, size = var_text_size, angle=0)
  f <- facet_grid(y_label ~ x_label, scales = "free", labeller=my_labeller)
  o <- theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
             axis.ticks = element_blank(), axis.text.y = element_blank(), 
             axis.text.x = element_blank(), axis.title.y = element_blank(), 
             axis.title.x = element_blank(), legend.position = "none")
  size_scale <- scale_size(limits = c(0, 1), range = cor_text_limits)
  the.plot <- ggplot(data = z)
  if (type == "bins") 
    the.plot <- the.plot + bin_layer
  else if (type == "points") 
    the.plot <- the.plot + points_layer + scale_alpha_identity()
  the.plot <- the.plot + cor_text + f + o + size_scale  # + var_text 
  if (type == "bins") 
    the.plot <- the.plot + scale_fill_gradient(low = "grey", 
                                               high = "black")
  if (lines) 
    the.plot <- the.plot + lm_line_layer
  if (main == "auto") 
    main <- cor.mat[[1]][[1]]$method
  the.plot <- the.plot + ggtitle(main)
  return(the.plot)
}
