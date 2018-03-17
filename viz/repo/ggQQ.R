

ggQQ <- function









  #Quantile-Quantile plot
  viz_QQ <- function(data, x, y, r2 = T, save = F, eq.scale = T, ...) {

  require(ggplot2)

  #List of x and y values
  xv <- select_(data, x) %>% unlist(use.names = F)
  yv <- select_(data, y) %>% unlist(use.names = F)

  if (eq.scale == TRUE) {

    x.break <- pretty(c(xv, yv), n = 6)
    y.break <- pretty(c(xv, yv), n = 6)

  } else {

    x.break <- pretty(xv, n = 6)
    y.break <- pretty(yv, n = 6)

  }

  #Display of r2 on the plot
  pos_x <- min(x.break) + (max(x.break) - min(x.break))* 0.5/10
  pos_y <- min(y.break) + (max(y.break) - min(y.break))* 9.5/10


  equ_r2 <- ifelse(r2 == TRUE, paste("r^2 == ",
    format(summary(lm(yv ~ xv))$r.squared, digits = 3)), "")

  p <- ggplot(data, aes_string(x = x, y = y)) +
    theme_bw(base_size = 10) +
    geom_point(shape=1, color = "blue") +
    labs(x = x, y = y) +
    scale_x_continuous(limits = range(x.break), breaks = x.break) +
    scale_y_continuous(limits = range(y.break), breaks = y.break) +
    geom_abline() +
    annotate("text", x=pos_x, y=pos_y, label = equ_r2, parse=T, hjust = 0)

  if(save == TRUE) {
    ggsave(paste0("viz_scatter_",x,"_", y,".png"), plot = p, ...)}

  return(p)
}

