

# One figure per powerpoint slide:
#png(file="****.png",width=600,height=400, res=80) ///ONE FIGURE PER SLIDE

#mydata_wide <- data_frame(
#    Date = as.Date("2015-01-01") + c(0:119) * months(1),
#    First = rnorm(120, mean = 50, sd = 10),
#    Second = rnorm(120, mean = 80, sd = 20),
#    Third = rnorm(120, mean = 10, sd = 5))

# mydata <- mydata_wide %>% gather(key = Source, value = value, -Date)

#  viz_series(data = mydata, x = "Date", y = "value", type = "Source")
#  viz_scatter(data = mydata_wide, x= "First", y = "Second", r2 = T)
#  viz_QQ(data = mydata_wide, x= "First", y = "Second")

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

  #Scatter plot of two vector objects
  viz_scatter <- function(data, x, y, r2 = T, save = F, eq.scale = F, ...) {

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

    equ_r2 <- ifelse(r2 == TRUE, paste("r == ",
      format(cor(yv, xv, use = "pairwise.complete.obs"), digits = 3)), "")


    #equ_r2 <- ifelse(r2 == TRUE, paste("r^2 == ",
    #  format(summary(lm(yv ~ xv))$r.squared, digits = 3)), "")

    p <- ggplot(data, aes_string(x = x, y = y)) +
      theme_bw(base_size = 10) +
      geom_point(shape=1, color = "blue") +
      labs(x = x, y = y) +
      scale_x_continuous(limits = range(x.break), breaks = x.break) +
      scale_y_continuous(limits = range(y.break), breaks = y.break) +
      geom_smooth(method='lm',formula = y ~ x, fullrange=TRUE) +
      annotate("text", x=pos_x, y=pos_y, label = equ_r2, parse=T, hjust = 0)

    if(save == TRUE) {
      ggsave(paste0("viz_scatter_",x,"_", y,".png"), plot = p, ...)}

    return(p)
  }

  #Time-series plotting
  viz_tseries <-  function(data, x, y, type, font.size = 10, color_set = NULL,
    save = F, name = NULL, ...) {

    #required packages
    require(scales);
    require(ggplot2);

    #Set color scheme
    num <- data %>% collect() %>% .[[type]] %>% unique() %>% length()
    if(!is.null(color_set)) { p_colors = color_set
    } else {  if(num == 1) { p_colors <- "#000000"
      } else { p_colors <- brewer.pal(num,"Dark2")}}

    #GGPlot template
    p <- ggplot(data, aes_string(x=x, y=y, color=type, group=type)) +
      ggtitle(name) +
      theme_bw(base_size = font.size) +
      scale_color_manual(values = p_colors)  +
      scale_y_continuous(breaks = pretty_breaks()) +
      geom_line() +
      labs(x ="", y = "") +
      theme(legend.justification = c(1, 1),
            legend.position = c(1, 1),
            plot.title   = element_text(lineheight=.8, face="bold"),
            legend.title = element_blank())

    if(save == TRUE) { ggsave(paste0("Series_",name,".png"), plot = p,...)}

    return(p)
  }

  #Flow-duration-curves
  viz_FDC <- function(data, x, y, type, font.size = 10, name = NULL,
     color_set = NULL, save = F, ...) {

    #required packages
    require(scales);
    require(ggplot2);

    #Set color scheme
    if(!is.null(color_set)) {
      p_colors = color_set
    } else {  if(ncol(data) == 1) {
      p_colors <- "#000000"
    } else { p_colors <- brewer.pal(ncol(data),"Dark2")}}

    #Flow Duration Curves
    df <- data %>%
      group_by_(eval(type)) %>%
      mutate(y = eval(value),
             y = sort(y, decreasing=TRUE, na.last = TRUE),
             x = 100*(1:n())/(n()+1))

    #GGPlot template
    p <- ggplot(mapping = aes(x=x, y = y)) +
      ggtitle(name) +
      theme_bw(base_size = font.size) +
      scale_color_manual(values = p_colors)  +
      theme(legend.justification = c(1, 1),
            legend.position = c(1, 1),
            plot.title   = element_text(lineheight=.8, face="bold"),
            legend.title = element_blank()) +
      geom_line(aes_string(color = type, group = type), data = df) +
      scale_x_continuous(breaks = seq(0,100,20)) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      labs(x = "Percentage exceedance (%)", y ="")

    if(save == TRUE) { ggsave(paste0("FDC_",name,".png"), plot = p, ...)}

    return(p)
  }

  #Box-plot
  viz_boxplot <- function(data, x, y, type, font.size = 10, name = NULL,
     color_set = NULL, save = F, ...) {

    #required packages
    require(scales);
    require(ggplot2);

    #Set color scheme
    if(!is.null(color_set)) {
      p_colors = color_set
    } else {  if(ncol(data) == 1) {
      p_colors <- "#000000"
    } else { p_colors <- brewer.pal(ncol(data),"Dark2")}}

    #GGPlot template
    data %<>% mutate(Month = as.factor(month(Date)))

    p <- ggplot(data = data, mapping = aes_string(y=y, color = type)) +
      ggtitle(name) +
      theme_bw(base_size = font.size) +
      geom_boxplot(aes(x=Month), outlier.size = 0.6) +
      scale_color_manual(values = p_colors)  +
      theme(legend.justification = c(1, 1),
            legend.position = c(1, 1),
            plot.title   = element_text(lineheight=.8, face="bold"),
            legend.title = element_blank()) +
      labs(x="", y="")

    if(save == TRUE) { ggsave(paste0("Boxplot_",name,".png"), plot = p, ...)}

    return(p)
  }

  #ACF & PACF functions
  viz_ACF <- function(v,lag=10, name.arg=NULL) {

   p <- list()

   require(ggplot2)
   require(grid)
   require(gridExtra)

   #ACF & PACF results
   v.acf  <- acf(v, plot=FALSE,lag.max=lag)
   v.pacf <- pacf(v, plot=FALSE,lag.max=lag)

   #Confidence intervals
   ci <- 0.95 # Indicates 95% confidence level
   clim0 <- qnorm((1 + ci)/2)/sqrt(v.acf$n.used)
   clim <- c(-clim0,clim0)
   hline.data <- data.frame(z=c(0,clim), type=c("base","ci","ci"))

   #Plot time-series
   p[['Time-Series']] <- ggplot(data.frame(ID=1:length(v),VAR=v), aes(x=ID,y=VAR)) +
      geom_line() + labs(x="years",y="precipitation (mm)")

   #Plot acf
   p[['ACF']] <- ggplot(data.frame(lag=v.acf$lag,acf=v.acf$acf)) +
      theme_bw(base_size=8) +
      ggtitle(name.arg) +
      geom_hline(aes(yintercept=z,colour=type,linetype=type),hline.data) +
      geom_linerange(aes(x=as.factor(lag),ymin=0,ymax=acf, size=0.8)) +
      scale_colour_manual(values = c("black","red")) +
      scale_linetype_manual(values =c("solid","dashed")) +
      scale_y_continuous(limits=c(-0.5,1), breaks=seq(-0.50,1,0.5)) +
      #labs(x='lag (years)', y='')
      labs(x=NULL, y=NULL) +
      guides(color=FALSE, shape=FALSE, size=FALSE) +
      theme(plot.margin = unit(c(0,0.2, 0.2, 0.2),"cm"))

   #Plot pacf
   p[['PACF']] <- ggplot(data.frame(lag=v.pacf$lag,pacf=v.pacf$acf)) +
      theme_bw(base_size=8) +
      ggtitle(name.arg) +
      geom_hline(aes(yintercept=z,colour=type,linetype=type),hline.data) +
      geom_linerange(aes(x=as.factor(lag),ymin=0,ymax=pacf, size=0.8)) +
      scale_colour_manual(values = c("black","red")) +
      scale_linetype_manual(values =c("solid","dashed")) +
      scale_y_continuous(limits=c(-0.5,1), breaks=seq(-0.50,1,0.5)) +
      #labs(x='lag (years)', y='')
      labs(x=NULL, y=NULL) +
      guides(color=FALSE, shape=FALSE, size=FALSE) +
      theme(plot.margin = unit(c(-0.15,-0.15,-0.15, -0.15),"cm"))

   return(p)

}

  viz_wavelet <- function(period, signif, obs, sim, Ribbon=TRUE) {

    require(reshape2)
    require(scales)

    # Climate time-series analysis plots

    # Required inputs
    # var_sim = simulated time-series of the annual climate variable (as vector)
    # var_obs = obseved time-series of the annual climate variable (as vector)
    # gws_sim = simulated global wavelet spectra (as matrix)
    # obs = observed global wavelet spectra (as vector)
    # signif = significance level
    # gws_period = fourier periods

    sim_mean   <- apply(sim, 2, mean)
    sim_0.025  <- apply(sim, 2, function(x) quantile(x, 0.025))
    sim_0.975  <- apply(sim, 2, function(x) quantile(x, 0.975))

    df <- data_frame(period, signif, obs, sim_0.025, sim_0.975, sim_mean)

    #Ribbon-style plot
    if (Ribbon == TRUE) {

      ggplot(df, aes(x=period)) +
        theme_bw(base_size = font_size) +
        geom_ribbon(aes(ymin = sim_0.025, ymax = sim_0.975), alpha = 0.2) +
        geom_line(mapping=aes(y=sim_mean), color='black', size=0.6) +
        geom_line(mapping=aes(y=signif), color="red", linetype="dashed", size=0.6) +
        geom_line(mapping=aes(y=obs), color="blue", size=0.6) +
        labs(x="Period (years)", y=expression(paste("Power (", mm^2,")"))) +
        scale_x_continuous(breaks=seq(0,50,5), expand=c(0,0)) +
        scale_y_log10(limits = c(5*10**2,5*10**5),labels = comma)

    } else {

      df2 <- data_frame(period) %>%
        bind_cols(as.data.frame(t(sim))) %>%
        gather(key = variable, value = value, -period)

      ggplot(df2, aes(x=period)) +
        theme_bw(base_size = font_size) +
        geom_line(mapping=aes(y=value, group=variable), color='gray', size=0.4, alpha=0.2) +
        geom_line(mapping=aes(y=signif), color="red", data=df, linetype="dashed", size=0.6) +
        geom_line(mapping=aes(y=sim_mean), color="black", data=df, size=0.6) +
        geom_line(mapping=aes(y=obs), color="blue", data=df, size=0.6) +
        labs(x="Period (years)", y=expression(paste("Power (", mm^2,")"))) +
        scale_x_continuous(breaks=seq(0,50,5), expand=c(0,0)) +
        scale_y_log10(limits = c(5*10**2,5*10**5),labels = comma)
    }


  }


