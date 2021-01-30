


#' Compare Time-series of variables from two datasets accross multiople stations
#'
#' @param data1
#' @param data2
#' @param data1.label
#' @param data2.label
#' @param time.step
#' @param var
#' @param var.lab
#' @param locations
#' @param show.title
#' @param show.r2
#' @param show.pval
#'
#' @return
#' @export
#'
#' @examples
#' 
#' #mydata_wide <- tibble(
#'  Date = as.Date("2015-01-01") + c(0:119) * months(1),
#'  First = rnorm(120, mean = 50, sd = 10),
#  Second = rnorm(120, mean = 80, sd = 20),
#  Third = rnorm(120, mean = 10, sd = 5))
#
#  mydata <- mydata_wide %>% gather(key = Source, value = value, -Date)
#' 
#' 
#' 
#' 
#' 
#' 
#' 


ggQQPlot <- function(data1 = NULL,
                                        data2 = NULL,
                                        data1.label = NULL,
                                        data2.label = NULL,
                                        var = NULL,
                                        var.lab = NULL,
                                        locations = NULL,
                                        show.title = FALSE,
                                        show.r2 = TRUE,
                                        show.pval = FALSE)
  
{


  df1 <- data1 %>% select(date, location, df1 = all_of(var)) %>%
    mutate(df1 = as.numeric(df1)) %>%
    filter(location %in% locations)
  
  df2 <- data2 %>% select(date, location, df2 = all_of(var)) %>%
    mutate(df2 = as.numeric(df2)) %>%
    filter(location %in% locations)
  
  df <- df1 %>%
    left_join(df2, by = c("date", "location")) %>%
    na.omit() %>%
    droplevels()
  
  yposSet <- function(x,y) {
    max(c(x,y))  + .3 * sd(c(x,y))
  }
  
  xposSet <- function(x,y) {
    min(x,y)  + .3 * sd(c(x,y))
  }
  
  # Statistics
  lm_stats <- df %>%
    group_by(location) %>%
    nest() %>%
    mutate(
      model = map(.x = data, ~ lm(df1 ~ df2, data = .x)),
      summary = map(model, summary),
      `r-squared`  = map_dbl(summary, "r.squared") %>% round(2),
      `p-value`  = map_dbl(model, lmp) %>% round(3),
      `p-value` = ifelse(`p-value` < 0.05, "<0.05", `p-value`),
      R_squared = paste0("r^2:", `r-squared`),
      p_value = paste0("pval:", `p-value`),
      ypos = map_dbl(.x=data, .f=~yposSet(x=.x$df1, y =.x$df2)),
      xpos = map_dbl(.x=data, .f=~xposSet(x=.x$df1, y =.x$df2))
    )
  
  # Calculate panel axis limits
  foo1 <- df %>% group_by(location) %>%
    summarize(x = max(c(df1,df2)), y = min(c(df1, df2)))
  
  foo2 <- foo1
  foo2$x <- foo1$y
  foo2$y <- foo1$x
  foo3 <- foo1
  foo3$y <- foo1$x
  foo4 <- foo1
  foo4$x <- foo1$y
  foo <- bind_rows(foo1, foo2, foo3, foo4) 
  
  text_size = 12

  
  p <- ggplot(df, aes(x = df1, y = df2)) +
    theme_bw(base_size = text_size) +
    geom_blank(data = foo, aes(x = x, y = y)) +
    geom_point(alpha = 0.3, size = 1) +
    facet_wrap(. ~ location, nrow = 3, scales = "free") +
    geom_abline(intercept = 0, color = "blue") +
    labs(x = data1.label, y = data2.label)  
    #scale_x_continuous(expand = c(0, 0)) +
    #scale_y_continuous(expand = c(0, 0))
  
  if (show.r2 == TRUE) {
    p <-
      p +  geom_text(
        data = lm_stats,
        aes(x = xpos, y = ypos, label = R_squared),
        color = 'blue',
        parse = TRUE,
        hjust = 0,
        size = 4
      )
  }
  
  if (show.pval == TRUE) {
    p <-
      p +  geom_text(
        data = lm_stats,
        aes(
          x = xpos,
          y = ypos * 0.9,
          label = p_value
        ),
        color = 'blue',
        parse = FALSE,
        hjust = 0,
        size = 4
      )
  }
  
  if (show.title == TRUE) {
    p <- p +   ggtitle(label = var.lab)
  }
  
  p
  
}




