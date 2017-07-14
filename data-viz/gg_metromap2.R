

#### THICK POINTS (metro stations)

library(ggplot2)

# For shapes that have a border (like 21), you can colour the inside and
# outside separately. Use the stroke aesthetic to modify the width of the
# border

ggplot(mtcars, aes(wt, mpg)) +
  theme_light() +
  geom_point(shape = 21, colour = "black", fill = "white", size = 2, stroke = 2)


#### Rounding edges (pathways)

df = structure(list(x = c(10, 10.1, 10.2, 10.3, 10.4, 10.5, 10.6, 10.7,
  10.8,10.9), y = c(282.083195814139, 281.463374904196,
    280.846027959358,280.231142308826, 279.618705353623,
    279.008704566105, 278.401127489482, 277.795961737318,
    277.193194993064, 276.592815009577)),
  .Names = c("x", "y"), row.names = c(NA, 10L), class = "data.frame")


ggplot(df, aes(x, y)) + geom_path(lwd = 4, lineend = "round")



################################################################################

df1 <- data_frame(name  = "FOMI",
  stage = c(1,2,3,4), value = c(6160, 6160, 6160, 6160), sn = 1)

df2 <- data_frame(name  = "FOMI",
  stage = c(1,2,3,4), value = c(7500, 7500, 7500, 7500), sn = 2)

df3 <- data_frame(name  = "FOMI",
  stage = c(1,2,3,4), value = c(0, 6160, 6160, 6160), sn = 3)

df4 <- data_frame(name  = "FOMI",
  stage = c(1,2,3,4), value = c(0, 6160, 6160, 6160), sn = 4)

df5 <- data_frame(name = 'FOMI',
  stage = rep(0.9, 4), value = rep(0, 4), sn = c(1,2,3,4))

df <- bind_rows(df1, df2, df3, df4) %>%
  mutate(sn = as.factor(sn))

ggplot(df, aes(x = stage, y = value, group = sn, color = sn)) +
  theme_light()  +
  geom_step(lwd = 0.8, alpha = 0.5,
    mapping = aes(x = jitter(stage,0.05), y = jitter(value, 0.05),
      group=sn, col=sn)) +
  geom_point(shape = 21, colour = "black",
    fill = "white", size = 2, stroke = 2)

