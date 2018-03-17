


############# Plot storage level & targets

df <- data_frame(date = sim_date, year = year(sim_date),
  jday = yday(date), Storage = S) %>%
  left_join(resops_volume_daily, by = "jday")

p1 <- ggplot(df, aes(x = date, y = Storage)) +
  geom_line(color = "blue") +
  geom_line(aes(y = min), linetype = "dashed") +
  geom_line(aes(y = max), linetype = "dashed")

p2 <- ggplot(df, aes(x = jday, y = Storage, group = year)) +
  geom_line(color = "blue") + 
  geom_line(aes(y = min), linetype = "dashed") +
  geom_line(aes(y = max), linetype = "dashed")

p1; p2







ggplot(op_levels, aes(x = day)) +
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.3) +
  scale_x_continuous(breaks = jdays_mid_of_month, labels = month.abb) +
  theme_light() +
  scale_y_continuous(limits = c(1320, 1440)) +
  geom_hline(yintercept = c(1330, 1430), linetype = "dashed") +
  labs(x = "", y = "water level (m.asl.)")

  


