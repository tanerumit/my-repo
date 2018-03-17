
# Steps to take prior to time-series analysis

# Is normality a reasonable assumption for the residuals?
# Are the data stationary?


# 1. CHECK FOR NORMALITY 

# Visual inspections:
library(waterData)

q05054000 <- importDVs("05054000", code="00060", stat = "00003",
  sdate = "2000-01-01", edate = "2010-12-31")

dat <- q05054000 %>% as_tibble() %>% select(date = dates, value = val)

#>> Histogram 
p <- ggplot(dat, aes(x = value)) + 
  geom_histogram(aes(y=..density..), position="identity") + 
  geom_density(aes(y=..density..), color = "blue") +
  labs(x = "Flow (cfs)", y = "density") 

#>> Q-Q plot
p <- ggplot(dat, aes(sample = value)) + stat_qq()







There are also specific methods for testing normality but these should be used in conjunction with
either a histogram or a Q-Q plot. The Kolmogorov-Smirnov test and the Shapiro-Wilk’s W test
whether the underlying distribution is normal. Both tests are sensitive to outliers and are
influenced by sample size:
• For smaller samples, non-normality is less likely to be detected but the Shapiro-Wilk test
should be preferred as it is generally more sensitive
• For larger samples (i.e. more than one hundred), the normality tests are overly conservative
and the assumption of normality might be rejected too easily (see robust exceptions below).


Hypothesis test for a test of normality
Null hypothesis: The data is normally distributed. If p> 0.05, normality can be assumed.
For both of these examples, the sample size is 35 so the Shapiro-Wilk test should be used.
shapiro.test(normal)
shapiro.test(skewed)




# Normality tests




# Data transformation methods



# Log Transform:




# Box-Cox Transform:

