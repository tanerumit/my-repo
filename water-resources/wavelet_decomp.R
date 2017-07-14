

LOW_FREQUENCY_COMPONENTS <- array(0,c(length(CLIMATE_VARIABLE),NUM_FINAL_PERIODS))
for (i in 1:NUM_FINAL_PERIODS) {
  CUR_PERIODS <- ALL_SIG_PERIODS[1:NUM_PERIODS_ALL_COMPS[i]]
  if (i>1) {CUR_PERIODS <- ALL_SIG_PERIODS[(1 + (i-1)*NUM_PERIODS_ALL_COMPS[i-1]):(NUM_PERIODS_ALL_COMPS[i] + (i-1)*NUM_PERIODS_ALL_COMPS[i-1])]}
  sj <- scale[CUR_PERIODS]
  #for Morlet Wavelet with freq = 6
  Cdelta <- .776
  w0_0 <- pi^(-1/4)
  if (length(CUR_PERIODS)>1) {LOW_FREQUENCY_COMPONENTS[,i] <- apply(sd(CURRENT_CLIMATE_VARIABLE_org)*(dj*sqrt(dt)/(Cdelta*w0_0))*Re(wave)[CUR_PERIODS,]/sqrt(sj),FUN=sum,c(2))}
  if (length(CUR_PERIODS)==1) {LOW_FREQUENCY_COMPONENTS[,i] <- sd(CURRENT_CLIMATE_VARIABLE_org)*(dj*sqrt(dt)/(Cdelta*w0_0))*Re(wave)[CUR_PERIODS,]/sqrt(sj)}
}

NOISE <- CURRENT_CLIMATE_VARIABLE_org - apply(LOW_FREQUENCY_COMPONENTS,FUN=sum,c(1))
