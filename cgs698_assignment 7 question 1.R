getwd()
setwd("c:/csv files")
getwd()

df.powerpose <- read.csv("df_powerpose.csv")

df.powerpose$x1 <- rep(NULL,39)
df.powerpose$diff <- df.powerpose$testm2-df.powerpose$testm1
for(i in 1:39){
  if(df.powerpose$hptreat[i]=='High'){
    df.powerpose$hptreat[i]<- 1
  }
  else{
    df.powerpose$hptreat[i] <- 0
  }
}
priors <- default_prior(diff ~ 1+hptreat,data= df.powerpose)
mfit <-
  brm(formula = diff~ 1+hptreat,
      data=df.powerpose,
      family = gaussian(),
      prior = priors,
      chains = 4,cores = 4,
      iter = 2000,warmup = 1000)

summary(mfit)

mcmc_plot(mfit,pars = c("b_Intercept","b_hptreat"))
