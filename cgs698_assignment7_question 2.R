data_red <- read.csv("df_red.csv")


model_red <- brm(
  red ~ 1 + risk,
  data = data_red,
  family = bernoulli(link = logit),
  prior  = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept")
  )
)

model_pink <- brm(
  pink~1 + risk,
  data = data_red,
  family = bernoulli(link=logit),
  prior= c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept")
  )
)

model_redorpink <- brm(
  redorpink ~ 1+ risk,
  data = data_red,
  family = bernoulli(link=logit),
  prior  = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept")
  )
)

summary(model_red)
summary(model_pink)
summary(model_redorpink)

mcmc_plot(model_red, pars = c("b_Intercept","b_risk"))
mcmc_plot(model_pink, pars = c("b_Intercept","b_risk"))
mcmc_plot(model_redorpink, pars = c("b_Intercept","b_risk"))


