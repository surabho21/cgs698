
# question 1.1

number_crossings <- function(alpha,beta,len){
  lambda <- exp(alpha + beta*len)
  number_cross <- rpois(1,lambda)
}

# question 1.2


y_prior <- rep(NULL,1000)
for(i in 1:1000){
 alpha <- rnorm(1,0.15,0.1)
  beta <- rnorm(1,0.25,0.05)
  y_prior[i] <- number_crossings(alpha,beta,4)
  
}
hist(y_prior)

# question 1.3

getwd()
setwd("c:/csv files")
getwd()



# Load required libraries
library(brms)

# Load the data
crossings <- read.csv("crossings.csv")
for(i in 1:1900){
  if(crossings$Language[i]=='German'){
    crossings$Language[i]<- 1
  }
  else{
    crossings$Language[i]<- 0
  }
}



formula_M1 <- bf(nCross ~ 1+s.length, family = poisson(link = 'log'))
# Set priors
priors_M1 <- c(
  prior(normal(0.15, 0.1), class = Intercept),
  prior(normal(0, 0.15), class = b)
)
# Fit the model
fit_M1 <- brm(formula_M1, data = crossings, prior = priors_M1)

# Model M2: Rate of crossings depends on sentence length and language
# Define the formula
formula_M2 <- bf(nCross ~ 1 + s.length + Language + s.length*Language, family = poisson(link='log'))
# Set priors

priors_M2 <- c(
  prior(normal(0.15, 0.1), class = Intercept),
  prior(normal(0,0.15), class = b)
  
)
# Fit the model
fit_M2 <- brm(formula_M2, data = crossings, prior = priors_M2)

# Summarize the models
summary(fit_M1)
summary(fit_M2)

mcmc_plot(fit_M1,pars = c("intercept","b"))

mcmc_plot(fit_M2,pars = c("intercept","b"))

#question 1.4



observed <- read.table("crossings.csv",sep=",",header=T)
# Visualize average rate of crossings
observed %>% group_by(Language,s.length) %>%
  summarise(mean.crossings=mean(nCross)) %>%
  ggplot(aes(x=s.length,y=mean.crossings,
             
             group=Language,color=Language))+
  geom_point()+geom_line()
# Code/center the predictors
observed$s.length <- observed$s.length - mean(observed$s.length)
observed$lang <- ifelse(observed$Language=="German",1,0)
# These two vectors will store log predictive desnities
# in each fold
lpds.m1 <- c()
lpds.m2 <- c()
untested <- observed
for(k in 1:5){
  # Prepare test data and training data
  ytest <- sample_n(untested,size=nrow(observed)/5)
  ytrain <- setdiff(observed,ytest)
  untested <- setdiff(untested,ytest)
  # Fit the models M1 and M2 on training data
  fit.m1 <-
    brm(nCross ~ 1 + s.length,data=ytrain,
        family = poisson(link = "log"),
        prior = c(prior(normal(0.15, 0.1), class = Intercept),
                  prior(normal(0, 0.15), class = b)),
        cores=4)
  fit.m2 <-
    brm(nCross ~ 1 + s.length + lang + s.length*lang,
        data=ytrain,
        family = poisson(link = "log"),
        prior = c(prior(normal(0.15, 0.1), class = Intercept),
                  prior(normal(0, 0.15), class = b)),
        cores=4)
  # retrieve posterior samples
  post.m1 <- posterior_samples(fit.m1)
  post.m2 <- posterior_samples(fit.m2)
  # Calculate log pointwise predcitive density using test data
  lppd.m1 <- 0
  lppd.m2 <- 0
  for(i in 1:nrow(ytest)){
    lpd_im1 <- log(mean(dpois(ytest[i,]$nCross,
                              
                              lambda=exp(post.m1[,1]+
                                           post.m1[,2]*ytest[i,]$s.length))))
    lppd.m1 <- lppd.m1 + lpd_im1
    lpd_im2 <- log(mean(dpois(ytest[i,]$nCross,
                              lambda=exp(post.m2[,1]+
                                           post.m2[,2]*ytest[i,]$s.length+
                                           post.m2[,3]*ytest[i,]$lang+
                                           post.m2[,4]*ytest[i,]$s.length*ytest[i,]$lang)
    )))
    lppd.m2 <- lppd.m2 + lpd_im2
  }
  lpds.m1 <- c(lpds.m1,lppd.m1)
  lpds.m2 <- c(lpds.m2,lppd.m2)
}
# Predictive accuracy of model M1
elpd.m1 <- sum(lpds.m1)
# Predictive accuracy of model M2
elpd.m2 <- sum(lpds.m2)
# Evidence in favor of M2 over M1
difference_elpd <- elpd.m2-elpd.m1


elpd.m1
elpd.m2
difference_elpd

