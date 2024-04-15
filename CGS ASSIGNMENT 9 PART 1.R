
y <- c(10, 15, 15, 14, 14, 14, 13, 11, 12, 16)

# question 1.1

# posterior predictions for model 1
y_m1 <- rep(NULL,1000)

for(i in 1:1000){
  theta_posterior <- rbeta(1,6+sum(y), 6 + 10*20 - sum(y))
  y_m1[i] <- rbinom(1,20,theta_posterior)
  
}
hist(y_m1)

#posterior predictions for model 2

y_m2 <- rep(NULL,1000)
for(i in 1:1000){
  theta_posterior2 <- rbeta(1,20+sum(y), 60 + 10*20 - sum(y))
  y_m2[i] <- rbinom(1,20,theta_posterior2)
  
}
hist(y_m2)



#question 1.2


lpd_m1 <- rep(NA,10)
for(i in 1:10){
 
  sample_theta <- rbeta(1000,6+sum(y),6+(length(y)*20)-sum(y))
  lppd_i <- 0
  for(j in 1:10){
    lpd_ij <- log(mean(dbinom(y_m1[j],20,sample_theta)))
    lppd_i <- lppd_i + lpd_ij
  }
  lpd_m1[i] <- lppd_i
}

lpd_m2 <- rep(NA,10)
for(i in 1:10){
 
  sample_theta <- rbeta(1000,20+sum(y),60+(length(y)*20)-sum(y))
  lppd_i <- 0
  for(j in 1:10){
    lpd_ij <- log(mean(dbinom(y_m2[j],20,sample_theta)))
    lppd_i <- lppd_i + lpd_ij
  }
  lpd_m2[i] <- lppd_i
}

llpd_m1 <- sum(lpd_m1)
llpd_m1

llpd_m2 <- sum(lpd_m2)
llpd_m2

#question 1.3

dev_m1 <- -2*llpd_m1
dev_m1


dev_m2 <- -2*llpd_m2

dev_m2


#question 1.4

#model 2 fits better

#question 1.5

y_new <- c(5, 6, 10, 8, 9)


lpd_m1 <- rep(NA,10)
for(i in 1:10){
  
  sample_theta <- rbeta(1000,6+sum(y),6+(length(y)*20)-sum(y))
  lppd_i <- 0
  for(j in 1:5){
    lpd_ij <- log(mean(dbinom(y_new[j],20,sample_theta)))
    lppd_i <- lppd_i + lpd_ij
  }
  lpd_m1[i] <- lppd_i
}

lpd_m2 <- rep(NA,10)
for(i in 1:10){
  
  sample_theta <- rbeta(1000,20+sum(y),60+(length(y)*20)-sum(y))
  lppd_i <- 0
  for(j in 1:5){
    lpd_ij <- log(mean(dbinom(y_new[j],20,sample_theta)))
    lppd_i <- lppd_i + lpd_ij
  }
  lpd_m2[i] <- lppd_i
}

llpd_m1.1 <- sum(lpd_m1)
llpd_m1.1

llpd_m2.2 <- sum(lpd_m2)
llpd_m2.2


out_dev1 <- -2*llpd_m1
out_dev1

out_dev2 <- -2*llpd_m2.2
out_dev2

#model 2 fits better


#question 1.6

dat <- data.frame(y)
N_obs <- 10
elpd_m1 <- 0
for(i in 1:N_obs){
  ytrain <- dat$y[-i]
  ytest <- dat$y[i]
  sample_theta <- rbeta(1000,6+sum(ytrain),6+(N_obs-1)*20-sum(ytrain))
  lpd_i <- log(mean(dbinom(ytest,20,sample_theta)))
  elpd_m1 <- elpd_m1 + lpd_i
}
elpd_m2 <- 0

for(i in 1:N_obs){
  ytrain <- dat$y[-i]
  ytest <- dat$y[i]
  sample_theta <- rbeta(1000,20+sum(ytrain),60+(N_obs-1)*20-sum(ytrain))
  lpd_i <- log(mean(dbinom(ytest,20,sample_theta)))
  elpd_m2 <- elpd_m2 + lpd_i
}
elpd_m1
elpd_m2














