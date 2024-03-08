
# part 1
#analytically derived posterior distribution

theta <- seq(from = 0 ,to = 1, length=50000)

analytic.post.dist <- rbeta(length(theta),135,67) 
 
hist(analytic.post.dist)
plot(analytic.post.dist)

# grid approximation
y <- c(10, 15, 15, 14, 14, 14, 13, 11, 12, 16) 
marginal.likelihood <- 0
for(i in 1:length(theta)){
  marginal.likelihood <- marginal.likelihood + prod(dbinom(y,20,theta[i]))*dbeta(theta[i],1,1)
}
 
grid_analytical_post_dist <- rep(NA,length(theta))
 for(i in 1:length(theta)){
   grid_analytical_post_dist[i]<- prod(dbinom(y,20,theta[i]))*dbeta(theta[i],1,1)/marginal.likelihood
 }
plot(theta,grid_analytical_post_dist)

#monte carlo integration

theta_drawn <- rbeta(50000,1,1)
likelihood <- rep(NA,50000)
for(i in 1:50000){
  likelihood[i] <- prod(dbinom(y,20,theta_drawn[i]))
}
marginal.likelihood.montecarlo <- mean(likelihood)

marginal.likelihood.montecarlo

#importance sampling

#proposal_density be the analytical posterior density

proposal_theta_drawn <- rbeta(200000,135,67)
likelihood_impt <- rep(NA,200000)
prior <- rep(NA,200000)
q <- rep(NA,200000)
w <- rep(NA,200000)
for(i in 1:200000)
{
  likelihood_impt[i] <- prod(dbinom(y,20,proposal_theta_drawn[i]))
  prior[i] <- dbeta(proposal_theta_drawn[i],1,1)
  q[i]  <- dbeta(proposal_theta_drawn[i],135,67)
  w[i] <- likelihood_impt[i]*prior[i]/q[i]
}

posterior <- sample(proposal_theta_drawn,size=50000,prob=w)

hist(posterior)
 

#markov_chain_montecarlo

# Markov chain
nsamp <- 50000
theta_chain <- rep(NA,nsamp)
#Initialization of Markov chain

theta_chain[1] <- rbeta(1,1,1)
#Evolution of Markov chain
i <- 1
reject <- 0
step <- 0.0008 # step-size for proposal distribution
while(i<nsamp){
  #Sample from proposal distribution
  proposal_theta <- rnorm(1,theta_chain[i],step)
  if(proposal_theta>0&proposal_theta<1){
    # Compute prior*likelihood
    post_new <- prod(dbinom(y,20,proposal_theta))*dbeta(proposal_theta,1,1)
    post_prev <- prod(dbinom(y,20,theta_chain[i]))*dbeta(theta_chain[i],1,1)
    #Compute Hastings ratio
    Hastings_ratio <- (post_new*dnorm(theta_chain[i],proposal_theta,step))/
      (post_prev*dnorm(proposal_theta,theta_chain[i],step))
    p_str <- min(Hastings_ratio,1) # probability of acceptance
    if(p_str>runif(1,0,1)){
      theta_chain[i+1] <- proposal_theta
      i <- i+1
    }else{
      reject <- reject+1
    }
  }
}
hist(theta_chain)


#comparision 
par(mfrow = c(1,3))
hist(analytic.post.dist)
hist(theta_chain)
hist(posterior)











