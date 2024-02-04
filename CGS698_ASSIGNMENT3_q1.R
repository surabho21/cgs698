 
#question 1
y <- 7
theta <- seq(from=0,to=1,by=0.001)
 length(theta)

post_density <- rep(NA,1001)
max.post <- 0
theta.max <- 0
for(i in 1:1001){
  if (theta[i]<0.5 | theta[i]>1){
    post_density[i] <- 0
  }
  else{
    post_density[i] <- ((dbinom(7,10,theta[i]))*2*1408)/227
  } 
  if (post_density[i]>max.post){
    max.post <- post_density[i]
    theta.max<- theta[i]
  }
}

 
plot(theta,post_density)
theta.max
likelihood <- rep(NA,1001)
for(i in 1:1001){
  likelihood[i]<- dbinom(7,10,theta[i])
}
plot(theta,likelihood)

prior_dist <- rep(NA,1001)
for(i in 1:1001) {
  if(theta[i]<0.5 | theta[i]>1){
    prior_dist[i]<- 0
  } else{
    prior_dist[i]<- 2
  }
}
plot(theta,prior_dist)
