#question 2

yr <- c(300,270,390,450,500,290,680,450)

mu <- c(300,900,50)
unnorm_post_denstity <- rep(NA,3)
 for(i in 1:3){
unnorm_post_denstity[i] <- prod(dnorm(yr,mu[i],50))*dnorm(mu[i],250,25)

 }
unnorm_post_denstity
  


remove(mu)




mu <- rnorm(1000,250,25)
unnormalised_post_density <- rep(NA,1000)
for(i in 1:1000){
  unnormalised_post_density[i] <- prod(dnorm(yr,mu[i],50))*dnorm(mu[i],250,25)
}
mat <- data.frame(mu,unnormalised_post_density)

ggplot(mat,aes(x=mu,y=unnormalised_post_density))+geom_line(size=1,color="blue")+
  theme_bw()+xlab(expression(mu))+ylab("unnormalised_post_density")

prior_density_mu <- rep(NA,1000)
for(i in 1:1000){
  prior_density_mu[i] <- dnorm(mu[i],250,25)
}
 
mat1 <- data.frame(mu,prior_density_mu)
ggplot(mat1,aes(x=mu,y=prior_density_mu))+geom_line(size=1,color="blue")+
  theme_bw()+xlab(expression(mu))+ylab("prior_density_mu")




