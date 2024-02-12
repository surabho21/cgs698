#QUESTION 1
lambda <- rgamma(1000,shape=135,scale=1/3)

fifth_day <- rep(NA,1000)

for(i in 1:length(lambda)){
  fifth_day[i]  <- rpois(1,lambda[i])
}


mean(fifth_day)

#QUESTION 2


dat <- read.table(
  "https://raw.githubusercontent.com/yadavhimanshu059/CGS698C/main/notes/Module-2/recognition.csv",
  sep=",",header = T)[,-1]
head(dat)


#null-hypothesis posterior distribution

mu <- rnorm(1000,300,50)
 
post_density <- rep(NA,1000)
 
for(i in 1:length(mu)){
  post_density[i] <- prod(dnorm(dat$Tw,mu[i],60))* prod(dnorm(dat$Tnw,mu[i],60))*dnorm(mu[i],300,50)
}

data <- data.frame(mu,post_density)

ggplot(data,aes(x=mu,y=post_density))+geom_line(size=1,color="blue")+
  theme_bw()+xlab(expression(mu))+ylab("post_density")



#lexical_access model prediction
 install.packages("truncnorm")
library(truncnorm)
 
 
 delta <- rtruncnorm(1000,a=0, b=Inf, mean=0, sd=50)
 word_time <- rep(NA,1000)
 nonword_time <- rep(NA,1000)
 
 for(i in 1:length(mu) ){
   word_time[i] <- rnorm(1,mu[i],60)
   nonword_time[i] <- rnorm(1,mu[i]+delta[i],60)
 }
 
 
 par(mfrow = c(1,2))
 
 
 hist(word_time)
hist(nonword_time)



#NULL HYPO VS LEXICAL ACCESS

null.word_time <- rep(NA,1000)
null.nonword_time <- rep(NA,1000)

for(i in 1:length(mu)){
  null.word_time[i] <- rnorm(1,mu[i],60)
  null.nonword_time[i] <- rnorm(1,mu[i],60)
}

#for word time it will be same for both models
#comparing for non words

hist(nonword_time)
hist(null.nonword_time)


#actual vs NULL hypothesis
par(mfrow = c(1,4))
hist(dat$Tw) 
hist(dat$Tnw)
hist(null.word_time)
hist(null.nonword_time)
 
#actual vs lexical acess
hist(dat$Tw) 
hist(dat$Tnw)
hist(word_time)
hist(nonword_time)

#unnormalized posterior for lexical access


lexi.post_density <- rep(NA,1000)

for(i in 1:length(mu)){
 lexi.post_density[i] <- prod(dnorm(dat$Tw,mu[i],60))* prod(dnorm(dat$Tnw,mu[i]+delta[i],60))*dnorm(mu[i],300,50)*dtruncnorm(delta[i],a=0,b= Inf,mean=0,sd=50)
}

data1 <- data.frame(mu,lexi.post_density)

ggplot(data,aes(x=mu,y=lexi.post_density))+geom_line(size=1,color="blue")+
  theme_bw()+xlab(expression(mu))+ylab("post_density")






