

#question 2.1


ML_binomial <- function(k,n,a,b){
  ML <- (factorial(n)/(factorial(k)*factorial(n-k)))*(
    factorial(k+a-1)*factorial(n-k+b-1)/factorial(n+a+b-1))
  ML
}

ML_binomial(2,10,0.1,0.4)
ML_binomial(2,10,1,1)
ML_binomial(2,10,2,6)
ML_binomial(2,10,6,2)
ML_binomial(2,10,20,60)
ML_binomial(2,10,60,20)

# using monte carlo integrator

ML_monte.carlo <- function(k,n,a,b){
  likelihood <- rep(NULL,1000)
  for(i in 1:1000){
    theta <- rbeta(1,a,b)
    likelihood[i] <- dbinom(2,10,theta)
  }
  ML <- mean(likelihood)
  ML
}

ML_monte.carlo(2,10,0.1,0.4)
ML_monte.carlo(2,10,1,1)
ML_monte.carlo(2,10,2,6)
ML_monte.carlo(2,10,6,2)
ML_monte.carlo(2,10,20,60)
ML_monte.carlo(2,10,60,20)




