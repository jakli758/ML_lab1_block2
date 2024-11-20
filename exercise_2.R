set.seed(1234567890)
max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log lik between two consecutive iterations
n=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=n, ncol=D) # training data
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")
# Producing the training data
for(i in 1:n) {
  m <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[i,d] <- rbinom(1,1,true_mu[m,d])
  }
}

M=3 # number of clusters
w <- matrix(nrow=n, ncol=M) # weights
pi <- vector(length = M) # mixing coefficients
mu <- matrix(nrow=M, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations
# Random initialization of the parameters
pi <- runif(M,0.49,0.51)
pi <- pi / sum(pi)
for(m in 1:M) {
  mu[m,] <- runif(D,0.49,0.51)
}

pi
mu



for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  Sys.sleep(0.5)
  # E-step: Computation of the weights
  # Your code here
  for (i in 1:n){
    for (m in 1:M){
      bern <- 1
      for (d in 1:D){
        bern_d <- mu[m,d]^(x[i,d]) * (1-mu[m,d])^(1-x[i,d])
        if (bern_d < 0){
          # print(bern_d)
        }
        bern <- bern * bern_d
      }
      w[i,m] <- pi[m] * bern
    }
    w[i,] <- w[i,]/(sum(w[i,]))
  }
  
  #Log likelihood computation.
  # Your code here
  #llik[it] <- sum(log(colSums(w)))
  llik[it] <- sum(colSums(log(w)))
  
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console()
  # Stop if the log likelihood has not changed significantly
  # Your code here
  if (it > 1){
    if (abs(llik[it] - llik[it-1]) <= 0.001){
      cat("No significant change in likelihood:", llik[it-1], "to", llik[it])
      break
    }
  }
  #M-step: ML parameter estimation from the data and weights
  # Your code here
  for (m in 1:M){
    # update pis
    pi[m] <- sum(w[,m]) / n
    
    # update mus
    weighted_x <- vector(length=D)
    for (i in 1:n){
       weighted_x <- weighted_x + sum(w[i,m]) * x[i,]
    }
    mu[m,] <- 1/sum(w[,m]) * weighted_x
  }
  
  #w <- matrix(nrow=n, ncol=M) # weights
  #pi <- vector(length = M) # mixing coefficients
  #mu <- matrix(nrow=M, ncol=D) # conditional distributions
 
}

mu
weighted_x
x[i,]
w[,1:2]
bern
sum(w[,2])
pi
mu
plot(llik[1:it], type="o")
x
colSums(w)
sum(w[,1])
w


