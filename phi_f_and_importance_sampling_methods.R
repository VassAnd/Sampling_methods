# sampling methods :a) phi and f ; b) importance sampling method
#
# a) phi and f 
# all the parameters for this method have an index of 1
# phi(x) is the indicator function
# f(x) is the pdf of Cauchy disribution

set.seed(2)
phi <- function(x)  # setup an indicator function
  {
    as.numeric(x>=2)
  }
n.sim1 <- 100
k1 <- 10
theta_hut1 <- rep(NA,k1)
for(i in 1:k1)
{
  x1 <- rcauchy(n.sim1)
  theta_hut1[i] <- mean(phi(x1))   # calc of È hut integral
}
mean(theta_hut1);sd(theta_hut1);var(theta_hut1)  # MC estimators
## 0.146       # mean
## 0.0275681   # standard deviation
## 0.00076     # variance 

#
sqrt(var(theta_hut1))  # for check, must be the standard deviation
## 0.0275681

# b) importance sampling method
# all the parameters for this method have an index of 2
# h(x) is the indicator function
# f(x) is the pdf of Cauchy disribution
# g(x) is the 2/x^2 function, mimics the shape of Cauchy

set.seed(2)
h <- function(x)  # setup an indicator function
  {
    as.numeric(x>=2)
  }
n.sim2 <- 100
k2 <- 10
theta_hut2 <- rep(NA,k2)
for(i in 1:k2)
{
  u2 <- runif(n.sim2) # draws fron Uniform(0,1)
  x2 <- 2/u2  # inverse transform sampling method
  theta_hut2[i] <- mean(h(x2)*(x2^2)/(2*pi*(1+x2^2)))  # calc of È hut integral
}
mean(theta_hut2);sd(theta_hut2);var(theta_hut2)
## 0.1475283       # mean
## 0.0008760867    # standard deviation
## 7.67528e-07     # variance 

sqrt(var(theta_hut2))  # for check, must be the standard deviation
## 0.0008760867

# ------------ and some plots -----------------------------
par(mfrow=c(2,1))
plot(density(theta_hut1),col="4", main=' phi-f method\n theta hut estimation')
abline(v = mean(theta_hut1), lty = 2, col = "1")
plot(density(theta_hut2),col="2", main=' importance sampling\n theta hut estimation')
abline(v = mean(theta_hut2),  lty = 2, col = "1")
# -------------------------------------------------------------------------
