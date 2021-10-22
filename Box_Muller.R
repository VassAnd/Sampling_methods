## Box - Muller sampling method
set.seed <- c(2442)
n.sim = 15000
N12 <- matrix(0, ncol=2, nrow=n.sim)
for (i in 1:n.sim) {
  u1 = runif(1)
  u2 = runif(1)
  theta = 2*pi*u2
  R = sqrt(-2*log(u1))
  N12[i,1] = R*cos(theta)
  N12[i,2] = R*sin(theta) 
}
mean(N12); sd(N12)
#
plot(density(N12))
plot(N12)
#-------- compare with rnorm -----------------------------------
n <- rnorm(15000)
mean(n);sd(n)
#-------- compare plots Box-Muller vs rnorm --------------------
plot(density(N12),col="2", main=" Box - Muller ìÝèïäïò\n n.sim = 15000 æåýãç ôéìþí")
lines(density(n), col="4")
legend(1.6,0.38, legend=c("Box-Muller", "rnorm"),col=c(2, 4),lty=1 ,cex=0.8)
