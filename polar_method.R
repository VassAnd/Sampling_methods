PolarMethod<-function(N)
{

  x<-numeric(N)
  y<-numeric(N)
  z<-numeric(N)

  i<-1

  while(i<=N)
  {u1<-runif(1)
  u2<-runif(1)
  v1<-(2*u1)-1
  v2<-(2*u2)-1
  s<-(v1^2)+(v2^2)

  if(s<=1)
  {
    x[i]<-((-2*log(s)/s)^(1/2))*v1
    y[i]<-((-2*log(s)/s)^(1/2))*v2
    z[i]<-(x[i]+y[i])/sqrt(2) #standarization
    i<-i+1
  }
  else
    i<-i-1
  }

  return(z)
}
z<-PolarMethod(10000)
hist(z,freq=F,ylab="Density",xlab=" z values")
curve(dnorm(x),from=-3,to=3,add=TRUE)
