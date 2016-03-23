

##clopper-pearson

N=999
coverageCP=rep(0,N)
for (p in 1:N){
  pi=p/(N+1)
  cov=0
  for (x in 0:25){
    a=binom.test(x, n=25, conf.level=.95)
    int=a$conf.int[1:2]
    if (pi>=int[1]&pi<=int[2]){
      cov=cov+dbinom(x,25, pi)
    }
  }
  coverageCP[p]=cov
}

#wald



coverageWald=rep(0,N)
for (p in 1:N){
  pi=p/(N+1)
  cov=0
  for (x in 0:25){
    int=rep(0,2)
    thetaHat=x/25
    int[1] = thetaHat-1.96*sqrt(thetaHat*(1-thetaHat)/25)
    int[2] = thetaHat+1.96*sqrt(thetaHat*(1-thetaHat)/25)
#     a=binom.test(x, n=25, conf.level=.95)
#     int=a$conf.int[1:2]
    if (pi>=int[1]&pi<=int[2]){
      cov=cov+dbinom(x,25, pi)
    }
  }
  coverageWald[p]=cov
}


##Score
coverageScore=rep(0,N)
for (p in 1:N){
  pi=p/(N+1)
  cov=0
  for (x in 0:25){
    int=rep(0,2)
    thetaHat=x/25
    z=1.96
    n=25
    int[1] = 1/(1+z^2/n)*(thetaHat+z^2/(2*n)-z*sqrt(thetaHat*(1-thetaHat)/n+z^2/(4*n^2)))
    int[2] = 1/(1+z^2/n)*(thetaHat+z^2/(2*n)+z*sqrt(thetaHat*(1-thetaHat)/n+z^2/(4*n^2)))
    if (pi>=int[1]&pi<=int[2]){
      cov=cov+dbinom(x,25, pi)
    }
  }
  coverageScore[p]=cov
}
# plot(coverageScore, type="l")
prob=seq(0.001,.999,.001)
plot(prob,coverageCP,type="l",col="red", ylim=c(.7,1))
lines(prob,coverageWald,col="green")
lines(prob,coverageScore, col="blue")
lines(prob,rep(.95,N))
legend("bottom", c("clopper-pearson", "wald", "score"), lty=c(1,1), col=c("red", "green", "blue"))
