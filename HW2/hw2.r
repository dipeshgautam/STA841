
#3.1
dose = seq(0,5)
mortality = c(0/7, 2/9, 3/8, 5/7, 7/9, 10/11)
weights= c(7,9,8,7,9,11)

data3 = data.frame(dose=dose, mortality=mortality, weights=weights)

plot(data3$dose, data3$mortality, pch=16, col=2, xlab="log dose", ylab="mortality y/n")

#3.2
probit.fit = glm(mortality~dose, family=binomial(link=probit),
                 data=data3, weights=data3$weights)
summary(probit.fit)
fitted_values=fitted.values(probit.fit)
plot(data3$dose, data3$mortality, pch=16, col=2, ylim=c(-.1,1))
points(fitted_values, pch=17, col=3, xlab="log dose", ylab="mortality y/n")
legend("topleft", c("observed","fitted"), col = c(2,3), pch=c(16,17) )


#3.3


set.seed(1000)
beta_hat = coef(probit.fit)

log_LD_50 = -beta_hat[1]/beta_hat[2]

sigma_hat = vcov(probit.fit)
sigma11= sigma_hat[1,1]
sigma22= sigma_hat[2,2]
sigma12= sigma_hat[1,2]

t = 1.96
b0 = beta_hat[1]
b1 = beta_hat[2]

a = b1^2 - t^2*sigma22
b = -2*(b0*b1-t^2*sigma12)  #check the 1.96 part
c = b0^2-t^2*sigma11

CI = -(-b+c(1,-1)*sqrt(b^2-4*a*c))/(2*a)


#3.4

set.seed(1000)
N = 1000
r.boot = double(N)
mortality.boot = double(6)
est.boot = matrix(NA, nrow=N, ncol=2)

for (i in 1:N){
  for (j in 1:length(data3$mortality)){
    mortality.boot[j] = sum(rbinom(weights[j], 1, mortality[j]))/weights[j]    
  }
  data3.boot = data.frame(mortaliy=mortality.boot, dose=dose, weights=weights)
  data3.boot
  probit.fit.boot = glm(mortality.boot~dose, family=binomial(link=probit),
                        data = data3.boot, weights=weights)
  beta_hat = coef(probit.fit.boot)
  beta_hat
  est.boot[i,] = beta_hat
  r.boot[i] = -beta_hat[1]/beta_hat[2]
  
}

CI.boot = quantile(r.boot, c(.025,.975))
CI.boot


#3.5

set.seed(1000)
dose = seq(0,5)
mortality = c(0/7, 2/9, 3/8, 5/7, 7/9, 10/11)
weights= c(7,9,8,7,9,11)

data3 = data.frame(dose=dose, mortality=mortality, weights=weights)

library(truncnorm)
library(mvtnorm)

Pi = mortality

y1= rbinom(weights[1], 1, Pi[1])
y2= rbinom(weights[2], 1, Pi[2])
y3= rbinom(weights[3], 1, Pi[3])
y4= rbinom(weights[4], 1, Pi[4])
y5= rbinom(weights[5], 1, Pi[5])
y6= rbinom(weights[6], 1, Pi[6])
Y =c(y1,y2,y3,y4,y5,y6)

X2=rep(dose,weights)
M = length(Y)
X1 = rep(1, M)

X = cbind(X1,X2)


sigma0 = matrix(c(1,0,0,1), nrow=2)
beta0= c(1,1)
N = 1000

#Initial Z
Z = rep(NA,M)
for (i in 1:M){
  Z[i] = rnorm(1, X[i,]*beta0, 1)
}

BETA = matrix(rep(NA, N*2), nrow=N)
sigma_hat = solve(solve(sigma0)+t(X)%*%X)
beta_hat = sigma_hat%*%(solve(sigma0)%*%beta0+t(X)%*%Z)

for (i in 1:N){
  beta = rmvnorm(1, beta_hat, sigma_hat)
  for (j in 1:M){
    if (Y[j]==1){
      Z[j] = rtruncnorm(1,a=0, b=Inf, mean= X[j,]%*%t(beta), sd= 1 )
    } else if(Y[j]==0){
      Z[j] = rtruncnorm(1,a=-Inf, b=0, mean= X[j,]%*%t(beta), sd= 1 ) 
    }
  }
  BETA[i,] = beta
  beta_hat = sigma_hat%*%(solve(sigma0)%*%beta0+t(X)%*%Z)
}

LD.50.bayes = -BETA[100:N,1]/BETA[100:N,2]
CI.bayes = quantile(LD.50.bayes, c(.025, .975))
post.mean =round(mean(LD.50.bayes),3)