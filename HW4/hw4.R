set.seed(1000)
setwd("C:/Users/dipesh/Desktop/GoogleDrive/GradClasses/SemIII/STA841/HW4")
#install.packages("aod")
#install.packages("plyr")
library(aod)
library(dplyr)
library(Matrix)
library(tidyr)


#Problem 1
cancermap = read.table("cancermap.dat")
cancermap=as.matrix(cancermap)
rotate <- function(x) t(apply(x, 2, rev))

#1.1
image(rotate(cancermap))
legend("bottomright", c("","low", "high"), col = c("white","red","orange"), pch=c(15,15,15))



#1.2
##just removed the data without all four neighbors
accept = matrix(0,nrow(cancermap),ncol(cancermap))
for (i in 2:(nrow(cancermap)-1)){
  for (j in 2:(ncol(cancermap)-1)){
    if (max(cancermap[i-1,j],cancermap[i+1,j],cancermap[i,j-1],cancermap[i,j+1])<2){
      #if neighboring cell not in map remove the data
      accept[i,j] = 1
    }
  }
}
cancermap1 = matrix(0,nrow(cancermap),ncol(cancermap))
cancermap1[which(accept==1)] = cancermap[which(accept==1)+1]+cancermap[which(accept==1)-1]+
  cancermap[which(accept==1)+ncol(cancermap)]+cancermap[which(accept==1)-ncol(cancermap)]
y=cancermap[which(accept==1)]
y1 = cancermap1[which(accept==1)]


m1=glm(cbind(y,1-y)~y1, family=binomial(link="logit"))
summary(m1)
devResid = round(m1$deviance,2)
df = m1$df.residual
beta = m1$coefficient[2]
odds=exp(beta)-1

#one more neighbor being in high rate increases the odds by exp(beta) of being in high rate. So, with
# all neighbors in high rate implies really high odds ratio

#1.3


test <- function(data){  
  indata = which(data==1|data==0)
  x = matrix(2, nrow(data), ncol(data))
  x[indata] = sample(data[indata])
  accept = matrix(0,nrow(x),ncol(x))
  data1_1 = matrix(0,nrow(x),ncol(x))
  for (i in 2:(nrow(x)-1)){
    for (j in 2:(ncol(x)-1)){
      if (max(x[i-1,j],x[i+1,j],x[i,j-1],x[i,j+1])<2){#if neighboring cell not in map ignore the data
        accept[i,j] = 1
      }
    }
  }
  data1 = matrix(0,nrow(data),ncol(data))
  data1[which(accept==1)] = x[which(accept==1)+1]+x[which(accept==1)-1]+
    x[which(accept==1)+ncol(x)]+x[which(accept==1)-ncol(x)]
  y=data[which(accept==1)]
  y1 = data1[which(accept==1)]
  
  fit=glm(cbind(y,1-y)~y1, family=binomial(link="logit"))
  fit$coefficients
}

BETA <- replicate(5000, test(cancermap))

hist(BETA[2,], xlab="beta", main="")
CI = quantile(BETA[2,], c(.005,.995))
CI

#null is false

#1.4

cancermap2 = matrix(0,nrow(cancermap),ncol(cancermap))
cancermap2[which(accept==1)] = cancermap[which(accept==1)-ncol(cancermap)]+
  cancermap[which(accept==1)+ncol(cancermap)]
y2 = cancermap2[which(accept==1)]

m2=glm(cbind(y,1-y)~y1+y2, family=binomial(link="logit"))
summary(m2)


#Problem 2
ses = rep(c("A","B","C","D", "E","F"),4)
status = rep(c("well","mild","moderate","impaired"), each=6)
count = c(64,57,57,72,36,21,94,94,105,141,97,71,58,54,65,77,54,54,46,40,60,94,78,71)

well = c(64,57,57,72,36,21)
mild = c(94,94,105,141,97,71)
moderate = c(58,54,65,77,54,54)
impaired = c(46,40,60,94,78,71)

data2 = data.frame(ses, status, count)
data2$status=factor(data2$status, ordered=T)
#2.1
# logit P(Y<=j|x) = alpha_j + socioeconomic effect


fit2 = polr(status~ses, weights = count, data = data2)
summary(fit2)
beta2= fit2$coefficients
intercept2=fit2$zeta


#Problem 4

data4 = read.table("fiji.dat", header =T)
data4$marriage = factor(data4$marriage, ordered=F)
data4$edu= factor(data4$edu, ordered=F)
data4$abode = factor(data4$abode)
data4$count = round(data4$average*data4$tot,0)
#4.1


fit4 = glm(count~marriage+edu+abode, family=poisson(link=log), 
           offset = log(tot), subset=(tot>0), data = data4)
summary(fit4)
s2= sum(fit4$resid^2/fit4$fitted)/fit4$df.resid
summary(fit4, dispersion=s2)



#4.3
varcovar=vcov(fit4)
cov= varcovar[c("(Intercept)","edu3", "marriage3"), 
         c("(Intercept)","edu3", "marriage3")]
var = cov[1,1]+cov[2,2]+cov[3,3]+2*cov[2,1]+ 2*cov[3,1]+2*cov[3,2]
mu = fit4$coefficients["(Intercept)"] + fit4$coefficients["edu3"]+
      fit4$coefficients["marriage3"]

samples = exp(rnorm(5000, mu, sqrt(var)))
quantile(samples, c(.025,.975))


#4.4
cov2 = varcovar[c("(Intercept)","abode2","edu4", "marriage6"), 
              c("(Intercept)","abode2","edu4", "marriage6")]
var2 = cov2[1,1]+cov2[2,2]+cov2[3,3]+cov2[4,4]+2*cov2[2,1]+ 2*cov2[3,1]+2*cov2[3,2]+2*cov2[4,1]+
      2*cov2[4,2]+2*cov2[4,3]
mu2 = fit4$coefficients["(Intercept)"] + fit4$coefficients["abode2"]+
  fit4$coefficients["edu4"] +fit4$coefficients["marriage6"]

samples2 = exp(rnorm(5000, mu2, sqrt(var2)))
quantile(samples2, c(.025,.975))




# log linear with marriage and education ordered factors