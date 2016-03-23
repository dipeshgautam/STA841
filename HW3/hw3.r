setwd("C:/Users/dipesh/Desktop/GoogleDrive/GradClasses/SemIII/STA841/HW3")
#install.packages("aod")
#install.packages("plyr")
library(aod)
library(dplyr)
#Problem 2
#2.1
dose = seq(0,5)
dead = c(0,2,3,5,7,10)
total = c(7,9,8,7,9,11)
alive = total-dead


data = data.frame(dose=dose, dead=dead, alive=alive, total=total)

#full model
fit.full = glm(cbind(dead,alive)~dose, family=binomial(link=probit),
                 data=data)

#restricted model

fit.sub=glm(cbind(dead,alive)~I(-5+dose)-1, family=binomial(link=probit),
    data=data)

summary(fit.sub)

LR_5=2*(logLik(fit.full)-logLik(fit.sub))
##if null is correct, chi-sq(1)

test=anova(fit.sub,fit.full, test="Chisq")
test
LR_5_1 = test$Dev[2]
p_value = test$Pr[2]


#2.2

N=100
profile.lik = rep(NA,N)
ld50=seq(1,7, length=N)
for (i in 1:N){
  fit.sub=glm(cbind(dead,alive)~I(-ld50[i]+dose)-1, family=binomial(link=probit),
              data=data)
  profile.lik[i] = logLik(fit.sub)
}
hor.line = as.numeric(-qchisq(0.9,1)/2+logLik(fit.full))
plot(seq(1,7,length=100),profile.lik, type="l", col="red", main="profile log-likelihood", xlab="log2LD50", ylab="profile ll")
abline(h=hor.line, lty = 2, col ="blue")

profLik = data.frame("LD50"=ld50,"profLik"=profile.lik)
profLik[,"diff"]=abs(profLik[,"profLik"]-hor.line)
profLik1 = arrange(profLik,diff)
upper = 2^max(profLik1[c(1,2),"LD50"])
lower = 2^min(profLik1[c(1,2),"LD50"])




#3
x <- scan("cyl.txt", what="", sep="\n")
y <- strsplit(x, "[[:space:]]+")
y=as.numeric(unlist(y))
n = rep(6,75)
height = rep(c(.5,.75,1), each=25)


#3.1
m1=glm(cbind(y,n-y)~height, family=binomial(link="logit"))
summary(m1)

#1/(1+exp(-(m1$coefficients[1]+m1$coefficients[2]* .5)))
est_height_logit = (log(.5)-m1$coefficients[1])/m1$coefficients[2]
CI_logit =(log(.5/(.5))-confint(m1)[1,])/confint(m1)[2,]
fitted_logit=fitted.values(m1)
predict_logit = predict(m1, newdata=data.frame(seq(.5,1,.5/74)), type="response")



#3.2
m2=glm(cbind(y,n-y)~height, family=binomial(link="probit"))
summary(m2)
est_height_probit=(qnorm(.3)-m2$coefficients[1])/m2$coefficients[2]
CI_probit =(qnorm(.3)-confint(m2)[1,])/confint(m2)[2,]
fitted_probit=fitted.values(m2)
plot(height, y/n, pch=16)
lines(height, fitted_logit, col = "red", lty=1,lwd = 2)
lines(height, fitted_probit, col = "blue", lty=2,lwd = 2)
legend("topleft", c("data","logit", "probit"), col = c("black","red","blue"), pch=c(16,NA,NA),lty= c(0,1,2))



#3.3
set.seed(1000)
N = 1000
diff.boot = double(N)
y.boot = double(75)

for (i in 1:N){
  for (j in 1:length(y)){
    y.boot[j] = sum(rbinom(n[j], 1, y[j]/n[j]))
  }
  logit.boot = glm(cbind(y.boot,n-y.boot)~height, family=binomial(link="probit"))
  est.logit = (log(.5)-logit.boot$coefficients[1])/logit.boot$coefficients[2]
  probit.boot = glm(cbind(y.boot,n-y.boot)~height, family=binomial(link="logit"))
  est.probit=(qnorm(.3)-probit.boot$coefficients[1])/probit.boot$coefficients[2]
  diff = est.logit-est.probit
  diff.boot[i] = diff  
}

CI.diff= quantile(diff.boot, c(.025,.975))




#problem 4
data4=read.table("kredit.asc", header=T)
data4=rename(data4, credit=kredit, currentBalance=laufkont, duration=laufzeit,
                      paymentPrevious=moral, use=verw,
                      maritalStatusGender=famges)

data4=data4[,c("credit", "currentBalance", "duration", "paymentPrevious", "use", 
               "maritalStatusGender")]
data4$currentBalance = as.factor(data4$currentBalance)
data4$paymentPrevious = as.factor(data4$paymentPrevious)
data4$use = as.factor(data4$use)
data4$maritalStatusGender = as.factor(data4$maritalStatusGender)

m1=glm(cbind(credit,1-credit)~currentBalance+duration+paymentPrevious+
         use+maritalStatusGender,
       family=binomial(link="logit"), data=data4)
summary(m1)


m2=glm(cbind(credit,1-credit)~currentBalance+duration+paymentPrevious+use+maritalStatusGender,
       family=binomial(link="probit"), data=data4)
summary(m2)
anova(m1,m2, test="Chisq")


quasibin(cbind(credit,1-credit)~currentBalance + duration + paymentPrevious + 
           use + maritalStatusGender,data = data4, link="logit")


m3=glm(cbind(credit,1-credit)~currentBalance+duration + duration:currentBalance + 
         duration:paymentPrevious + paymentPrevious + use+ duration:use+ maritalStatusGender,
       family=binomial(link="logit"), data=data4)
summary(m3)

test1 = anova(m1,m3, test="Chisq")
print(test1)




#problem 5

clutch = rep(seq(1,7), 3)
hatched =c(rep(0,7),3,0,8,10,25,7,10,0,0,6,9,23,5,4)
total = rep(c(6,13,10,16,32,7,21),3)
treatment = rep(c(1,2,3),each=7)

data5 = data.frame(clutch = rep(seq(1,7), 3),
                   hatched =c(rep(0,7),3,0,8,10,25,7,10,0,0,6,9,23,5,4),
                   total = rep(c(6,13,10,16,32,7,21),3),
                   treatment = as.factor(rep(c(1,2,3),each=7)))


#5.1
fit.5a = glm(cbind(hatched,total-hatched)~treatment-1,
             family=binomial(link="logit"),  data =data5)
summary(fit.5a)
resid.dev = fit.5a$deviance
df = fit.5a$df.residual


#5.2
fit.5b = glm(cbind(hatched,total-hatched)~treatment-1,
             family=quasibinomial(link="logit"),  data =data5)
summary(fit.5b)
quasibin(cbind(hatched,total-hatched)~treatment-1,data=data5,link="logit")


#5.3
betabin(cbind(hatched,total-hatched)~treatment-1, random=~treatment, data =data5)




