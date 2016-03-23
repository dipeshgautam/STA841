library(vcd)

##Problem 1 (11.12) Agresti

count = c(24, 8,13,8,13,11, 10,9,64)
ebert = c(1,2,3,1,2,3,1,2,3)
siskel = c(1,1,1,2,2,2,3,3,3)

movies = data.frame(count=count, ebert=ebert, siskel=siskel)
movies$sym = factor(paste(pmin(movies$ebert,movies$siskel),pmax(movies$ebert,movies$siskel),
                    sep=","))

#1.a
#symmetry
options(contrasts = c("contr.treatment", "contr.ploy"))
fit.sym = glm(count~sym, family = poisson(log), data = movies)
summary(fit.sym)


#quasi-symmetry
movies$ebert = factor(movies$ebert)
movies$siskel = factor(movies$siskel)


options(contrasts = c("contr.treatment", "contr.ploy"))
fit.qsym = glm(count~sym + ebert, family = poisson(log), data = movies)
summary(fit.qsym)

#quasi-independence
movies$D1 = as.numeric(movies$sym =="1,1")
movies$D2 = as.numeric(movies$sym =="2,2")
movies$D3 = as.numeric(movies$sym =="3,3")

options(contrasts = c("contr.treatment", "contr.ploy"))
fit.qind = glm(count~siskel+ebert +D1+D2+D3, family = poisson(log), data = movies)
summary(fit.qind)


#1.b
anova(fit.sym, fit.qsym)
gsq = fit.sym$deviance-fit.qsym$deviance
df = fit.sym$df.residual-fit.qsym$df.residual

#gsq on df so marginal homogeneity is plausible
#1.c



data.tab = xtabs(count~siskel+ebert, data = movies)
kappa = Kappa(data.tab, weights = "Equal-Spacing")
unweighted = kappa$Unweighted[1]
unweighted.se = kappa$Unweighted[2]

weighted = kappa$Weighted[1]
weighted.se = kappa$Weighted[2]



#Problem 2 Agresti 


y = c(714,33,320,284,730,425,813,276,
      498,68,1072,325,221,17,142,188)

cited = factor(rep(c("biometrika", "communstat", "jasa",
            "jrss-b"),4))
citing = factor(rep(c("biometrika", "communstat", "jasa",
               "jrss-b"),each=4))


journal = data.frame(y=y, cited=cited, citing = citing)

journal$sym = factor(paste(pmin(as.numeric(journal$cited),as.numeric(journal$citing)),
                                      pmax(as.numeric(journal$cited),as.numeric(journal$citing)),
                           sep=","))

options(contrasts = c("contr.treatment", "contr.ploy"))
fit2.bt = glm(y~sym + cited, family = poisson(log), data = journal)
summary(fit2.bt)



# Problem 3 (Problem left from hw4)
count = read.delim("pick6.dat", sep="", header=F, fill=T)

count = as.vector(t(count))
count = count[!is.na(count)]
tot = rep(312, 54)
pick6 = data.frame(count=count, tot=tot)
pick6$row = c(rep(0, 9), rep(c(10, 20, 30, 40), each=10), rep(50, 5) )
pick6$column = c(seq(1,9), rep(seq(0,9), 4), seq(0,4))
pick6$geq45 =  as.numeric((pick6$row+pick6$column)<=45)
pick6$row = factor(pick6$row)
pick6$column = factor(pick6$column)

fit3.1 = glm(count~, family=poisson(link=log), 
             offset = log(tot), subset=(tot>0), data = pick6)

summary(fit3.1)


fit3.2 = glm(count~1+column, family=poisson(link=log), 
             offset = log(tot), subset=(tot>0), data = pick6)

summary(fit3.2)


fit3.3 = glm(count~1+row, family=poisson(link=log), 
             offset = log(tot), subset=(tot>0), data = pick6)

summary(fit3.3)



fit3.4 = glm(count~1+geq45, family=poisson(link=log), 
             offset = log(tot), subset=(tot>0), data = pick6)

summary(fit3.4)

pick6$tot1 = c(rep(312, 44), rep(168, 10))

fit3.1a = glm(count~1, family=poisson(link=log), 
             offset = log(tot1), subset=(tot>0), data = pick6)

summary(fit3.1a)

fit3.2a = glm(count~1+column, family=poisson(link=log), 
             offset = log(tot1), subset=(tot>0), data = pick6)

summary(fit3.2a)



fit3.3a = glm(count~row, family=poisson(link=log), 
             offset = log(tot1), subset=(tot>0), data = pick6)

summary(fit3.3a)