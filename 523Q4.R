library(faraway)
beetles <- read.table("Beetles2.dat", header=TRUE)
attach(beetles)

# plotting the data
plot(dead/n~logdose,xlab="Logdose",ylab="Proportion Dead")

#binomial GLM with the canonical (logit) link
logitmod <- glm(cbind(dead,n-dead)~logdose,family=binomial)
summary(logitmod)

x <- seq(from=1,to=3,by=.05)
lines(x,ilogit(coef(logitmod)[1]+coef(logitmod)[2]*x), type='l', col="red")

#probit link
probitmod <- glm(cbind(dead,n-dead)~logdose,family=binomial(link=probit))
summary(probitmod)
lines(logdose,pnorm(coef(probitmod)[1]+coef(probitmod)[2]*logdose),col="darkgreen")

#log-log link
loglogmod <- glm(cbind(dead,n-dead)~logdose,family=binomial(link=cloglog))
summary(loglogmod)
lines(x,1-exp(-exp(coef(loglogmod)[1]+coef(loglogmod)[2]*x)),col="blue")

# model selection with AIC
logitmod$aic
probitmod$aic
loglogmod$aic


#increase in odds when increasing log Dosage by 0.01
oddsRatio0.1 <- exp(coef(logitmod)[2]*0.1)
oddsRatio0.1
#increase in odds when increasing log Dosage by 0.05
oddsRatio0.05 <- exp(coef(logitmod)[2]*0.05)
oddsRatio0.05