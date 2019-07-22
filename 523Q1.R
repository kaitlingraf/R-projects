library(faraway)
#data(orings)
#attach(orings)
#head(orings)

#beetles<-read.table("Beetles2.dat", header=TRUE)
#attach(beetles)
#head(beetles)
#the data
logdose<-c(1.691, 1.724, 1.755, 1.784, 1.811, 1.837, 1.861, 1.884)
n<-c(59, 60, 62, 56, 63, 59, 62, 60)
dead<-c(6, 13, 18, 28, 52, 53, 61, 60)
#beetles<-matrix(append(dead,n-dead), ncol=2)


#plot(damage/6~temp,xlim=c(25,85),ylim=c(0,1),xlab="Temperature",ylab="Prob of damage")
plot(dead~logdose, xlab="Logdose", ylab="Dead")
# fitting a naive linear model


# Now fit a binomial GLM with the canonical (logit) link
#beetles<-matrix(append(dead,n-dead), ncol=2)

logitmod<-glm(cbind(dead,n-dead)~logdose, family=binomial)
#logitmod <- glm(cbind(damage,6-damage)~temp,family=binomial)

#names(logitmod)
summary(logitmod)
#coef(logitmod)

x <- seq(from=25,to=85,by=1)
lines(x,ilogit(coef(logitmod)[1]+coef(logitmod)[2]*x),col="red")

# Next, explore the same GLM model with the probit link

probitmod <- glm(cbind(dead,n-dead)~logdose,family=binomial(link=probit))
summary(probitmod)
lines(x,pnorm(coef(probitmod)[1]+coef(probitmod)[2]*x),col="darkgreen")

# Finally, explore the complementary log-log link

loglogmod <- glm(cbind(dead,n-dead)~logdose,family=binomial(link=cloglog))
summary(loglogmod)
lines(x,1-exp(-exp(coef(loglogmod)[1]+coef(loglogmod)[2]*x)),col="blue")


# model selection with AIC
logitmod$aic
probitmod$aic
loglogmod$aic




