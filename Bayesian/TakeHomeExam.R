setwd("C:/Users/debri/Downloads")
data = read.csv("exam2_data.csv")
library(rjags)
Y = data$Y
theta = data$theta
n = length(Y)

model = "model{

# Likelihood

for(i in 1:n){
  Y[i] ~ dpois(lambda[i])
}

for(i in 1:n){
  lambda[i] = alpha + inprod(phi[i,],beta[])
  for(j in 1:p){
    phi[i,j] = exp(-0.5*(theta[i]-gamma[j])^2/(tau[j]^2))
  }
}
#Prior for alpha
alpha ~ dgamma(1, 1)

#Prior for Betas
for(j in 1:p){
  beta[j] ~ dgamma(1, 1)
}

#Prior for Gamma and Tau
for(j in 1:p){
  gamma[j] ~ dunif(0,90)
  tau[j] ~ dgamma(1,1) 
}
Gamma[1:p] = sort(gamma)

}"

model0 <- jags.model(textConnection(model), n.chains=2,
                     data = list(Y=Y, theta=theta, n=n, p = 5))
update(model0, 1000) 
dic1   <- dic.samples(model0, 
                      variable.names=c("Gamma"), 
                      n.iter=1000, progress.bar="text")
samps1 <- coda.samples(model0, 
                       variable.names=c("Gamma"), 
                       n.iter=1000, progress.bar="text")
par(mar = rep(2, 4))
autocorr.plot(samps1)
effectiveSize(samps1)
gelman.plot(samps1)
plot(samps1)
summary(samps1)
dic1


model = "model{

# Likelihood

for(i in 1:n){
Y[i] ~ dpois(lambda[i])
}

for(i in 1:n){
lambda[i] = alpha + inprod(phi[i,],beta[])
for(j in 1:p){
phi[i,j] = exp(-0.5*(theta[i]-gamma[j])^2/(tau[j]^2))
}
}
#Prior for alpha
alpha ~ dgamma(1, 1)

#Prior for Betas
for(j in 1:p){
beta[j] ~ dgamma(1, 1)
}

#Prior for Gamma and Tau
for(j in 1:p){
gamma[j] ~ dunif(0,90)
tau[j] ~ dgamma(1,1) 
}
Gamma[1:p] = sort(gamma)

}"

model1 <- jags.model(textConnection(model), n.chains=2,
                    data = list(Y=Y, theta=theta, n=n, p = 6))

update(model1, 5000) 
dic2   <- dic.samples(model1, 
                      variable.names=c("Gamma"), 
                      n.iter=2000, progress.bar="text")
samps2 <- coda.samples(model1, 
                       variable.names=c("Gamma"), 
                       n.iter=5000, progress.bar="text")
par(mar = rep(2, 4))
autocorr.plot(samps2)
effectiveSize(samps2)
gelman.plot(samps2)
plot(samps2)
summary(samps2)
dic2



model = "model{

# Likelihood

for(i in 1:n){
Y[i] ~ dpois(lambda[i])
}

for(i in 1:n){
lambda[i] = alpha + inprod(phi[i,],beta[])
for(j in 1:p){
phi[i,j] = exp(-0.5*(theta[i]-gamma[j])^2/(tau[j]^2))
}
}
#Prior for alpha
alpha ~ dgamma(1, 1)

#Prior for Betas
for(j in 1:p){
beta[j] ~ dgamma(1, 1)
}

#Prior for Gamma and Tau
for(j in 1:p){
gamma[j] ~ dunif(0,90)
tau[j] ~ dgamma(1,1) 
}
Gamma[1:p] = sort(gamma)

}"

model2 <- jags.model(textConnection(model), n.chains=2,
                     data = list(Y=Y, theta=theta, n=n, p = 7))

update(model2, 20000) 
dic3   <- dic.samples(model2, 
                      variable.names=c("Gamma"), 
                      n.iter=2000, progress.bar="text")
samps3 <- coda.samples(model2, 
                       variable.names=c("Gamma"), 
                       n.iter=2000, progress.bar="text")
par(mar = rep(2, 4))
autocorr.plot(samps3)
effectiveSize(samps3)
gelman.plot(samps3)
plot(samps3)
summary(samps3)
dic3

plot(x = NULL, y = NULL, xlab="Angle Theta", ylab="Intensity", main="Intensity at Angle Theta", xlim=c(0,90),ylim=c(0,30))
points(x=theta, y=Y)

samps = samps2[[1]]
gamma.mn = colMeans(samps)
plot(x = theta, y = Y, xlab="Angle Theta", ylab= "Intensity", main="Intensity at Angle Theta",type="l")
for(i in 1:6){
  abline(v=gamma.mn[i],col=3,lwd=2)
}
legend("topright", c("Observed Data", "Gamma Means"), col=c(1,3),lty=1)
