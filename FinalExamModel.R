#refresh 
rm(list = ls())

#get data
wd = 'C:/Users/debri/Documents/ST 495 Applied Bayesian Statistics/Final'
setwd(wd)
data = load('USHCNprcpSetup.RData')

#create informative names
siteNames = c()
for(i in 1:1218){
  siteName = paste("site", i, sep = '_')
  siteNames = c(siteNames, siteName)
}

#add years and days to make more informative
year = sort(rep(1900:2014, 365))
PRCP = cbind(year, PRCP)
day = rep(1:365,(dim(PRCP)[1]/365))
PRCP = cbind(day, PRCP)

#implement informative names and subset to years >=1950
colnames(PRCP)[3:dim(PRCP)[2]] = siteNames
PRCP.redox = subset(PRCP, year > 1949)
#additional removal of all NA columns
PRCP.redox = PRCP.redox[, colSums(is.na(PRCP.redox)) != nrow(PRCP.redox)]

#data frame that will be used to create new data frame to solve
PRCP.redox = as.data.frame(PRCP.redox)
print(dim(PRCP.redox)[2] == (length(siteNames) + 2)) 

#new data frame
cols = length(siteNames)
rows = 2014 - 1950 + 1
Y = as.data.frame(matrix(nrow = cols, ncol = rows))
#Y column names
Ycol = c()
for(i in 1950:2014){
  y = paste('Year', i, sep = '_')
  Ycol = c(Ycol, y)
}
colnames(Y) = Ycol

#populate data frame
for(i in 1950:2014){
  for(j in 3:dim(PRCP.redox)[2]){
    bin = which(PRCP.redox$year == i)
    colindex = j - 2
    rowindex = i - 1949
    if(sum(is.na(PRCP.redox[bin,j])) > 50){
      Y[colindex, rowindex] = NA  
    } else {
      Y[colindex, rowindex] = max(PRCP.redox[bin,j], na.rm = T)
    }
  }
}

#check Y
max(Y, na.rm=T)
Y = Y[rowSums(is.na(Y)) <= 10, ]
##any zero value absolutely makes zero sense anywhere in the country
Y[Y == 0] = NA
#only two zero values anyhow
dim(Y)
sum(is.na(Y))

#wrapper function for X
year.scale = function(t = t){
  X = (t - 1950)/10
}
#so we make X
X = year.scale(1950:2014)


#time for some Bayes 
nt = length(X)
ns = dim(Y)[1]
library(rjags)
model.1 = "model{
  
  # Likelihood
  
  for(i in 1:ns){for(t in 1:nt){
      Y[i,t] ~ dgamma(a[i,t],b[i,t])
      a[i,t] <- pow(mu[i,t],2)/v[i,t]
      b[i,t] <- mu[i,t]/v[i,t]
      log(v[i,t]) <- beta[i,3] 
      log(mu[i,t]) <- beta[i,1] + X[t] * beta[i,2] 
  }}
  

  #Priors for betas
  for(i in 1:ns){
      beta[i,1] ~ dnorm(0, 0.1)
      beta[i,2] ~ dnorm(0, 0.1)
      beta[i,3] ~ dgamma(0.1, 0.1)
  }
  
}"
#data
dat = list(Y = Y, X = X, ns = ns, nt = nt)

#model 1 Super simple
model1 = jags.model(textConnection(model.1), data = dat, n.chains = 3)
update(model1, 2000)
codasamp1 = coda.samples(model1, 500, variable.names = c('beta'))
dicsamp1 = dic.samples(model1, 1000)

#plot traces
par(mar = rep(2, 4))
#these traces look good

#just check it
gelman.plot(codasamp1)
autocorr.plot(codasamp1)
effectiveSize(codasamp1)
plot(codasamp1)
summary(codasamp1)
dicsamp1



#fit another model Hyper Parameters
model.2 = "model{
  
# Likelihood

for(i in 1:ns){for(t in 1:nt){
Y[i,t] ~ dgamma(a[i,t],b[i,t])
a[i,t] <- pow(mu[i,t],2)/v[i,t]
b[i,t] <- mu[i,t]/v[i,t]
log(v[i,t]) <- beta[i,3] 
log(mu[i,t]) <- beta[i,1] + X[t] * beta[i,2] 
}}


#Priors for betas
for(i in 1:ns){
beta[i,1] ~ dnorm(0, invar.b1)
beta[i,2] ~ dnorm(0, invar.b2)
beta[i,3] ~ dgamma(0.1, 0.1)
}

#priors for inverse variance of Beta[,1] and Beta[,2]
invar.b1 ~ dgamma(0.1, 0.1)
invar.b2 ~ dgamma(0.1, 0.1)

}"

model2 = jags.model(textConnection(model.2), data = dat, n.chains = 3)
update(model2, 2000)
codasamp2 = coda.samples(model2, 500, variable.names = c('beta'))
dicsamp2 = dic.samples(model2, 1000)

#plot traces
par(mar = rep(2, 4))
#these traces look good

#just check it
gelman.plot(codasamp2)[,4]
autocorr.plot(codasamp2)
effectiveSize(codasamp2)
plot(codasamp2)[,4]
summary(codasamp2)
dicsamp2

#fit another model Bayesian LASSO
model.3 = "model{
  
# Likelihood

for(i in 1:ns){for(t in 1:nt){
Y[i,t] ~ dgamma(a[i,t],b[i,t])
a[i,t] <- pow(mu[i,t],2)/v[i,t]
b[i,t] <- mu[i,t]/v[i,t]
log(v[i,t]) <- beta[i,3] 
log(mu[i,t]) <- beta[i,1] + X[t] * beta[i,2] 
}}


#Priors for betas
for(i in 1:ns){
beta[i,1] ~ ddexp(0, invar.b1)
beta[i,2] ~ ddexp(0, invar.b2)
beta[i,3] ~ dgamma(0.1, 0.1)
}

#priors for inverse variance of Beta[,1] and Beta[,2]
invar.b1 ~ dgamma(0.1, 0.1)
invar.b2 ~ dgamma(0.1, 0.1)

}"

model3 = jags.model(textConnection(model.3), data = dat, n.chains = 3)
update(model3, 2000)
codasamp3 = coda.samples(model3, 500, variable.names = c('beta'))
dicsamp3 = dic.samples(model3, 1000)

#plot traces
par(mar = rep(2, 4))
#these traces look good

#just check it
gelman.plot(codasamp3[,c(randomBetas)])
autocorr.plot(codasamp3[,c(randomBetas)])
effectiveSize(codasamp3[,c(randomBetas)])
plot(codasamp3)
summary(codasamp3[,c(randomBetas)])
dicsamp3


#fit another model Cauchy
model.4 = "model{

# Likelihood

for(i in 1:ns){for(t in 1:nt){
Y[i,t] ~ dgamma(a[i,t],b[i,t])
a[i,t] <- pow(mu[i,t],2)/v[i,t]
b[i,t] <- mu[i,t]/v[i,t]
log(v[i,t]) <- beta[i,3] 
log(mu[i,t]) <- beta[i,1] + X[t] * beta[i,2] 
}}


#Priors for betas
for(i in 1:ns){
beta[i,1] ~ dt(0, invar.b1,1)
beta[i,2] ~ dt(0, invar.b2,1)
beta[i,3] ~ dgamma(0.1, 0.1)
}

#priors for inverse variance of Beta[,1] and Beta[,2]
invar.b1 ~ dgamma(0.1, 0.1)
invar.b2 ~ dgamma(0.1, 0.1)

}"

model4 = jags.model(textConnection(model.4), data = dat, n.chains = 3)
update(model4, 2000)
codasamp4 = coda.samples(model4, 500, variable.names = c('beta'))
dicsamp4 = dic.samples(model4, 1000)

#plot traces
par(mar = rep(2, 4))
#these traces look good

#just check it
gelman.plot(codasamp4[,1])
autocorr.plot(codasamp4[,1])
effectiveSize(codasamp4)
plot(codasamp4)
summary(codasamp4)
dicsamp4


randomBetas = sample(1:ns, 6) 
beta = c(571,453,565,862,389)
B1 = as.vector(summary(codasamp4)[[1]][beta,1])
B2 = c(-1.049e-03, 4.460e-03, -3.417e-03, 1.536e-02, 1.243e-02)
plot(x = NULL, y = NULL, ylim =c(0,max(Y[beta,],na.rm=T)), xlim = c(0,max(X)), xlab = 'Year', ylab = 'Extreme Rainfall', main = 'Extreme Rainfall Event by Year')
for(i in 1:5){
  abline(B1[i], B2[i], col = i)
}
for(j in 1:5){
  for(i in 1:length(X)){
    points(X[i], Y[beta[j],i])
  }
}  
maxprcp = c()
for(i in 3:dim(PRCP)[2]){
  max = max(PRCP[,i], na.rm = T)
  maxprcp = c(maxprcp, max)
}

hist(maxprcp, main = "Histogram of Maximum Daily Rainfall at Each Site between 1950-1964", xlab = "Rainfall Hundredth of an Inch")

