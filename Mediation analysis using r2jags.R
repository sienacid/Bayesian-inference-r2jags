### Mediation - Adjusted - Overcontrol-NSSI (total) #####
mypackages <- c("tidyverse","rjags","posterior","bayesplot","R2jags","MCMCvis",
                "mcmcplots","standardize","jagsUI","rstanarm","performance",
                "loo","rstantools","CalvinBayes","bayesrules","ggplot2",
                "ggstance","ggformula","superdiag",
                "dplyr","plyr","skimr","corrplot","Hmisc","xlsx","openxlsx",
                "naniar","knitr","kableExtra","finalfit")
for (p in mypackages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

fulldata<-read.csv('fulldata_recode.csv')

fulldata<-fulldata%>%
  dplyr::select(-id_nspn,-nssi_cat_type,-ethnic_final)

#scale data
fulldata[,c(4:6,8:9,39)]<-scale(fulldata[,c(4:6,8:9,39)],center=TRUE, scale=TRUE)
fulldata[,c(7,10:38,40:45)]<-0.5*scale(fulldata[,c(7,10:38,40:45)],center=TRUE, scale=TRUE)

fulldata$nssi_binary_t2<-as.numeric(fulldata$nssi_binary_t2)

modfile <- tempfile()
writeLines("
model{
# Prior distributions
intm ~ dnorm(0,0.16) # prior for the intercept M
inty ~ dlogis(0, 0.01) # prior for the intercept Y

a ~ dnorm(0,0.16)  # prior for for a
b ~ dnorm(0,0.16) # prior for b
c ~ dnorm(0,0.16) # prior for c

beta_age ~ dnorm(0,0.16)
beta_loi ~ dnorm(0,0.16)
beta_fad ~ dnorm(0,0.16)

ya ~ dnorm(0,0.16)
yb ~ dnorm(0,0.16)
yc ~ dnorm(0,0.16)

al ~ dnorm(0,0.16)
bl ~ dnorm(0,0.16)
cl ~ dnorm(0,0.16)
dl ~ dnorm(0,0.16)

ab ~ dnorm(0,0.16)
bb ~ dnorm(0,0.16)
cb ~ dnorm(0,0.16)

af ~ dnorm(0,0.16)
bf ~ dnorm(0,0.16)

lprec  ~ dgamma(0.01,0.01)
bprec  ~ dgamma(0.01,0.01)
fprec ~ dgamma(0.01,0.01)


tau.em ~ dgamma(0.01, 0.01); # prior for the error precision M
var_m <- 1/tau.em # residual variance of M
tau.ey ~ dgamma(0.01, 0.01); # prior for the error precision Y
var_y <- 1/tau.ey # residual variance of Y

# Potential outcomes estimators
indirect_eff <- a*b
total<- c + (a*b)
  
  #Derived parameters
  fit <- sum(res[])
  fit.new <- sum(res.new[])


for(i in 1:n){

    # posterior predictive
  res[i]<-nssi_binary_t2[i] - logit(y.prime[i])
  
  nssi_binary_t2_rep[i] ~ dbern(y.prime[i])
  
  res.new[i] <- nssi_binary_t2_rep[i] - logit(y.prime[i])
  
  #predicted value
  m.prime[i] <- intm + a*control_total[i] +beta_age*agehqpdone[i]+
  beta_loi*loitot_3cat[i]+beta_fad*fadtotal[i]
  
  logit(y.prime[i]) <- inty + c*control_total[i] +  b*leq_total_45yes[i] + ya*agehqpdone[i]+
  yb*loitot_3cat[i]+yc*fadtotal[i]
  
  # conditional distributions of covariates with missing values
  control_total[i] ~ dnorm(0,0.16)

  loitot_3cat[i] ~ dnorm (loimu[i], lprec)
  loimu[i]<- al+bl*agehqpdone[i]+
  cl*bistotal[i]+dl*fadtotal[i]
  
  bistotal[i] ~ dnorm (bismu[i], bprec)
  bismu[i]<- ab+bb*agehqpdone[i]+cb*fadtotal[i]
  
  fadtotal[i] ~ dnorm (fadmu[i], fprec)
  fadmu[i]<- af+bf*agehqpdone[i]
  
  # conditional distributions of m and y
  leq_total_45yes[i] ~ dnorm(m.prime[i], tau.em)
  nssi_binary_t2[i] ~ dbern(y.prime[i])

} }", con=modfile)

data<-list(nssi_binary_t2=fulldata$nssi_binary_t2,
           control_total=fulldata$control_total,
           agehqpdone=fulldata$agehqpdone,
           leq_total_45yes=fulldata$leq_total_45yes,
           loitot_3cat=fulldata$loitot_3cat,
           fadtotal=fulldata$fadtotal,
           n = nrow(fulldata))

inits <- function(){
  list("a" = c(0.5), "b" = c(0.5), "c" = c(0.5),
       "beta_age" = c(0.5),"beta_loi" = c(0.5), 
       "beta_fad" = c(0.5))  ## Note that we're giving inits to theta, not theta1
}

parameters <-c("a","b","c","indirect_eff","total","fit","fit.new")

#Run analysis
control_total_adj <- jags(data = data, inits=inits,parameters.to.save=parameters, n.chains = 2, n.iter = 15000, 
                          n.burnin = 1000, model.file = modfile)

control_total_adj

#Posterior predictive check plot
pp.check(control_total_adj,observed='fit',simulated='fit.new',
         main="Posterior Predictive Check for Control(total) - NSSI(total)",cex.main=0.9,cex.lab=1.0)




